use std::collections::HashMap;
use std::rc::Rc;

use super::value::{HeapEntry, HeapRef, HeapValue, Upvalue, Value};
use super::vm::State;

const DEBUG_GC_TRACING: bool = false;

const INITIAL_HEAP_CAPACITY: usize = 16 * 1024;
const HEAP_GROW_FACTOR: usize = 2;

pub fn reserve_new_heap() -> Vec<HeapEntry> {
    Vec::with_capacity(INITIAL_HEAP_CAPACITY)
}

pub fn collect_garbage(state: &mut State) {
    mark_reachable_heap(state);

    let mut open_slots = Vec::new();
    for (current_index, entry) in state.value_heap.iter_mut().enumerate() {
        if !entry.marked.get() {
            open_slots.push(current_index)
        }
    }
    let new_heap_size = state.value_heap.len() - open_slots.len();
    let old_heap_size = state.value_heap.len();

    // relocate any marked heap values beyond the desired capacity and then trim to that capacity
    let mut open_slots_iter = open_slots.iter();
    let mut remapping = HashMap::new();
    for original_slot in new_heap_size..state.value_heap.len() {
        if state.value_heap[original_slot].marked.get() {
            let new_slot = *open_slots_iter.next().unwrap();
            state.value_heap.swap(original_slot, new_slot);
            remapping.insert(original_slot, new_slot);
        }
    }
    state.value_heap.truncate(new_heap_size);

    remap_all_heap_refs(state, &remapping);

    // reserve/shrink heap capacity if necessary, but don't shrink below INITIAL_HEAP_CAPACITY
    let new_heap_capacity = std::cmp::max(HEAP_GROW_FACTOR * new_heap_size, INITIAL_HEAP_CAPACITY);
    if new_heap_capacity > state.value_heap.capacity() {
        state.value_heap.reserve(new_heap_capacity);
    } else {
        state.value_heap.shrink_to(new_heap_capacity);
    }

    if DEBUG_GC_TRACING {
        println!(
            "GC COMPLETE: Marked {} out of {} entries and relocated {} entries. Next GC at {} entries.",
            new_heap_size,
            old_heap_size,
            remapping.len(),
            state.value_heap.capacity()
        );
    }
}

fn mark_reachable_heap(state: &State) {
    let global_refs = state
        .globals
        .values()
        .filter_map(|v| if let Value::HeapRef(loc) = v { Some(*loc) } else { None });
    let stack_refs = state
        .value_stack
        .iter()
        .filter_map(|v| if let Value::HeapRef(loc) = v { Some(*loc) } else { None });
    let call_stack_captured_refs = state.call_stack.iter().flat_map(|v| {
        v.closure.upvalues.iter().filter_map(|v| {
            if let Some(Value::HeapRef(loc)) = *v.closed.borrow() {
                Some(loc)
            } else {
                None
            }
        })
    });

    let all_refs = global_refs.chain(stack_refs).chain(call_stack_captured_refs);
    for loc in all_refs {
        mark_heap_entry(&state.value_heap, loc);
    }
}

// marks the given entry and all entries referenced by it
fn mark_heap_entry(heap: &[HeapEntry], loc: HeapRef) {
    let already_marked = heap[loc.0].marked.replace(true);
    // no need to mark children if this entry has already been processed
    if already_marked {
        return;
    }

    match &heap[loc.0].value {
        HeapValue::Class(class) => {
            class.methods.values().for_each(|v| mark_heap_entry(heap, *v));
        }
        HeapValue::Instance(instance) => {
            mark_heap_entry(heap, instance.class);
            instance
                .fields
                .values()
                .filter_map(|v| if let Value::HeapRef(loc) = v { Some(loc) } else { None })
                .for_each(|loc| mark_heap_entry(heap, *loc));
        }
        HeapValue::BoundMethod(method) => {
            mark_heap_entry(heap, method.instance);
            mark_heap_entry(heap, method.closure);
        }
        HeapValue::Closure(closure) => {
            closure
                .upvalues
                .iter()
                .filter_map(|v| {
                    if let Some(Value::HeapRef(loc)) = *v.closed.borrow() {
                        Some(loc)
                    } else {
                        None
                    }
                })
                .for_each(|loc| mark_heap_entry(heap, loc));
        }
        HeapValue::NativeFunction(_) => {
            // native functions don't have any heap references
        }
    }
}

// reset the marked state of each heap entry and perform applicable remapping for all heap references in the VM state
fn remap_all_heap_refs(state: &mut State, remapping: &HashMap<usize, usize>) {
    for entry in &mut state.value_heap {
        entry.marked.set(false);
        remap_heap_object(&mut entry.value, remapping);
    }

    let remap_fn = |value| remap_value(value, remapping);
    state.globals.values_mut().for_each(remap_fn);
    state.value_stack.iter_mut().for_each(remap_fn);

    let remap_fn = |value: &Rc<Upvalue>| remap_upvalue(value, remapping);
    state
        .call_stack
        .iter()
        .flat_map(|frame| frame.closure.upvalues.iter())
        .for_each(remap_fn);
}

// remap any heap references held by this object
fn remap_heap_object(value: &mut HeapValue, remapping: &HashMap<usize, usize>) {
    match value {
        HeapValue::Class(class) => {
            for heap_ref in class.methods.values_mut() {
                remap_heap_ref(heap_ref, remapping);
            }
        }
        HeapValue::Instance(instance) => {
            remap_heap_ref(&mut instance.class, remapping);
            for value in instance.fields.values_mut() {
                remap_value(value, remapping)
            }
        }
        HeapValue::BoundMethod(method) => {
            remap_heap_ref(&mut method.instance, remapping);
            remap_heap_ref(&mut method.closure, remapping);
        }
        HeapValue::Closure(closure) => {
            for upvalue in &closure.upvalues {
                remap_upvalue(upvalue, remapping)
            }
        }
        HeapValue::NativeFunction(_) => {
            // native functions don't have any heap references
        }
    }
}

fn remap_value(value: &mut Value, remapping: &HashMap<usize, usize>) {
    if let Value::HeapRef(heap_ref) = value {
        remap_heap_ref(heap_ref, remapping);
    }
}

fn remap_upvalue(upvalue: &Upvalue, remapping: &HashMap<usize, usize>) {
    if let Some(Value::HeapRef(mut heap_ref)) = *upvalue.closed.borrow_mut() {
        remap_heap_ref(&mut heap_ref, remapping);
    }
}

fn remap_heap_ref(heap_ref: &mut HeapRef, remapping: &HashMap<usize, usize>) {
    if let Some(new_index) = remapping.get(&heap_ref.0) {
        heap_ref.0 = *new_index;
    }
}
