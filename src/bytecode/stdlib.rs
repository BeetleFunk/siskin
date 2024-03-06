use std::thread;
use std::time::{Duration, Instant};

use once_cell::sync::Lazy;

use super::value::{HeapEntry, NativeFunction, Value};
use crate::error::{BasicError, BasicResult};

// TODO: make this thread local and avoid requirement on sync?
static EPOCH: Lazy<Instant> = Lazy::new(Instant::now);

// for stdlib functions like forceGC() that need special handling in the VM itself
pub struct VirtualMachineFlags {
    pub force_gc: bool,
}

pub type NativeFnResult = BasicResult<(Value, Option<VirtualMachineFlags>)>;

impl From<Value> for NativeFnResult {
    fn from(value: Value) -> Self {
        Ok((value, None))
    }
}

pub fn native_functions() -> Vec<NativeFunction> {
    vec![
        NativeFunction {
            arity: 0,
            func: clock,
            name: "clock".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: sqrt,
            name: "sqrt".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: to_string,
            name: "toString".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: sleep,
            name: "sleep".to_string(),
        },
        NativeFunction {
            arity: 0,
            func: get_num_heap_entries,
            name: "getNumHeapEntries".to_string(),
        },
        NativeFunction {
            arity: 0,
            func: force_gc,
            name: "forceGC".to_string(),
        },
    ]
}

fn clock(_heap: &[HeapEntry], _args: &[Value]) -> NativeFnResult {
    // make sure epoch is initialized first (lazy init)
    let epoch = *EPOCH;
    let duration = Instant::now() - epoch;
    // lossy conversion to f64 here, shouldn't be an issue for a while though
    Value::Number(duration.as_millis() as f64).into()
}

fn sqrt(_heap: &[HeapEntry], args: &[Value]) -> NativeFnResult {
    if let Value::Number(value) = args[0] {
        Value::from(value.sqrt()).into()
    } else {
        Err(BasicError::new("Expected number argument for sqrt function."))
    }
}

fn to_string(heap: &[HeapEntry], args: &[Value]) -> NativeFnResult {
    Value::from(args[0].to_string(heap)).into()
}

fn sleep(_heap: &[HeapEntry], args: &[Value]) -> NativeFnResult {
    if let Value::Number(value) = args[0] {
        if value < 0.0 {
            return Err(BasicError::new("Expected positive number argument for sleep function."));
        }
        let duration = Duration::from_millis(value as u64);
        thread::sleep(duration);
        Value::Nil.into()
    } else {
        Err(BasicError::new("Expected number argument for sleep function."))
    }
}

fn get_num_heap_entries(heap: &[HeapEntry], _args: &[Value]) -> NativeFnResult {
    Value::Number(heap.len() as f64).into()
}

fn force_gc(_heap: &[HeapEntry], _args: &[Value]) -> NativeFnResult {
    Ok((Value::Nil, Some(VirtualMachineFlags { force_gc: true })))
}
