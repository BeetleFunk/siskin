use std::thread;
use std::time::{Duration, Instant};

use crate::error::{BasicError, BasicResult};

use super::value::{HeapEntry, NativeFunction, Value};

use once_cell::sync::Lazy;

// TODO: make this thread local and avoid requirement on sync?
static EPOCH: Lazy<Instant> = Lazy::new(Instant::now);

pub fn standard_library() -> Vec<NativeFunction> {
    vec![
        NativeFunction {
            arity: 0,
            func: native_clock,
            name: "clock".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: native_sqrt,
            name: "sqrt".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: native_to_string,
            name: "toString".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: native_sleep,
            name: "sleep".to_string(),
        },
        NativeFunction {
            arity: 0,
            func: native_heap_check,
            name: "getNumHeapEntries".to_string(),
        },
    ]
}

fn native_clock(_heap: &[HeapEntry], _args: &[Value]) -> BasicResult<Value> {
    // make sure epoch is initialized first (lazy init)
    let epoch = *EPOCH;
    let duration = Instant::now() - epoch;
    // lossy conversion to f64 here, shouldn't be an issue for a while though
    Ok((duration.as_millis() as f64).into())
}

fn native_sqrt(_heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    if let Value::Number(value) = args[0] {
        Ok(Value::from(value.sqrt()))
    } else {
        Err(BasicError::new(
            "Expected number argument for sqrt function.",
        ))
    }
}

fn native_to_string(heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    Ok(Value::from(args[0].to_string(heap)))
}

fn native_sleep(_heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    if let Value::Number(value) = args[0] {
        if value < 0.0 {
            return Err(BasicError::new(
                "Expected positive number argument for sleep function.",
            ));
        }
        let duration = Duration::from_millis(value as u64);
        thread::sleep(duration);
        Ok(Value::Nil)
    } else {
        Err(BasicError::new(
            "Expected number argument for sleep function.",
        ))
    }
}

fn native_heap_check(heap: &[HeapEntry], _args: &[Value]) -> BasicResult<Value> {
    Ok(Value::Number(heap.len() as f64))
}
