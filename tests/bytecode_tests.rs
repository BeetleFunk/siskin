use siskin::bytecode;
use siskin::error;
use siskin::error::BasicResult;

type TestResult = BasicResult<()>;

// run siskin code using a fresh interpreter environment and return a string containing the program output
fn run(code: &str) -> error::BasicResult<String> {
    let mut buffer = Vec::new();
    bytecode::execute(code, &mut buffer)?;

    let output = std::str::from_utf8(&buffer.as_slice()).unwrap();

    Ok(output.to_string())
}

#[test]
fn variable_scoping() -> TestResult {
    let code = "\
        var a = \"global a\";\n\
        var b = \"global b\";\n\
        var c = \"global c\";\n\
        {\n\
            var a = \"outer a\";\n\
            var b = \"outer b\";\n\
            {\n\
                var a = \"inner a\";\n\
                print a;\n\
                print b;\n\
                print c;\n\
            }\n\
            print a;\n\
            print b;\n\
            print c;\n\
        }\n\
        print a;\n\
        print b;\n\
        print c;";

    let output = run(code)?;

    let expected = "\
        inner a\n\
        outer b\n\
        global c\n\
        outer a\n\
        outer b\n\
        global c\n\
        global a\n\
        global b\n\
        global c\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn reassignment() -> TestResult {
    let code = "\
        var a = \"original\";\n\
        a = \"updated\";\n\
        print a;";

    let output = run(code)?;

    let expected = "\
        updated\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn shadowing() -> TestResult {
    let code = "\
        var a = 5;
        var a = 6;
        print a;
        a = a;
        print a;
        {
            var a = 7;
            var a = 8;
            print a;
            a = a;
            print a;
        }";

    let output = run(code)?;

    let expected = "\
        6\n6\n8\n8\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn bind_to_global() -> TestResult {
    // since we allow shadowing, this code should compile but then fail at runtime
    let code = "\
        {
            var a = a;
        }";

    let result = run(code);
    let error = result.unwrap_err();
    assert!(error.description.contains("Undefined variable"));

    Ok(())
}

#[test]
fn if_statement() -> TestResult {
    let code = "\
        if (5 < 7) {
            print \"first true\";
        }
        if (false) {
            print \"second true\";
        }";

    let output = run(code)?;
    let expected = "\
            first true\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn if_else_statement() -> TestResult {
    let code = "\
        if (5 + 7 < 9000) {
            print \"hit then\";
        } else {
            print \"hit else\";
        }
        
        if (false) {
            print \"hit then\";
        } else if (true) {
            print \"hit else if\";
        }
        
        if (6 == 5) {
            print \"hit then\";
        } else if(6 == 7) {
            print \"hit else if\";
        } else {
            print \"hit final else\";
        }";

    let output = run(code)?;
    let expected = "\
            hit then\nhit else if\nhit final else\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn logical_and() -> TestResult {
    let code = "\
        if (6 - 2 < 7 and false) {
            print \"condition true\";
        } else {
            print \"condition false\";
        }
        
        if (8 > 2 and !false) {
            print \"condition true\";
        } else {
            print \"condition false\";
        }";

    let output = run(code)?;
    let expected = "condition false\ncondition true\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn logical_or() -> TestResult {
    let code = "\
        if (6 - 2 < 7 or false) {
            print \"condition true\";
        } else {
            print \"condition false\";
        }
        
        if (8 > 2 or !false) {
            print \"condition true\";
        } else {
            print \"condition false\";
        }
        
        if (4 > 9 or 9 < 4) {
            print \"condition true\";
        } else {
            print \"condition false\";
        }";

    let output = run(code)?;
    let expected = "condition true\ncondition true\ncondition false\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn while_loop() -> TestResult {
    let code = "\
        while (99 + 77 <= 1) {
            print \"impossible\";
        }
        
        var a = 0;
        while (a < 5) {
            print a;
            a = a + 1;
        }";

    let output = run(code)?;
    let expected = "0\n1\n2\n3\n4\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn for_loop() -> TestResult {
    let code = "\
        var i = 2;
        for (; i<5; ) {
            print i;
            i = i + 1;
        }

        for (i=999; false; ) {
            print \"impossible\";
        }
        print i;

        {
            for (var j = 0; j<5;) {
                print j;
                j = j + 2;
            }
        }

        for (var i=0; i<5; i = i + 1) {
            print i;
        }";

    let output = run(code)?;
    let expected = "2\n3\n4\n999\n0\n2\n4\n0\n1\n2\n3\n4\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn basic_functions() -> TestResult {
    let code = "\
        fun noArgs() {
            print \"no args\";
        }
        noArgs();
        
        fun oneArg(arg1) {
            print arg1;
        }
        var implicitReturnVal = oneArg(5);
        print implicitReturnVal;
        
        {
            var a = \"a\";

            fun addTwoArgs(arg1, arg2) {
                var sum = arg1 + arg2;
                return sum;
            }

            var b = \"b\";

            print addTwoArgs(21, 78);
        }";

    let output = run(code)?;
    let expected = "no args\n5\nNil\n99\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn local_function_with_local_vars() -> TestResult {
    let code = "\
        {
            var a = 1;
            var b = 2;
            fun display(value) {
                print value;
            }
            a = b;
            display(a);
        }";

    let output = run(code)?;
    let expected = "2\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn recursion() -> TestResult {
    let code = "\
        fun incrementUntil5(value) {
            print value;
            if (value >= 5) {
                return value;
            } else {
                value = value + 1;
                return incrementUntil5(value);
            }
        }
        var five = incrementUntil5(2);
        {
            print five;

            fun decrementUntil10(value) {
                print value;
                if (value <= 10) {
                    return value;
                } else {
                    var decremented = value - 1;
                    return decrementUntil10(decremented);
                }
            }
            var result1 = decrementUntil10(14);
            var result2 = decrementUntil10(12);

            print result1;
            print result2;
        }";

    let output = run(code)?;
    let expected = "2\n3\n4\n5\n5\n14\n13\n12\n11\n10\n12\n11\n10\n10\n10\n";

    assert_eq!(expected, output);

    Ok(())
}

#[test]
fn native_function() -> TestResult {
    let code = "\
        print sqrt(64);

        fun fib(n) {
            if (n < 2) return n;
            return fib(n - 2) + fib(n - 1);
        }

        var start = clock();
        print fib(18);
        print clock() - start;";

    let output = run(code)?;

    assert!(output.starts_with("8"));
    assert!(output.contains("2584"));

    Ok(())
}

#[test]
fn upvalues_on_stack() -> TestResult {
    let code = "\
        fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
                var c = 3;
                var d = 4;
                fun inner() {
                    print a + c + b + d;
                    a = 5;
                }
                inner();
            }
            middle();
            print a;
        }
        outer();";

    let output = run(code)?;

    assert_eq!("10\n5\n", output);

    Ok(())
}

#[test]
fn upvalues_closed_on_return() -> TestResult {
    let code = "\
        fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
                print a + b;
                a = a + 1;
            }
            return middle;
        }
        var closure = outer();
        closure();
        closure();
        closure();";

    let output = run(code)?;

    assert_eq!("3\n4\n5\n", output);

    Ok(())
}

#[test]
fn upvalues_closed_on_scope_exit() -> TestResult {
    let code = "\
        fun outer() {
            var result;
            {
                var a = 1;
                var b = 2;
                fun middle() {
                    print a + b;
                    a = a + 1;
                }
                result = middle;
            }
            return result;
        }
        var closure = outer();
        closure();
        closure();
        closure();";

    let output = run(code)?;

    assert_eq!("3\n4\n5\n", output);

    Ok(())
}

#[test]
fn closure_variable_semantics() -> TestResult {
    // closure variable will share the same upvalues (captured vars) as the assigned closure
    let code = "\
        fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
                print a + b;
                a = a + 1;
            }
            return middle;
        }
        var closureRef1 = outer();
        var closureRef2 = closureRef1;
        closureRef1();
        closureRef2();
        closureRef1();
        closureRef2();";

    let output = run(code)?;

    assert_eq!("3\n4\n5\n6\n", output);

    Ok(())
}

// Using the Rc<RefCell<Value>> technique for moving upvalues to the heap does allow for ownership cycles that will never be cleaned up
// The two closures inside the local scope will never be dropped for the lifetime of the VM because they reference each other via closed upvalue
// To observe this, you can turn on drop tracing for closures
#[test]
fn create_ownership_cycle() -> TestResult {
    let code = "\
        print \"Entering local scope\";
        {
            var SomeFunc;
            fun DoThing() {
                return SomeFunc();
            }
            fun CaptureDoThing(condition) {
                if (condition) {
                    DoThing();
                }
                return condition;
            }
            // create the reference cycle after vars have been captured
            SomeFunc = CaptureDoThing;
        }
        // with proper cycle detection (or tracing GC) it should be possible to drop those local closures at this point
        print \"Exited local scope\";";

    let output = run(code)?;

    assert_eq!("Entering local scope\nExited local scope\n", output);

    Ok(())
}

#[test]
fn empty_class_definition() -> TestResult {
    let code = "\
        class EmptyClass {}
        print EmptyClass;
        print EmptyClass();
        {
            class EmptyLocalClass {}
            print EmptyLocalClass;
            print EmptyLocalClass();
        }";

    let output = run(code)?;

    assert_eq!(
        "EmptyClass\nEmptyClass instance\nEmptyLocalClass\nEmptyLocalClass instance\n",
        output
    );

    Ok(())
}

#[test]
fn get_and_set_instance_fields() -> TestResult {
    let code = "\
        class Stuff {}

        var thing = Stuff();
        thing.first = 1;
        thing.third = thing.second = 2;
        print thing.first + thing.second + thing.third;";

    let output = run(code)?;

    assert_eq!("5\n", output);

    Ok(())
}

#[test]
fn get_undefined_field() -> TestResult {
    let code = "\
        class Stuff {}
        var thing = Stuff();
        thing.first = 1;
        print thing.missing;";

    let result = run(code);
    let error = result.unwrap_err();
    assert!(error.description.contains("Undefined property 'missing'"));

    Ok(())
}

#[test]
fn class_with_methods() -> TestResult {
    let code = "\
        class Bird {
            flap(numberOfTimes) {
                var i = 0;
                while (i < numberOfTimes) {
                    i = i + 1;
                    print \"flap \" + toString(i);
                }
            }
        }
        var sparrow = Bird();
        sparrow.flap(3);";

    let output = run(code)?;
    assert_eq!("flap 1\nflap 2\nflap 3\n", output);
    Ok(())
}

#[test]
fn method_referencing_own_class() -> TestResult {
    let code = "\
        var first;
        {
            class CoolThing {
                makeAnother() {
                    var result = CoolThing();
                    result.name = \"another!\";
                    return result;
                }
            }
            first = CoolThing();
            first.name = \"original!\";
        }
        var second = first.makeAnother();
        print first.name;
        print second.name;";

    let output = run(code)?;
    assert_eq!("original!\nanother!\n", output);
    Ok(())
}

#[test]
fn this_expression() -> TestResult {
    let code = "\
        class ValueAdder {
            compute(input) {
                return input + this.value;
            }
        }
        var adder = ValueAdder();
        adder.value = 5;
        print adder.compute(10);";

    let output = run(code)?;
    assert_eq!("15\n", output);
    Ok(())
}

#[test]
fn invalid_this_expression() -> TestResult {
    {
        let code = "print this;";
        let result = run(code);
        let error = result.unwrap_err();
        assert!(error
            .description
            .contains("Can't use 'this' outside of a class"));
    }
    {
        let code = "\
            fun test() {
                this = 5;
            }";
        let result = run(code);
        let error = result.unwrap_err();
        assert!(error
            .description
            .contains("Can't use 'this' outside of a class"));
    }
    Ok(())
}

#[test]
fn method_recursion() -> TestResult {
    let code = "\
        class FactorialOneShot {
            init() {
                this.accumulator = 1;
            }

            compute(input) {
                this.accumulator = this.accumulator * input;
                if (input > 1) {
                    return this.compute(input - 1);
                } else {
                    return this.accumulator;
                }
            }
        }
        var factorial = FactorialOneShot();
        print factorial.compute(6);";

    let output = run(code)?;
    assert_eq!("720\n", output);
    Ok(())
}

#[test]
fn type_init() -> TestResult {
    let code = "\
        class Point {
            init(x, y) {
                this.x = x;
                this.y = y;
            }

            display() {
                var info = \"x: \" + toString(this.x) + \" y: \" + toString(this.y);
                print info;
            }
        }
        var point = Point(1, 2);
        point.display();";

    let output = run(code)?;
    assert_eq!("x: 1 y: 2\n", output);
    Ok(())
}

#[test]
fn type_init_early_return() -> TestResult {
    let code = "\
        class Thing {
            init() {
                print \"initializing\";
                this.field = 999;
                return;
                print \"boom\";
            }
        }
        var instance = Thing();
        print instance.field;";

    let output = run(code)?;
    assert_eq!("initializing\n999\n", output);
    Ok(())
}

#[test]
fn type_init_argument_count() -> TestResult {
    {
        let code = "\
            class Bad {
                init(a, b) {
                    this.something = b;
                }
            }
            var instance = Bad();";
        let result = run(code);
        let error = result.unwrap_err();
        assert!(error
            .description
            .contains("Expected 2 arguments to the type initializer for class Bad but received 0"));
    }
    {
        let code = "\
            class NoInit {}
            var instance = NoInit(1);";
        let result = run(code);
        let error = result.unwrap_err();
        assert!(error.description.contains(
            "Unexpected arguments to initializer for class NoInit that has no init() method"
        ));
    }
    Ok(())
}

#[test]
fn type_init_invalid_return() -> TestResult {
    let code = "\
            class Bad {
                init() {
                    return 0;
                }
            }";
    let result = run(code);
    let error = result.unwrap_err();
    assert!(error
        .description
        .contains("Can't return a value from an initializer"));
    Ok(())
}

#[test]
fn method_invocation_optimization() -> TestResult {
    let code = "\
        class Dynamite {
            countdown(time) {
                while (time > 0) {
                    print time;
                    time = time - 1;
                }
                print \"Boom!\";
            }
        }
        var boom = Dynamite();
        boom.countdown(5);

        fun confettiCountdown(time) {
            print time;
            if (time > 0) {
                confettiCountdown(time - 1);
            } else {
                print \"Surprise!\";
            }
        }
        // field should shadow the method now
        boom.countdown = confettiCountdown;
        boom.countdown(5);";

    let output = run(code)?;
    assert_eq!("5\n4\n3\n2\n1\nBoom!\n5\n4\n3\n2\n1\n0\nSurprise!\n", output);
    Ok(())
}

#[test]
fn basic_inheritance() -> TestResult {
    let code = "\
        class Charge {
            boom() {
                print \"Asplode!\";
            }
        }
        class Dynamite < Charge {}
        var dynamite = Dynamite();
        dynamite.boom();";

    let output = run(code)?;
    assert_eq!("Asplode!\n", output);
    Ok(())
}
