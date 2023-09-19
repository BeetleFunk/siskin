use siskin::error;
use siskin::treewalk;

type TestResult = error::GenericResult<()>;

// run siskin code using a fresh interpreter environment and return a string containing the program output
fn run(code: &str) -> error::GenericResult<String> {
    let mut buffer = Vec::new();
    let mut env = treewalk::environment::Environment::new(&mut buffer);
    treewalk::execute(code, &mut env)?;

    let output = std::str::from_utf8(&buffer.as_slice())?;

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
fn if_statement_true() -> TestResult {
    let code = "\
        var a = \"do it\";\n\
        if (a == \"do it\") {\n\
            print \"condition was true\";\n\
        }";

    let output = run(code)?;
    assert_eq!("condition was true\n", output);

    Ok(())
}

#[test]
fn if_statement_false() -> TestResult {
    let code = "\
        var a = true;\n\
        if (!a) {\n\
            print \"condition was true\";\n\
        }";

    let output = run(code)?;
    assert!(output.is_empty());

    Ok(())
}

#[test]
fn if_else_statement() -> TestResult {
    let code = "\
        var a = \"cond2\";\n\
        if (a == \"cond1\") {\n\
            print \"if condition was true\";\n\
        } else if (a == \"cond2\") {\n\
            print \"else condition was true\";\n\
        }";

    let output = run(code)?;
    assert_eq!("else condition was true\n", output);

    Ok(())
}

#[test]
fn logical_or() -> TestResult {
    let code = "\
        var a = true;\n\
        var b = false;\n\
        if (a or b) {\n\
            print \"condition was true\";\n\
        }";

    let output = run(code)?;
    assert_eq!("condition was true\n", output);

    Ok(())
}

#[test]
fn logical_and() -> TestResult {
    let code = "\
        var a = true;\n\
        var b = false;\n\
        if (a and b) {\n\
            print \"condition was true\";\n\
        }";

    let output = run(code)?;
    assert!(output.is_empty());

    Ok(())
}

#[test]
fn while_loop() -> TestResult {
    let code = "\
        var a = 0;\n\
        while (a < 5) {\n\
            print a;\n\
            a = a + 1;\n\
        }";

    let output = run(code)?;
    assert_eq!("0\n1\n2\n3\n4\n", output);

    Ok(())
}

#[test]
fn for_loop() -> TestResult {
    let code = "\
        for (var a = 0; a < 5; a = a + 1) { print a; }
        for (var a = 14; a >= 10; ) { print a; a = a - 1; }
        var a = 20;
        for (; a < 25;) { print a; a = a + 1; }";

    let output = run(code)?;
    assert_eq!(
        "0\n1\n2\n3\n4\n14\n13\n12\n11\n10\n20\n21\n22\n23\n24\n",
        output
    );

    Ok(())
}

#[test]
fn function_declaration() -> TestResult {
    let code = "\
        fun say(n) {
            if (n > 2) print \"big one\";
            print n;
        }
        say(3);";

    let output = run(code)?;
    assert_eq!("big one\n3\n", output);

    Ok(())
}

#[test]
fn function_return() -> TestResult {
    let code = "\
        fun getNumber() {
            return 82;
            print \"Shouldn't reach this.\";
        }
        var result = getNumber();
        print result;";

    let output = run(code)?;
    assert_eq!("82\n", output);

    Ok(())
}

#[test]
fn recursion() -> TestResult {
    let code = "\
        fun fib(n) {
            if (n <= 1) return n;
            return fib(n - 2) + fib(n - 1);
        }
        
        for (var i = 0; i < 20; i = i + 1) {
            print fib(i);
        }";

    let output = run(code)?;
    assert_eq!("0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n", output);

    Ok(())
}

#[test]
fn first_class_functions() -> TestResult {
    let code = "\
        fun say(n) {
            print n;
        }
        var sayAlias = say;
        sayAlias(\"test string\");";

    let output = run(code)?;
    assert_eq!("test string\n", output);

    Ok(())
}

#[test]
fn function_capture() -> TestResult {
    let code = "\
        var funcRef;
        {
            var divisor = 2;
            fun printHalf(n) {
                var result = n / divisor;
                print result;
            }
            funcRef = printHalf;
        }
        var divisor = 300;
        funcRef(8);";

    let output = run(code)?;
    assert_eq!("4\n", output);

    Ok(())
}

#[test]
fn capture_with_reassignment() -> TestResult {
    let code = "\
        var funcRef;
        {
            var divisor = 2;
            fun printDivisionResult(n) {
                var result = n / divisor;
                print result;
            }
            printDivisionResult(32);
            divisor = 8;
            funcRef = printDivisionResult;
            printDivisionResult(32);
        }
        var divisor = 400; // not captured and shouldn't affect the function
        funcRef(32);";

    let output = run(code)?;
    assert_eq!("16\n4\n4\n", output);

    Ok(())
}
