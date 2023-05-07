use siskin::error;

type TestResult = error::GenericResult<()>;

// run siskin code using a fresh interpreter environment and return a string containing the program output
fn run(code: &str) -> error::GenericResult<String> {
    let mut buffer = Vec::new();
    let mut env = siskin::Environment::new(&mut buffer);
    siskin::execute(code, &mut env)?;

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
        var a = \"nope\";\n\
        if (a == \"do it\") {\n\
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
            a = a + 1;
        }";

    let output = run(code)?;
    assert_eq!("0\n1\n2\n3\n4\n", output);

    Ok(())
}
