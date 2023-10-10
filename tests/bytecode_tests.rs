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
