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
// the original strategy of primitive reference counting (using Rust's Rc) does not reclaim circular references once they go out of scope
fn circular_references() -> TestResult {    
    let code = "\
        class Node {
            init(value) {
                this.value = value;
            }
        }

        class List {
            init() {
                this.start = nil;
                this.end = nil;
            }

            push(value) {
                var node = Node(value);
                if (this.start == nil) {
                    node.prev = nil;
                    node.next = nil;
                    this.start = node;
                    this.end = node;
                } else {
                    node.prev = this.end;
                    node.next = nil;
                    this.end.next = node;
                    this.end = node;
                }
            }

            toString() {
                if (this.start == nil) {
                    return \"EMPTY LIST\";
                }

                var current = this.start;
                var contents = \"[\" + toString(current.value);
                while (current.next != nil) {
                    current = current.next;
                    contents = contents + \", \" + toString(current.value);
                }
                contents = contents + \"]\";
                return contents;
            }
        }

        {
            var outerList = List();
            var numLists = 1000;
            var innerListSize = 1000;
            for (var i = 0; i < numLists; i = i + 1) {
                var innerList = List();
                for (var j = 0; j < innerListSize; j = j + 1) {
                    innerList.push(j);
                }
                outerList.push(innerList);
            }
            print \"Made a big list!\";
            sleep(10000);
        }
        print \"List is now out of scope!\";
        sleep(10000);";
    
    let output = run(code)?;
    println!("{output}");
    Ok(())
}
