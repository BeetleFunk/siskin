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
// the original strategy of primitive reference counting (using Rust's Rc) did not reclaim circular references once they went out of scope
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
            var numLists = 30; //var numLists = 1000;
            var innerListSize = 30; //var innerListSize = 1000;
            for (var i = 0; i < numLists; i = i + 1) {
                var innerList = List();
                for (var j = 0; j < innerListSize; j = j + 1) {
                    innerList.push(j);
                }
                outerList.push(innerList);
            }
            print \"Made a big list!\";
            print \"heap size: \" + toString(getNumHeapEntries());
            sleep(1000);
        }
        print \"List is now out of scope!\";
        sleep(1000);
        print \"heap size: \" + toString(getNumHeapEntries());";

    let output = run(code)?;
    println!("{output}");
    Ok(())
}

#[test]
fn relocated_heap_references() -> TestResult {    
    let code = "\
        class Node {
            init(value) {
                this.value = value;
                this.prev = nil;
                this.next = nil;
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
        var global1 = Node(1);
        var global2 = global1;
        var global3;
        {
            var outerList = List();
            var numLists = 16;
            var innerListSize = 8;
            for (var i = 0; i < numLists; i = i + 1) {
                var innerList = List();
                for (var j = 0; j < innerListSize; j = j + 1) {
                    innerList.push(j);
                }
                outerList.push(innerList);
            }
            print \"Made a big list!\";
            print \"heap size: \" + toString(getNumHeapEntries());
            global2 = Node(3);
            global3 = Node(5);
        }
        print \"List is now out of scope!\";
        print \"heap size: \" + toString(getNumHeapEntries());
        print \"Read global1.value: \" + toString(global1.value);
        print \"Read global2.value: \" + toString(global2.value);
        print \"Read global3.value: \" + toString(global3.value);";
    
    let output = run(code)?;
    println!("{output}");
    Ok(())
}
