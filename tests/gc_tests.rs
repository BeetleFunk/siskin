use siskin::bytecode;
use siskin::error;
use siskin::error::BasicResult;

type TestResult = BasicResult<()>;

// write the definition once since these classes are used in multiple garbage collection tests
const LINKED_LIST_DEFINITION: &str = "\
    class Node {
        init(value) {
            this.value = value;
        }
    }

    class LinkedList {
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
    }";

// setup common code in a new Siskin VM, run the provided test script, and return a string containing the program output
fn run(code: &str) -> error::BasicResult<String> {
    let mut buffer = Vec::new();
    let mut vm_state = bytecode::create_vm();
    bytecode::interpret(&mut vm_state, LINKED_LIST_DEFINITION, &mut buffer)?;
    bytecode::interpret(&mut vm_state, code, &mut buffer)?;
    let output = std::str::from_utf8(buffer.as_slice()).unwrap();
    Ok(output.to_string())
}

#[test]
// the original strategy of primitive reference counting (using Rust's Rc) did not reclaim circular references once they went out of scope
fn circular_references() -> TestResult {
    let code = "\
        var heapSizeBeforeGC;
        {
            var outerList = LinkedList();
            var numLists = 100;
            var innerListSize = 200;
            for (var i = 0; i < numLists; i = i + 1) {
                var innerList = LinkedList();
                for (var j = 0; j < innerListSize; j = j + 1) {
                    innerList.push(j);
                }
                outerList.push(innerList);
            }
            forceGC();
            print \"Made a big list on the heap.\";
            heapSizeBeforeGC = getNumHeapEntries();
        }
        print \"List is now out of scope. Forcing garbage collection.\";
        forceGC();
        var heapEntriesFreed = heapSizeBeforeGC - getNumHeapEntries();
        print \"Freed \" + heapEntriesFreed + \" heap entries\";";

    let output = run(code)?;
    let expected = "\
        Made a big list on the heap.\n\
        List is now out of scope. Forcing garbage collection.\n\
        Freed 20201 heap entries\n";
    assert_eq!(expected, output);
    Ok(())
}

#[test]
fn relocated_heap_references() -> TestResult {
    let code = "\
        var global1 = Node(1);
        var global2 = global1;
        var global3;
        var heapSizeBeforeGC;
        {
            var outerList = LinkedList();
            var numLists = 100;
            var innerListSize = 100;
            for (var i = 0; i < numLists; i = i + 1) {
                var innerList = LinkedList();
                for (var j = 0; j < innerListSize; j = j + 1) {
                    innerList.push(j);
                }
                outerList.push(innerList);
            }
            // set these globals to new heap objects that should exist after the big list
            global2 = Node(3);
            global3 = Node(5);
            forceGC();
            print \"Made a big list on the heap.\";
            heapSizeBeforeGC = getNumHeapEntries();
        }
        print \"List is now out of scope. Forcing garbage collection.\";
        forceGC();
        var heapEntriesFreed = heapSizeBeforeGC - getNumHeapEntries();
        print \"Freed \" + heapEntriesFreed + \" heap entries\";
        print \"Read global1.value: \" + toString(global1.value);
        print \"Read global2.value: \" + toString(global2.value);
        print \"Read global3.value: \" + toString(global3.value);";

    let output = run(code)?;
    let expected = "\
        Made a big list on the heap.\n\
        List is now out of scope. Forcing garbage collection.\n\
        Freed 10201 heap entries\n\
        Read global1.value: 1\n\
        Read global2.value: 3\n\
        Read global3.value: 5\n";
    assert_eq!(expected, output);
    Ok(())
}
