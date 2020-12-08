# Scheme comipler tests
a repository for storing the tests of the scheme compiler

## Quick Start
1. clone this repositry into your compiler folder (the folder in which the reader and tag-parser files are).
2. if you don't have alcotest installed, run `opam install alcotest`
3. run the tests by running the command `ocaml <alcotest-test-file-name>` where alcotest-test-file-name can be one of the following:
    - [alcotest_reader_tests.ml](./reader_tests/alcotest_reader_tests.ml)
    - [alcotest_tag_parser_tests.ml](./tag_parser_tests/alcotest_tag_parser_tests.ml)
    - [alcotest_semantic_analyser_tests.ml](./semantics_tests/alcotest_semantic_analyser_tests.ml)

### Alcotest Doesn't work
If the tests don't run because ocaml can't find alcotest, then you can either:
1. try and reinstall ocaml using opam (search for instructions on ocaml's website) then restart your VM/dualboot, install alcotest again an run the test again.
2. use another test framework (whichever you would like) such as oUnit to run the tests. see the testing framwork section for info.

## Adding Tests
The test cases defines as a 3-tuple such that the first element is the test description, the second is the input and the third is the expected output i.e.
```ocaml
("description", <reader/tag-parser/semantic-analyzer input>, <expected-output>)
```
the test cases are specified inside a list in the appropriate file, for example 
- [reader_tests.ml](./reader_tests/reader_tests.ml)
- [tag_parser_tests.ml](./tag_parser_tests/tag_parser_tests.ml)
- [semantic_analyser_tests.ml](./semantics_tests/semantic_analyser_tests.ml)

to add a new test case just add a new item to the appropriate list

## Testing Framework
I use alcotest as my testing package but if it doesn't work for you you can use any other testing package you'de like. since the test cases are agnostic to the testing framework you can just map the test_suite lists to match your testing framework. for example you look at the mapping done in the alcotest tests file.
