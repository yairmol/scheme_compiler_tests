import os
import unittest

def get_max_test_num():
    files = os.listdir(".")
    max_test_num = 0
    for file_name in files:
        if file_name.find("scheme") == -1 and file_name.find(".scm") != -1:
            try:
                max_test_num = max(max_test_num, int(file_name[:file_name.find(".scm")]))
            except ValueError:
                pass
    return max_test_num


class CompilerTests(unittest.TestCase):
    pass

def create_tests():
    makefile_path = "../../Makefile"
    file_names = os.listdir(".")
    for i in range(get_max_test_num()):
        if f"{i}.scm" not in file_names:
            continue
        def test(self):
            os.system(f"""make -f {makefile_path} {i};\\
                o1=`scheme -q < {i}.scm`; o2=`./{i}`;\\
                echo \"(equal? '($o1) '($o2))\" > test.scm;\\
                scheme -q < test.scm > res.scm;\\
                unset o1; unset o2""")
            with open("test.scm", "r") as comparison, open("res.scm", "r") as result:
                result = result.readline()
                assert result == "#t", f"result was {result}. reason: {comparison.read()}"
        setattr(CompilerTests, f"test_{i}", test)

if __name__ == "__main__":
    create_tests()
    unittest.main()