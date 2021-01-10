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
    def test_compiler(self):
        makefile_path = "../../Makefile"
        file_names = os.listdir(".")
        for i in range(get_max_test_num()):
            if f"{i}.scm" not in file_names:
                continue
            os.system(f"make -f {makefile_path} {i}; scheme -q < {i}.scm > chez_result.scm; ./{i} > my_result.scm")
            with open("chez_result.scm", "r") as chez_result, open("my_result.scm", "r") as my_result, open("test.scm", "w+") as test:
                test.write(f"(equal? '({chez_result.read()[:-1]}) '({my_result.read()[:-1]}))")
            os.system("scheme -q < test.scm > res.scm")
            with open("test.scm", "r") as comparison, open("res.scm", "r") as result:
                result = result.readline()
                comparison = comparison.read()
                print(f"comparing {comparison}. result {result}.")
                assert result.find("#t") != -1, f"result was {result}. reason: {comparison}"

def clean_up():
    files1 = os.listdir("../..")
    files2 = os.listdir(".")
    for i in range(get_max_test_num()):
        f1, f2, f3 = f"{i}.s", f"{i}.o", f"{i}"
        for f in {f1, f2, f3}.intersection(files1):
            os.remove(f"../../{f}")
        for f in {f1, f2, f3}.intersection(files2):
            os.remove(f"{f}")

if __name__ == "__main__":
    unittest.main()
    clean_up()