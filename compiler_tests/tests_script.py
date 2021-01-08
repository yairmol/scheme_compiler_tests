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
        for i in range(get_max_test_num()+1):
            if f"{i}.scm" not in file_names:
                continue
            os.system("unset o1; unset o2")
            os.system(f"""make -f {makefile_path} {i};\
o1=`scheme -q < {i}.scm`; o2=`./{i}`;\
echo "(equal? '($o1) '($o2))" > test.scm;\
scheme -q < test.scm > res.scm;\
unset o1; unset o2""")
            with open("test.scm", "r") as comparison, open("res.scm", "r") as result:
                result = result.readline()
                comparison = comparison.readline()
                print(f"comparing {comparison}. result {result}.")
                assert result.find("#t") != -1, f"result was {result}. reason: {comparison}"

def clean_up():
    files1 = os.listdir("../..")
    files2 = os.listdir(".")
    for i in range(get_max_test_num()+1):
        f1, f2, f3 = f"{i}.s", f"{i}.o", f"{i}"
        for f in {f1, f2, f3}.intersection(files1):
            os.remove(f"../../{f}")
        for f in {f1, f2, f3}.intersection(files2):
            os.remove(f"{f}")

if __name__ == "__main__":
    try:
        unittest.main()
    except Exception:
        pass
    finally:
        clean_up()