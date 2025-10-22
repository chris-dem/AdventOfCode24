import sys
from collections import deque
from itertools import islice, chain


def file_input() -> list[str]:
    file_name = "test_input.txt" if len(sys.argv) > 1 else "input.txt"
    ret = []
    with open(file_name, "r") as f:
        ret = list(map(lambda x: x.strip(), f.readlines()))
    return ret


to_find = "XMAS"


def main_diag(arr: list[list[any]]) -> list[list[any]]:
    ret = []
    n = len(arr)
    for i in range(n):
        app = []
        app2 = []
        for j in range(n):
            if i + j < n:
                app.append(arr[i + j][j])
                if i != 0:
                    app2.append(arr[j][j + i])
        ret.append(app)
        if i > 0:
            ret.append(app2)
    return ret


def sec_diag(arr: list[list[any]]) -> list[list[any]]:
    ret = []
    n = len(arr)
    for i in range(n):
        app = []
        app2 = []
        for j in range(n):
            if i + j < n:
                app.append(arr[n - 1 - j - i][j])
                if i != 0:
                    app2.append(arr[n - 1 - j][j + i])
        ret.append(app)
        if i > 0:
            ret.append(app2)
    return ret


def sliding_window(iterable, n):
    it = iter(iterable)
    window = deque(islice(it, n), maxlen=n)
    if len(window) == n:
        yield window
    for x in it:
        window.append(x)
        yield tuple(window)


def part1(arr: list[str]) -> int:
    count = 0
    lists = (
        arr,  ## Rows
        list(zip(*arr)),  ## Cols
        main_diag(arr),
        sec_diag(arr),
    )
    for el in chain(*lists):
        for w in sliding_window(el, len(to_find)):
            w = "".join(w)
            count += w == to_find
            count += w == to_find[::-1]
    return count


def check_X(sub_arr: list[str]):
    assert len(sub_arr) == 3
    assert all(map(lambda x: len(x) == 3, sub_arr)), sub_arr
    # print(*sub_arr, sep="\n")
    one_d = "".join([sub_arr[0][0], sub_arr[1][1], sub_arr[2][2]])
    two_d = "".join([sub_arr[2][0], sub_arr[1][1], sub_arr[0][2]])
    # print(one_d, two_d)
    # print("---")
    return (two_d == "MAS" or two_d == "SAM") and (one_d == "MAS" or one_d == "SAM")


def extract_x(arr, n):
    def ex(i, j):
        if i + 2 >= n or j + 2 >= n:
            return None
        return [
            arr[i][j : j + 3],
            arr[i + 1][j : j + 3],
            arr[i + 2][j : j + 3],
        ]

    return ex


def part2(arr):
    n = len(arr)
    foo = extract_x(arr, n)
    count = 0
    for i in range(n - 1):
        for j in range(n - 1):
            if arr[i + 1][j + 1] != "A":
                continue
            X = foo(i, j)
            if X is None:
                break
            flag = check_X(X)
            if not flag:
                print(*X, sep="\n")
                print(flag)
            count += flag
    return count


def main():
    arr = file_input()
    # print(part1(arr))
    print(part2(arr))


if __name__ == "__main__":
    main()
