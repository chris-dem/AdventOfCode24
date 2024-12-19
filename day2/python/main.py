import numpy as np


def check(direction: int, diff):
    return (
        (np.sign(diff) == direction) & (0 < np.abs(diff)) & (np.abs(diff) <= 3)
    ).all()


def ex(line: list[int]) -> int:
    if len(line) <= 2:
        return 1
    if len(line) == 3:
        return 1 - int(np.all(np.diff(np.array(line)) == 0))
    lnp = np.array(line)
    diff = np.diff(lnp)
    sgns = np.sign(diff)
    d_cnt = {-1: 0, 0: 0, 1: 0}
    direct, c = -1, -1
    sgn = np.unique_counts(sgns)
    for ind, el in enumerate(sgn[0]):
        ci = d_cnt[el] = sgn[1][ind]
        if ci > c:
            c = ci
            direct = el

    if check(direct, diff):
        return 1

    fst = 5
    try:
        fst = int(np.where((sgns != direct) | (0 == abs(diff)) | (abs(diff) > 3))[0][0])
    except Exception as e:
        raise e
    if fst == 0:
        a, b = diff[0], diff[1]
        if 0 < (b + a) * direct <= 3:
            diff = np.concatenate([[a + b], diff[2:]])
        else:
            diff = diff[1:]
    elif fst == len(diff) - 1:
        a, b = diff[-2], diff[-1]
        if 0 < (b + a) * direct <= 3:
            diff = np.concatenate([diff[:-2], [a + b]])
        else:
            diff = diff[:-1]
    else:
        a, b, c = diff[fst - 1], diff[fst], diff[fst + 1]
        # print(a,b,c, direct)
        x = diff.shape[0]
        if np.sign(a) != direct or abs(a) > 3 or abs(a) == 0:
            diff = np.concatenate([diff[: fst - 1], [a + b], diff[fst + 1 :]])
        if np.sign(c) != direct or abs(c) > 3 or abs(c) == 0:
            diff = np.concatenate(
                [diff[:fst], [b + c], diff[fst + 2 :] if fst < len(diff) - 2 else []]
            )
        elif 0 < (b + a) * direct <= 3:
            diff = np.concatenate([diff[: fst - 1], [a + b], diff[fst + 1 :]])
        else:
            diff = np.concatenate(
                [diff[:fst], [b + c], diff[fst + 2 :] if fst < len(diff) - 2 else []]
            )
        y = diff.shape[0]
        assert x - y == 1
    return int(check(direct, diff))


def sol(data: list[list[int]]) -> int:
    d = sum(map(ex, data))
    with open("output.txt", "w+") as f:
        for el in data:
            clc = ex(el)
            if clc == 1:
                m = np.diff(np.array(el))
                if check(np.sign(m[0]), m):
                    continue
                print(el, np.diff(np.array(el)), file=f)
    return d


def main():
    data = None
    with open("input.txt") as f:
    # with open("test_input.txt", "r", encoding="ascii") as f:
        data = f.readlines()
    data = [list(map(int, line.strip().split(" "))) for line in data]
    print(sol(data))


if __name__ == "__main__":
    main()
