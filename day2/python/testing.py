import unittest
from itertools import accumulate
from hypothesis import given, strategies as st
from main import ex


def stuff(idx, shift, lis) -> list[int]:
    lis.insert(idx, lis[idx] + shift)
    return lis


class Testing(unittest.TestCase):

    @given(
        st.tuples(
            st.one_of(st.just(-1), st.just(1)),
            st.integers(min_value=100, max_value=1000),
            st.lists(st.one_of(st.just(1), st.just(2), st.just(3)), min_size=4),
        )
        .map(
            lambda x: list(
                accumulate(x[2], lambda acc, el: acc + el * x[0], initial=x[1])
            )
        )
        .flatmap(
            lambda x: st.tuples(
                st.integers(min_value=0, max_value=len(x) - 1),
                st.integers(min_value=-10, max_value=10).filter(
                    lambda x: x not in (1, 2, 3)
                ),
                st.just(x),
            )
        )
        .map(lambda x: stuff(x[0], x[1], x[2])),
    )
    def test_correct(self, lis):
        self.assertEqual(ex(lis), 1)


if __name__ == "__main__":
    unittest.main()
