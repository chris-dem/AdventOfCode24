#include <fstream>
#include <iostream>
#include <string>
#include <regex>

std::string read_file(bool debug) {
    std::ifstream input_data;
    std::string lines;
    if (debug) {
        input_data.open("test_input.txt");
    } else {
        input_data.open("input.txt");
    }

    if (input_data.is_open()) {
        std::string line;
        while (getline(input_data, line)) {
            lines += line;
        }
        input_data.close();
    } else {
        std::cout << "Unable to open file";
        exit(1);
    }
    return lines;
}

int solution(std::string inp) {
    std::regex mul_experssion("mul\\([0-9]{1,3}, [0-9]{1,3}\\)");
    auto iter = std::sregex_iterator(inp.begin(), inp.end(), mul_experssion);
    std::cout << std::distance(iter, std::sregex_iterator()) << std::endl;
    return 0;
}

int main() {
    std::string lines = read_file(false);
    solution(lines);
    std::cout << lines.length() << std::endl;
    return 0;
}
