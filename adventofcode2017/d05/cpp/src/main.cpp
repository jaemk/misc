#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iterator>
#include <boost/algorithm/string.hpp>
#include <fstream>


std::vector<int32_t> read_file(const std::string& fname) {
    std::ifstream file(fname);
    std::string line;
    std::vector<int32_t> lines;
    while (std::getline(file, line)) {
        boost::trim(line);
        if (line.size() == 0) { continue; }
        int32_t n = std::stol(line);
        lines.push_back(n);
    }
    return lines;
}


namespace p1 {
    int32_t modifier(int32_t n) {
        return n + 1;
    }
}

namespace p2 {
    int32_t modifier(int32_t n) {
        if (n > 2) { return n - 1; }
        return n + 1;
    }
}


template <typename Func>
uint32_t escape(Func modifier, std::vector<int32_t> input) {
    uint32_t count = 0;
    int32_t i = 0;
    int32_t len = static_cast<int32_t>(input.size());
    while (i >= 0 && i < len) {
        int32_t jump = input[i];
        input[i] = modifier(input[i]);
        i += jump;
        count++;
    }
    return count;
}


int main() {
    std::vector<int32_t> input = read_file("../input.txt");
    std::cout << "d5\n";

    auto ans1 = escape(p1::modifier, input);
    std::cout << "p1: " << ans1 << std::endl;

    auto ans2 = escape(p2::modifier, input);
    std::cout << "p2: " << ans2 << std::endl;
}

