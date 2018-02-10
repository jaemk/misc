
#include <string>
#include <vector>
#include <cstdint>
#include <limits>
#include <fstream>
#include <iostream>
#include <boost/algorithm/string.hpp>


std::string read_input(const std::string& fname) {
    std::ifstream file(fname);
    file.seekg(0, file.end);
    std::size_t len = file.tellg();
    file.seekg(0, file.beg);
    std::string s;
    s.reserve(len);
    s.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    boost::trim(s);
    return s;
}


uint32_t part1(const std::string& input) {
    uint32_t sum = 0;
    std::vector<std::string> lines;
    boost::split(lines, input, boost::is_any_of("\n"));
    for (auto line : lines) {
        std::vector<std::string> nums;
        boost::split(nums, line, boost::is_any_of("\t"));
        uint32_t min_ = std::numeric_limits<uint32_t>::max();
        uint32_t max_ = 0;
        for (auto num : nums) {
            uint32_t n = std::stoul(num);
            if (n < min_) { min_ = n; }
            if (n > max_) { max_ = n; }
        }
        sum += max_ - min_;
    }
    return sum;
}


uint32_t part2(const std::string& input) {
    uint32_t sum = 0;
    std::vector<std::string> lines;
    boost::split(lines, input, boost::is_any_of("\n"));
    for (auto line : lines) {
        std::vector<std::string> str_nums;
        boost::split(str_nums, line, boost::is_any_of("\t"));
        std::vector<uint32_t> nums;
        for (auto s : str_nums) {
            nums.push_back(std::stoul(s));
        }
        bool next_line = false;
        for (uint32_t n : nums) {
            if (next_line) { break; }
            for (uint32_t other : nums) {
                if (n == other) { continue; }
                if (other % n == 0) {
                    sum += other / n;
                    next_line = true;
                    break;
                }
            }
        }
    }
    return sum;
}


int main() {
    std::string input = read_input("../input.txt");
    auto p1 = part1(input);
    std::cout << "part1: " << p1 << std::endl;
    auto p2 = part2(input);
    std::cout << "part2: " << p2 << std::endl;
}

