#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <boost/algorithm/string.hpp>


std::string read_input(const std::string& fname) {
    std::ifstream file(fname);
    std::stringstream content_;
    std::string line;
    while (std::getline(file, line)) {
        content_ << line;
    }
    std::string content = content_.str();
    boost::trim(content);
    return content;
}


unsigned int part1(const std::string& content) {
    std::string offset = std::string(content);
    offset.push_back(content[0]);
    unsigned int sum = 0;
    for (unsigned int i = 0; i < content.size(); i++) {
        if (content[i] == offset[i+1]) {
            sum += content[i] - '0';
        }
    }
    return sum;
}


unsigned int part2(const std::string& content) {
    auto size = content.size();
    auto half = size / 2;
    unsigned int sum = 0;
    for (unsigned int i = 0; i < size; i++) {
        auto other_ind = i + half;
        if (other_ind > size - 1) {
            other_ind = other_ind % size;
        }
        if (content[i] == content[other_ind]) {
            sum += content[i] - '0';
        }
    }
    return sum;
}


int main() {
    std::string content = read_input("../input.txt");
    auto sum1 = part1(content);
    std::cout << "part1: " << sum1 << "\n";
    auto sum2 = part2(content);
    std::cout << "part2: " << sum2 << "\n";
}

