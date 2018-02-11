#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <map>
#include <boost/algorithm/string.hpp>


std::vector<uint32_t> read_input(const std::string& fname) {
    std::ifstream file(fname);
    std::string line;
    std::getline(file, line);
    boost::trim(line);
    std::vector<std::string> parts;
    boost::split(parts, line, boost::is_any_of(" \t"));
    std::vector<uint32_t> nums;
    for (auto s : parts) { nums.push_back(std::stoul(s)); }
    return nums;
}


size_t find_max(const std::vector<uint32_t>& banks) {
    size_t ind = 0;
    uint32_t max = 0;
    for (size_t i = 0; i < banks.size(); i++) {
        if (banks[i] > max) {
            max = banks[i];
            ind = i;
        }
    }
    return ind;
}


void redistribute(std::vector<uint32_t>& banks) {
    size_t i = find_max(banks);
    size_t len = banks.size();
    auto incr = [len](size_t i) -> size_t {
        if (i < len - 1) { return i + 1; }
        return 0;
    };
    uint32_t blocks = banks[i];
    banks[i] = 0;
    while (blocks > 0) {
        i = incr(i);
        banks[i]++;
        blocks--;
    }
    return;
}


struct Ans { uint32_t total_steps, steps_in_loop; };


Ans solve(std::vector<uint32_t> banks) {
    uint32_t steps = 0;
    std::map<std::vector<uint32_t>, uint32_t> seen;
    while (seen.find(banks) == seen.end()) {
        seen[banks] = steps;
        redistribute(banks);
        steps++;
    }
    uint32_t steps_in_loop = steps - seen[banks];
    return Ans { steps, steps_in_loop };
}


int main() {
    std::cout << "d6\n";
    auto input = read_input("../input.txt");
    auto ans = solve(input);
    std::cout << "p1: " << ans.total_steps << std::endl;
    std::cout << "p2: " << ans.steps_in_loop << std::endl;
}

