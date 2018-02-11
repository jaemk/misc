#include <string>
#include <vector>
#include <unordered_set>
#include <map>
#include <functional>
#include <fstream>
#include <iterator>
#include <iostream>
#include <boost/algorithm/string.hpp>


std::string read_file(const std::string& fname) {
    std::ifstream file(fname);
    file.seekg(file.end);
    size_t size = file.tellg();
    file.seekg(file.beg);
    std::string s;
    s.reserve(size);
    s.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    boost::trim(s);
    return s;
}


namespace p1 {
    bool is_valid(const std::string& pass) {
        std::string s = boost::trim_copy(pass);
        std::vector<std::string> parts;
        boost::split(parts, s, boost::is_any_of(" "));
        std::unordered_set<std::string> seen;
        for (auto part : parts) {
            if (seen.find(part) != seen.end()) { return false; }
            seen.insert(part);
        }
        return true;
    }
}


namespace p2 {
    std::map<char, uint32_t> char_freq(const std::string& s) {
        std::map<char, uint32_t> freq;
        for (auto c : s) {
            freq[c]++;
        }
        return freq;
    }
    bool is_valid(const std::string& pass) {
        auto s = boost::trim_copy(pass);
        std::vector<std::string> parts;
        boost::split(parts, s, boost::is_any_of(" "));
        std::vector<std::map<char, uint32_t>> freqs;
        for (auto part : parts) {
            freqs.push_back(char_freq(part));
        }
        size_t len = freqs.size();
        for (size_t i = 0; i < len - 1; i++) {
            for (size_t j = i+1; j < len; j++) {
                if (freqs[i] == freqs[j]) { return false; }
            }
        }
        return true;
    }
}


uint32_t solve(std::function<bool(const std::string&)> is_valid, const std::string& input) {
    std::vector<std::string> lines;
    boost::split(lines, input, boost::is_any_of("\n"));
    uint32_t count = 0;
    for (auto line : lines) {
        if (is_valid(line)) { count++; }
    }
    return count;
}


int main() {
    std::cout << "d4\n";
    std::string input = read_file("../input.txt");

    auto ans1 = solve(p1::is_valid, input);
    std::cout << "p1: " << ans1 << std::endl;

    auto ans2 = solve(p2::is_valid, input);
    std::cout << "p2: " << ans2 << std::endl;
}

