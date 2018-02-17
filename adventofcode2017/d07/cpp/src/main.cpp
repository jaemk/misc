#include <stdexcept>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <map>
#include <unordered_set>
#include <iterator>
#include <fstream>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/iter_find.hpp>
#include <boost/algorithm/string/finder.hpp>


std::string read_file(const std::string& fname) {
    std::ifstream file(fname);
    file.seekg(file.end);
    size_t len = file.tellg();
    file.seekg(file.beg);
    std::string content;
    content.resize(len);
    content.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    boost::trim(content);
    return content;
}


class Disc {
    private:
        std::vector<std::string> _children;
        bool _weight_set;
        uint32_t _child_weight;
    public:
        std::string name;
        uint32_t weight;

        void from_line(const std::string& line) {
            std::vector<std::string> parts;
            iter_split(parts, line, boost::algorithm::first_finder("->"));
            if (parts.size() > 1) {
                std::vector<std::string> kids;
                boost::trim(parts[1]);
                boost::split(kids, parts[1], boost::is_any_of(","));
                for (auto kid : kids) {
                    boost::trim(kid);
                    this->_children.push_back(kid);
                }
            }
            boost::trim(parts[0]);
            std::vector<std::string> info;
            boost::split(info, parts[0], boost::is_any_of(" "));
            this->name = info[0];
            boost::trim_left_if(info[1], boost::is_any_of("("));
            boost::trim_right_if(info[1], boost::is_any_of(")"));
            this->weight = std::stol(info[1]);
        }

        const std::vector<std::string>& children() const { return this->_children; }

        uint32_t children_weight(const std::map<std::string, Disc> discs) {
            if (this->_weight_set) {
                return this->_child_weight;
            }
            throw std::logic_error("Unimplemented");
            return discs.size();
        }

        std::string display() {
            std::stringstream s;
            s << this->name << " (" << this->weight << ") ";
            for (auto kid : _children) {
                s << kid << " ";
            }
            return s.str();
        }
};


class Tower {
    public:
        std::map<std::string, Disc> inner;
        void from_input(const std::string& input) {
            std::vector<std::string> lines;
            boost::split(lines, input, boost::is_any_of("\n"));
            for (auto line : lines) {
                Disc d;
                d.from_line(line);
                this->inner[d.name] = std::move(d);
            }
        }

        void display() {
            for (auto e : inner) {
                std::cout << e.first << ": " << e.second.display() << std::endl;
            }
        }
};


std::string part1(Tower& tower) {
    std::unordered_set<std::string> names;
    for (auto const& e : tower.inner) {
        names.insert(e.first);
    }
    for (auto const& e : tower.inner) {
        auto kids = e.second.children();
        for (auto const& kid : kids) {
            names.erase(kid);
        }
    }
    if (names.size() > 1) {
        throw std::invalid_argument("Expected one base disc");
    }
    for (auto e : names) { return e; }
    throw std::invalid_argument("Expected one base disc");
}


int main() {
    std::cout << "d7" << std::endl;

    std::string input = read_file("../input.txt");
    Tower tower;
    tower.from_input(input);
    /* tower.display(); */

    auto ans1 = part1(tower);
    std::cout << "part1: " << ans1 << std::endl;
}

