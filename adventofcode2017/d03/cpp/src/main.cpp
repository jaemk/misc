#include <iostream>
#include <cmath>
#include <map>

enum class Dir {
    Up, Down, Left, Right
};
Dir turn_left(Dir& dir) {
    switch (dir) {
        case Dir::Up:
            return Dir::Left;
        case Dir::Left:
            return Dir::Down;
        case Dir::Down:
            return Dir::Right;
        case Dir::Right:
            return Dir::Up;
        default:
            std::cerr << "unhandled variant\n";
            std::abort();
    };
}

namespace p1 {
    class Point {
        public:
            uint32_t x, y;
            Point(uint32_t x, uint32_t y): x(x), y(y) {}
            void step(Dir& dir) {
                switch (dir) {
                    case Dir::Up:
                        y++;
                        break;
                    case Dir::Down:
                        y--;
                        break;
                    case Dir::Left:
                        x--;
                        break;
                    case Dir::Right:
                        x++;
                        break;
                    default:
                        std::cerr << "unhandled variant\n";
                        std::abort();
                }
            }
            uint32_t step_distance(Point& other) {
                uint32_t a, b;
                if (x > other.x) { a = x - other.x; } else { a = other.x - x; }
                if (y > other.y) { b = y - other.y; } else { b = other.y - y; }
                return a + b;
            }
    };

    class Mem {
        public:
            uint32_t size;
            Mem(uint32_t size): size(size) {}
            Point find_port(uint32_t port) {
                uint32_t mem_value;
                if (size < 3) { mem_value = 2; }
                else {
                    float s = std::pow(static_cast<float>(size - 2), 2);
                    mem_value = static_cast<uint32_t>(s) + 1;
                }
                uint32_t start_x, start_y;
                start_x = size;
                if (size == 1) { start_y = 1; }
                else { start_y = 2; }
                Point point(start_x, start_y);
                Dir dir = Dir::Up;
                while (mem_value < port) {
                    switch (dir) {
                        case Dir::Up:
                            if (point.y < size) { point.step(dir); }
                            else { dir = turn_left(dir); continue; }
                            break;
                        case Dir::Left:
                            if (point.x > 1) { point.step(dir); }
                            else { dir = turn_left(dir); continue; }
                            break;
                        case Dir::Down:
                            if (point.y > 1) { point.step(dir); }
                            else { dir = turn_left(dir); continue; }
                            break;
                        case Dir::Right:
                            if (point.y < size) { point.step(dir); }
                            else { dir = turn_left(dir); continue; }
                            break;
                    }
                    mem_value += 1;
                }
                return point;
            }
    };

    uint32_t solve(uint32_t input) {
        float size_ = static_cast<float>(input);
        uint32_t size = static_cast<uint32_t>(std::ceil(std::sqrt(size_)));
        if (size % 2 == 0) { size++; }

        Mem mem(size);
        Point point = mem.find_port(input);

        uint32_t origin;
        if (size == 1) { origin = 1; }
        else { origin = ((size - 1) / 2) + 1; }
        Point origin_point(origin, origin);
        return origin_point.step_distance(point);
    }
}

namespace p2 {
    class Point {
        public:
            uint32_t x, y;
            Point(uint32_t x, uint32_t y): x(x), y(y) {}
            bool operator<(const Point& rhs) const {
                if (x == rhs.x) { return y < rhs.y; }
                else { return x < rhs.x; }
            }
            friend std::ostream& operator<<(std::ostream& os, const Point& p) {
                os << "Point { x: " << p.x << ", y: " << p.y << " }";
                return os;
            }
            Point point_left(Dir& dir) {
                switch (dir) {
                    case Dir::Up:
                        return Point(x - 1, y);
                    case Dir::Left:
                        return Point(x, y - 1);
                    case Dir::Down:
                        return Point(x + 1, y);
                    case Dir::Right:
                        return Point(x, y + 1);
                    default:
                        std::cerr << "unhandled variant\n";
                        std::abort();
                }
            }
            Point point_forward(Dir& dir) {
                switch (dir) {
                    case Dir::Up:
                        return Point(x, y+1);
                    case Dir::Left:
                        return Point(x-1, y);
                    case Dir::Down:
                        return Point(x, y-1);
                    case Dir::Right:
                        return Point(x+1, y);
                    default:
                        std::cerr << "unhandled variant\n";
                        std::abort();
                }
            }
    };

    uint64_t sum_surrounding(Point& p, std::map<Point, uint64_t>& field) {
        uint64_t total = 0;
        try { total += field.at(Point(p.x-1, p.y)); }
        catch(const std::out_of_range& _e) {}
        try { total += field.at(Point(p.x-1, p.y-1)); }
        catch(const std::out_of_range& _e) {}
        try { total += field.at(Point(p.x-1, p.y+1)); }
        catch(const std::out_of_range& _e) {}

        try { total += field.at(Point(p.x+1, p.y)); }
        catch(const std::out_of_range& _e) {}
        try { total += field.at(Point(p.x+1, p.y-1)); }
        catch(const std::out_of_range& _e) {}
        try { total += field.at(Point(p.x+1, p.y+1)); }
        catch(const std::out_of_range& _e) {}

        try { total += field.at(Point(p.x, p.y-1)); }
        catch(const std::out_of_range& _e) {}
        try { total += field.at(Point(p.x, p.y+1)); }
        catch(const std::out_of_range& _e) {}
        return total;
    }

    uint64_t solve(uint64_t input) {
        std::map<Point, uint64_t> seen;
        Dir dir = Dir::Right;
        Point point(0, 0);
        seen[point] = 1;
        point = point.point_forward(dir);
        while (true) {
            if (seen.find(point) == seen.end()) {
                uint64_t value = sum_surrounding(point, seen);
                if (value > input) { return value; }
                seen[point] = value;
            }
            Point left = point.point_left(dir);
            if (seen.find(left) == seen.end()) {
                dir = turn_left(dir);
                continue;
            }
            point = point.point_forward(dir);
        }
    }
}

int main() {
    std::cout << "d3\n";
    uint32_t ans = p1::solve(265149);
    std::cout << "p1: " << ans << std::endl;
    uint64_t ans2 = p2::solve(265149);
    std::cout << "p2: " << ans2 << std::endl;
}

