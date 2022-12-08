#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <utility>

using Matrix = std::vector<std::string>;
using MatrixCoords = uint64_t;
using CountSet = std::set<MatrixCoords>;

MatrixCoords matrix_coords(size_t row_idx, size_t column_idx) {
    return (row_idx << 32) | column_idx;
}

void count_down(Matrix& matrix, CountSet& count_set) {
    auto& first_row = matrix.front();
    size_t row_length = first_row.size();

    for (size_t column_idx = 1; column_idx < row_length - 1; column_idx++) {
        char min_height = first_row[column_idx];
        for (size_t row_idx = 1; row_idx < matrix.size() - 1; row_idx ++) {
            auto& row = matrix[row_idx];
            char tree_height = row[column_idx];
            if (tree_height > min_height) {
                count_set.insert(matrix_coords(row_idx, column_idx));
                min_height = std::max(tree_height, min_height);
            }
        }
    }
}

void count_up(Matrix& matrix, CountSet& count_set) {
    auto& last_row = matrix.back();
    size_t row_length = last_row.size();

    for (size_t column_idx = 1; column_idx < row_length - 1; column_idx++) {
        char min_height = last_row[column_idx];
        for (size_t row_idx = 1; row_idx < matrix.size() - 1; row_idx ++) {
            size_t actual_row_idx = matrix.size() - row_idx - 1;
            auto& row = matrix[actual_row_idx];
            char tree_height = row[column_idx];
            if (tree_height > min_height) {
                count_set.insert(matrix_coords(actual_row_idx, column_idx));
                min_height = std::max(tree_height, min_height);
            }
        }
    }
}

void count_right(Matrix& matrix, CountSet& count_set) {
    size_t column_length = matrix.size();

    for (size_t row_idx = 1; row_idx < column_length - 1; row_idx ++) {
        auto& row = matrix[row_idx];
        char min_height = row.front();
        for (size_t column_idx = 1; column_idx < row.size() - 1; column_idx++) {
            char tree_height = row[column_idx];
            if (tree_height > min_height) {
                count_set.insert(matrix_coords(row_idx, column_idx));
                min_height = std::max(tree_height, min_height);
            }
        }
    }
}

void count_left(Matrix& matrix, CountSet& count_set) {
    size_t column_length = matrix.size();

    for (size_t row_idx = 1; row_idx < column_length - 1; row_idx ++) {
        auto& row = matrix[row_idx];
        char min_height = row.back();
        for (size_t column_idx = 1; column_idx < row.size() - 1; column_idx++) {
            size_t actual_column_idx = row.size() - column_idx - 1;
            char tree_height = row[actual_column_idx];
            if (tree_height > min_height) {
                count_set.insert(matrix_coords(row_idx, actual_column_idx));
                min_height = std::max(tree_height, min_height);
            }
        }
    }
}

int main(void) {
    /*
     * Read matrix from input
     */
    std::string line;
    assert(std::getline(std::cin, line));

    size_t row_length = line.size();
    assert(row_length > 0);
    if (line.back() == '\n') {
        row_length -= 1;
    }

    Matrix matrix;
    matrix.push_back(std::move(line));

    while (std::getline(std::cin, line)) {
        assert(line.size() >= row_length);
        matrix.push_back(std::move(line));
    }
    const size_t column_length = matrix.size();
    assert(column_length >= 2);

    /*
     * Do the counts
     */
    CountSet count_set;
    count_down(matrix, count_set);
    count_up(matrix, count_set);
    count_right(matrix, count_set);
    count_left(matrix, count_set);

    size_t visible_trees = (
        // northern and southern edges
        (2 * row_length)
        // eastern and western edges, minus corners shared with north and south
        + (2 * (column_length - 2))
        + count_set.size()
    );

    std::cout << "Part 1: " << visible_trees << std::endl;
}
