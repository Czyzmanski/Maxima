cmake_minimum_required(VERSION 3.10)

# container the project name
project(Maxima)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

# add the executable
add_executable(maxima_test src/maxima_example.cc)