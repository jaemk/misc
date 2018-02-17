# Sample cmake
# Assuming a project structure:
#
# |-- CMakeLists.txt
# |-- src
# |   `-- source.cpp
# |-- include
#     `-- includes.h
cmake_minimum_required(VERSION 3.0.0)

# set project name
project(advent)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)   # artifact output dir
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)      # shared lib output dir

# set local includes/sources
include_directories("include")    # includes
file(GLOB SOURCES "src/*.cpp" "include/*/*.cpp")  # sources
add_executable(${PROJECT_NAME} ${SOURCES})  # output artifact

## Find and link boost libs
set(Boost_USE_STATIC_LIBS ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_RUNTIME OFF)
find_package(Boost 1.60.0 REQUIRED COMPONENTS)        # list all boost libs here
#find_package(Boost 1.60.0 REQUIRED COMPONENTS regex)        # list all boost libs here
include_directories(${Boost_INCLUDE_DIRS})                  # include boost things
target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})   # link boost things

## Using pkg-config -- libnotify install via `sudo apt install libnotify-dev`
#find_package(PkgConfig REQUIRED)
#pkg_search_module(LIB_NOTIFY REQUIRED libnotify)
#target_link_libraries(${PROJECT_NAME} ${LIB_NOTIFY_LIBRARIES})
#target_include_directories(${PROJECT_NAME} PUBLIC ${LIB_NOTIFY_INCLUDE_DIRS})
#target_compile_options(${PROJECT_NAME} PUBLIC ${LIB_NOTIFY_CFLAGS_OTHER})

# List all compile flags here
set(_FLAGS "-Wall -Wextra -Werror -Wpedantic -std=c++11 -O3 -march=native")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${_FLAGS}")

# spit out a `compile_commands.json` file for ycm completions
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# Add command & target to run the built artifact.
# The custom-target depends on the custom-command which
# runs the artifact with `make run`.
add_custom_command(
    OUTPUT .run.bin
    COMMAND ${PROJECT_NAME}
    COMMENT "Running artifact"
)
add_custom_target(
    run
    DEPENDS .run.bin
)
add_custom_command(
    OUTPUT .leak.bin
    COMMAND valgrind --leak-check=yes --track-origins=yes ${EXECUTABLE_OUTPUT_PATH}/${PROJECT_NAME}
    COMMENT "Leak checking artifact"
)
add_custom_target(
    leak
    DEPENDS .leak.bin
)

