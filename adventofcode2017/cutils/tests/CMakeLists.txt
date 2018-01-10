# Sample cmake
# Assuming a project structure:
#
# |-- CMakeLists.txt
# |-- src
# |   `-- source.c
# |-- include
#     `-- includes.h
cmake_minimum_required(VERSION 3.0.0)

# set project name
project(advent)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)   # artifact output dir
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)      # shared lib output dir

# set local includes/sources
include_directories("include" "../")    # includes
file(GLOB SOURCES "src/*.c" "../*.c")  # sources
add_executable(${PROJECT_NAME} ${SOURCES})  # output artifact

## Using pkg-config -- libnotify install via `sudo apt install libnotify-dev`
#find_package(PkgConfig REQUIRED)
#pkg_search_module(LIB_NOTIFY REQUIRED libnotify)
#target_link_libraries(${PROJECT_NAME} ${LIB_NOTIFY_LIBRARIES})
#target_include_directories(${PROJECT_NAME} PUBLIC ${LIB_NOTIFY_INCLUDE_DIRS})
#target_compile_options(${PROJECT_NAME} PUBLIC ${LIB_NOTIFY_CFLAGS_OTHER})

# List all compile flags here
set(_FLAGS "-Wall -Wextra -Werror -Warray-bounds -Wpedantic -std=c99")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${_FLAGS}")

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

