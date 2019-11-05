cmake_minimum_required(VERSION 2.8.4)
project(Eidomatica)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}" ${CMAKE_MODULE_PATH})
add_custom_target(
    version
	ALL
    COMMAND /bin/bash version.sh
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
find_package(Mathematica)


install(DIRECTORY Eidomatica DESTINATION "${Mathematica_USERBASE_DIR}/Applications/")

