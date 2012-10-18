project(mgunit)
set(VERSION_MAJOR "1")
set(VERSION_MINOR "3")
set(VERSION_PATCH "0")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

find_package(Git)
if (GIT_FOUND)
  execute_process(
    COMMAND git log -1 --pretty=format:%h
    OUTPUT_VARIABLE REVISION
  )
else ()
  set(REVISION "")
endif ()

# required version of CMake
cmake_minimum_required(VERSION 2.8.3)

add_subdirectory(docs)
#add_subdirectory(lib)
add_subdirectory(src)

file(GLOB_RECURSE PRO_FILES "src/*.pro")
file(GLOB_RECURSE IDLDOC_FILES "src/*.idldoc")

add_custom_command(
  OUTPUT api-docs/index.html
  COMMAND idl -e mgunit_build_docs
  DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
)
add_custom_target(doc DEPENDS api-docs/index.html)

add_custom_command(
  OUTPUT api-docs/index.html
  COMMAND idl -e mgunit_build_userdocs
  DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
)
add_custom_target(doc DEPENDS api-userdocs/index.html)
