project(mgunit)
set(VERSION_MAJOR "1")
set(VERSION_MINOR "3")
set(VERSION_PATCH "0")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

# required version of CMake
cmake_minimum_required(VERSION 2.8.3)
cmake_policy(SET CMP0017 NEW)

set(CMAKE_MODULE_PATH
  ${CMAKE_SOURCE_DIR}/mgcmake/Modules
  ${CMAKE_ROOT}/Modules
  ${CMAKE_MODULE_PATH}
)

find_package(Git)
if (GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} log -1 --pretty=format:%h
    OUTPUT_VARIABLE REVISION
  )
else ()
  set(REVISION "")
endif ()

find_package(IDL)

add_subdirectory(docs)
add_subdirectory(src)

file(GLOB_RECURSE PRO_FILES "src/*.pro")
file(GLOB_RECURSE IDLDOC_FILES "src/*.idldoc")

add_custom_command(
  OUTPUT api-docs/index.html
  COMMAND ${Idl_EXECUTABLE} -e mgunit_build_docs
  DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
)
add_custom_target(doc DEPENDS api-docs/index.html)

add_custom_command(
  OUTPUT api-userdocs/index.html
  COMMAND ${Idl_EXECUTABLE} -e mgunit_build_userdocs
  DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
)
add_custom_target(userdoc DEPENDS api-userdocs/index.html)

install(FILES README.rst COPYING.rst RELEASE.rst DESTINATION .)
install(SCRIPT mgunit_build.cmake)
install(FILES mgunit.sav DESTINATION lib)
