project(mgunit)
set(VERSION_MAJOR "1")
set(VERSION_MINOR "4")
set(VERSION_PATCH "0")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
SET(CMAKE_BUILD_TYPE Release)

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
find_package(IDLdoc)

add_subdirectory(docs)
add_subdirectory(src)

file(GLOB_RECURSE PRO_FILES "src/*.pro")
file(GLOB_RECURSE IDLDOC_FILES "src/*.idldoc")

if (IDLdoc_FOUND)
  add_custom_command(
    OUTPUT api-docs/index.html
    COMMAND ${IDL_EXECUTABLE} -quiet -IDL_QUIET 1 -IDL_STARTUP \"\" -IDL_PATH \"+${CMAKE_CURRENT_SOURCE_DIR}${IDL_PATH_SEP}+${IDLdoc_DIR}${IDL_PATH_SEP}<IDL_DEFAULT>\" -e mgunit_build_docs
    DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
  )
  add_custom_target(doc DEPENDS api-docs/index.html)
  
  add_custom_command(
    OUTPUT api-userdocs/index.html
    COMMAND ${IDL_EXECUTABLE} -quiet -IDL_QUIET 1 -IDL_STARTUP \"\" -IDL_PATH \"+${CMAKE_CURRENT_SOURCE_DIR}${IDL_PATH_SEP}+${IDLdoc_DIR}${IDL_PATH_SEP}<IDL_DEFAULT>\" -e mgunit_build_userdocs
    DEPENDS ${PRO_FILES} ${IDLDOC_FILES}
  )
  add_custom_target(userdoc ALL DEPENDS api-userdocs/index.html)
endif ()

set_directory_properties(PROPERTIES
  ADDITIONAL_MAKE_CLEAN_FILES "api-userdocs;api-docs;mgunit.sav"
)

add_custom_command(
  OUTPUT mgunit.sav
  COMMAND ${IDL_EXECUTABLE} -quiet -IDL_QUIET 1 -IDL_STARTUP \"\" -IDL_PATH \"+${CMAKE_CURRENT_SOURCE_DIR}${IDL_PATH_SEP}<IDL_DEFAULT>\" mgunit_build
  DEPENDS ${PROFILES})
add_custom_target(savfile ALL DEPENDS mgunit.sav)

# setup installer

if (WIN32)
  set(CPACK_GENERATOR ZIP)
else ()
  set(CPACK_GENERATOR TGZ)
endif ()

set(MGUNIT_PACKAGE_VERSION "${VERSION}")
set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-MGUNIT_PACKAGE_VERSION")

include(CPack)

# script to build mgunit.sav only at "make install"
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/mgunit.sav DESTINATION lib)

install(FILES README.rst COPYING.rst RELEASE.rst DESTINATION .)
if (IDLdoc_FOUND)
  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/api-userdocs DESTINATION .)
endif ()