Using mgunit
============

.. sectnum::
   :depth: 3

:Author: Michael Galloy


Introduction
------------

mgunit is a unit testing framework for IDL modeled on other popular frameworks such as JUnit. The goal is to allow easy creation of tests and simple, clear reporting of results of the tests, while still allowing for many different testing situations. Simple naming conventions replace formal creation of hierarchies and specification of tests. This allows test suites to be created with a minimum of overhead so that the developer can focus on the actual code of the tests themselves.

Developers using mgunit need to be familiar with the basic syntax for object-oriented programming in IDL.


Basic use
---------

Individual tests are methods of a class that subclasses `MGutTestCase`. Each method returns `1` for success or `0` (or throws an error) for failure. Each test method's name must start with "test". 

Let's look at some of the tests for routines in my library. For example, the `MG_EVALEXPR` routine evaluates a mathematical expression specified as a string, reporting the error status of parsing the expression. Let's write some tests to verify it's functionality. First, subclass `MGutTestCase` like below::

  pro mg_evalexpr_ut__define
    compile_opt strictarr
  
    define = { mg_evalexpr_ut, inherits MGutTestCase }
  end

A test is just a method of this class whose name starts with "test". The mgunit framework will find the tests automatically. For example, a simple test::

  function mg_evalexpr_ut::test_basic
    compile_opt strictarr
  
    result = mg_evalexpr('1 + 2', error=error)
    assert, error eq 0, 'incorrect error status: %d', error
    assert, result eq 3, 'incorrect result: %d', result
  
    return, 1
  end

Tests return `1` for success. For failure, they either return `0` or throw an error. Here the helper routine `ASSERT` will throw an error using the given message if its condition is not met. This will be reported as a failure along with the message. To run this test, do the following::

  IDL> mgunit, 'mg_evalexpr_ut.test_basic'
  "All tests" test suite starting (1 test suite/case, 1 test)
     "mg_evalexpr_ut" test case starting (1 test)
        test_basic: passed (0.000314 seconds)
     Results: 1 / 1 tests passed, 0 skipped
  Results: 1 / 1 tests passed, 0 skipped

There are actually several tests in `mg_evalexpr_ut__define.pro`; to run them all do::

  IDL> mgunit, 'mg_evalexpr_ut'
  "All tests" test suite starting (1 test suite/case, 8 tests)
     "mg_evalexpr_ut" test case starting (8 tests)
        test_basic: passed (0.057548 seconds)
        test_error1: passed (0.000403 seconds)
        test_function1: passed (0.001067 seconds)
        test_order1: passed (0.000455 seconds)
        test_order2: passed (0.000817 seconds)
        test_order3: passed (0.001468 seconds)
        test_subshash: passed (0.046330 seconds)
        test_subsstruct: passed (0.000618 seconds)
     Results: 8 / 8 tests passed, 0 skipped
  Results: 8 / 8 tests passed, 0 skipped

A test case may have as many individual tests (methods with names starting with "test") as necessary. 


Running multiple test cases
---------------------------

To run multiple test cases, you can pass an array of test case names to `MGUNIT`::

  IDL> mgunit, ['mg_evalexpr_ut', 'mg_linear_function_ut']
  "All tests" test suite starting (2 test suites/cases, 9 tests)
     "mg_evalexpr_ut" test case starting (8 tests)
        test_basic: passed (0.006440 seconds)
        test_error1: passed (0.000407 seconds)
        test_function1: passed (0.001036 seconds)
        test_order1: passed (0.000439 seconds)
        test_order2: passed (0.000817 seconds)
        test_order3: passed (0.001452 seconds)
        test_subshash: passed (0.004932 seconds)
        test_subsstruct: passed (0.000507 seconds)
     Results: 8 / 8 tests passed, 0 skipped
     "mg_linear_function_ut" test case starting (1 test)
        test_basic: passed (0.017841 seconds)
     Results: 1 / 1 tests passed, 0 skipped
  Results: 9 / 9 tests passed, 0 skipped

Typically, you will have a directory (or directory hierarchy) of test cases to run. To automatically group all the tests in a directory hierarchy in one "test suite", create a subclass of `MGutTestSuite`. For example, my library tests are grouped into the a test suite by `mglib_uts__define.pro`::

  function mglib_uts::init, _extra=e
    compile_opt strictarr

    if (~self->mguttestsuite::init(_strict_extra=e)) then return, 0

    self->add, /all

    return, 1
  end

  pro mglib_uts__define
    compile_opt strictarr

    define = { mglib_uts, inherits MGutTestSuite }
  end

For the automatic collection of tests to work, the classnames for all the test cases must end in "ut", e.g., `mg_evalexpr_ut`. The test suite definition file is placed in the root directory for all the files for the test cases. To run all the test cases, just do::

  IDL> mgunit, `mglib_uts`


Fixtures
--------

The `setup` and `teardown` methods of a test case class are executed before and after each individual test. By default, they are empty, but subclasses of `MGutTestCase` can override them to do any common setup/teardown tasks. Any data to be stored from the setup is normally saved as an instance variable of the test case class so that it can be accessed by the test and the `teardown` method.

Pointer and object memory leaks can be tested for using fixtures by comparing the number of current pointers and objects during setup and teardown. For an example of doing this, see the code for the `MGutLibTestCase` class in `mgutlibtestcase__define.pro`. Note that the example tests provided actually subclass from `MGutLibTestCase` to provide memory leak checking.


Invalid tests
-------------

Sometimes there are tests that are only valid to run in certain circumstances. The `ASSERT` routine can be used to stop a test from counting in the final tally of passes and failures by using the `SKIP` keyword. For example, if you only want a test to run if running under IDL 8.0 or later, put the following at the beginning of the test::

  assert, long((strsplit(!version.release, '.', /extract))[0]) ge 8, $
          'IDL version too old: %s', !version.release, $
          /skip

For IDL versions before 8.0, tests including the above will not immediately stop and not be counted in the final count of passed/failed tests.


Crashes in a test
-----------------

Normally, a runtime error in a test will cause the test to fail. But sometimes routines are supposed to crash on invalid input. To test those cases, i.e., where invalid inputs are purposefully fed into a routine to cause a failure, use the `CATCH` block defined in the batch file `error_is_pass.pro`. For example, the following makes sure that the `FINDGEN` routine fails when given a string argument::

  function my_routine_ut::test_basic
    compile_opt strictarr
    @error_is_pass
    
    a = findgen('a string')
    
    return, 1 
  end

By default, runtime errors will cause a test to fail, but IO errors will not. Use the `CATCH` block present in `error_is_fail.pro` to make an IO error cause the test to fail also.


Alternative output
------------------

Results can be sent to a log file with the `FILENAME` keyword::

  IDL> mgunit, 'mglib_uts', filename='test-results.log'

This will send the normal output to the `results.log` file.

HTML output can also be created with the boolean `HTML` keyword to the `MGUNIT` routine. Generally, the `FILENAME` keyword is used in conjunction with this option::

  IDL> mgunit, 'mglib_uts', filename='test-results.html', /html


Test templates
--------------

Templates for the IDL Workbench are provided to make test/suite creation even faster. To use them, first navigate to the Workbench preferences. There should be a Templates section under the IDL heading. Click the "Import" button on the right and navigate to the `test-templates.xml` file in the mgunit source. Two new templates, "Test case" and "Test suite", should now be available. Typing "testcase" into a new file and then selecting *Edit > Content Assist* from the menus will create a test case which can be filled out like a form. Suites can be created the same way by typing "testsuite".


Tips
----

It can be useful to create a subclass of `MGutTestCase` for a project so that each test case in the project inherits from that class instead of directly from `MGutTestCase`. This parent test case can do work common to all the tests, i.e., find the location of test data, have common setup/teardown methods, etc. The `MGutLibTestCase` class discussed in the Fixtures section is used in this manner.

The `NTESTS`, `NPASS`, and `NFAIL` keywords to the `MGUNIT` routine output the appropriate values. These can be handy for automated scripts, i.e., sending email if any test fails, etc.

Unit testing is a large topic, with a large collection of literature covering it. [Frameworks]_ covers the theory and background of unit test frameworks and offers step-by-step instruction in basic unit test development; examples are in Java and C++. [DesignDrivenTesting]_ argues that testing should verify a design instead of pretending that unit tests are a replacement for design. [Patterns]_ contains a catalog of proven patterns and how to use them to solve common testing problems.


.. [Frameworks] Hamill, Paul. *Unit Test Frameworks.* O'Reilly Media, 2004.

.. [Patterns] Gerard Meszaros. *Unit Test Patterns: Refactoring Test Code.* Addison-Wesley, 2007.

.. [DesignDrivenTesting] Stephens, Matt and Doug Rosenberg. *Design Driven Testing: Test Smarter, Not Harder.* Apress, 2010.
