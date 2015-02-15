; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test
; runner. Each subclass of MGutTestRunner displays them in some way.
; MGutTestRunner itself is abstract and shouldn't be instantiated.
;
; :Private:
;-

;= MGutTestRunner interface

;+
; Report a test suite has begun.
;
; :Params:
;    testsuite : in, required, type=string
;       name of test suite
;
; :Keywords:
;   ntestcases : in, required, type=integer
;     number of test suites/cases contained by the test suite
;   ntests : in, required, type=integer
;     number of tests contained in the hierarchy below this test suite
;   level : in, required, type=level
;     level of test suite
;-
pro mguttestrunner::reportTestSuiteStart, testsuite, $
                                          ntestcases=ntestcases, $
                                          ntests=ntests, $
                                          level=level
  compile_opt strictarr
end


;+
; Report the results of a test suite.
;
; :Keywords:
;   npass : in, required, type=integer
;     number of passing tests contained in the hierarchy below the test suite
;   nfail : in, required, type=integer
;     number of failing tests contained in the hierarchy below the test suite
;   nskip : in, required, type=integer
;     number of skipped tests contained in the hierarchy below the test suite
;   level : in, required, type=integer
;     level of test suite
;   total_nlines : in, required, type=long
;     total number of lines in testing routines
;   covered_nlines : in, required, type=long
;     number of lines covered in testing routines
;   testing_routines : in, required, type=array
;     array of testing routines defined at the suite level
;-
pro mguttestrunner::reportTestSuiteResult, npass=npass, nfail=nfail, $
                                           nskip=nskip, level=level, $
                                           total_nlines=total_nlines, $
                                           covered_nlines=covered_nlines, $
                                           testing_routines=testing_routines
  compile_opt strictarr

end


;+
; Report a test case has begun.
;
; :Params:
;   testcase : in, required, type=string
;     name of test case
;
; :Keywords:
;   ntests : in, required, type=integer
;     number of tests contained in this test case
;   level : in, required, type=level
;     level of test case
;-
pro mguttestrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

end


;+
; Report the results of a test case.
;
; :Keywords:
;   npass : in, required, type=integer
;     number of passing tests
;   nfail : in, required, type=integer
;     number of failing tests
;   nskip : in, required, type=integer
;     number of skipped tests
;   level : in, required, type=integer
;     level of test case
;-
pro mguttestrunner::reportTestCaseResult, npass=npass, nfail=nfail, $
                                          nskip=nskip, level=level
  compile_opt strictarr

end


;+
; Report the test coverage of a test case.
;
; :Params:
;   covered_routines : in, required, type=strarr
;     string array of routines completely covered or `!null` if no routine
;     was completely covered
;   tested_routines : in, required, type=array of structures
;     array of structures of all tested routines of the form::
;
;       { name: '', is_function: 0B, untested_lines: '' }
;
; :Keywords:
;   level : in, required, type=integer
;     level of test case
;   total_nlines : in, required, type=long
;     total number of lines in testing routines
;   covered_nlines : in, required, type=long
;     number of lines covered in testing routines
;-
pro mguttestrunner::reportTestCaseCoverage, covered_routines, tested_routines, $
                                            level=level, $
                                            total_nlines=total_nlines, $
                                            covered_nlines=covered_nlines
  compile_opt strictarr

end


;+
; Report the start of single test.
;
; :Params:
;   testname : in, required, type=string
;     name of test
;
; :Keywords:
;   level : in, required, type=integer
;     level of test case
;-
pro mguttestrunner::reportTestStart, testname, level=level
  compile_opt strictarr

end


;+
; Report the result of a single test.
;
; :Params:
;   msg : in, required, type=string
;     message to display when test fails
;
; :Keywords:
;   passed : in, required, type=boolean
;     whether the test passed
;   output : in, optional, type=string
;     output from the test run
;   time : in, required, type=float
;     time for the test to run
;   level : in, required, type=integer
;     level of test case
;   skipped : in, required, type=boolean
;     indicates whether the test should be counted in the results
;   math_errors : out, optional, type=integer
;     bitmask of `CHECK_MATH` return values
;-
pro mguttestrunner::reportTestResult, msg, passed=passed, $
                                      output=output, time=time, $
                                      skipped=skipped, level=level, $
                                      math_errors=math_errors
  compile_opt strictarr

end


;= lifecycle methods

;+
; Free resources.
;-
pro mguttestrunner::cleanup
  compile_opt strictarr

  if (obj_valid(self.parent)) then obj_destroy, self.parent
  if (obj_valid(self.suite)) then obj_destroy, self.suite
end


;+
; Initialize the test runner.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   parent : in, optional, type=object
;     parent compound test runner
;   test_suite : in, optional, type=object
;     test suite object
;
;-
function mguttestrunner::init, parent=parent, test_suite=testSuite
  compile_opt strictarr

  self.suite = obj_valid(testsuite) ? testsuite : obj_new()
  self.parent = obj_valid(parent) ? parent : obj_new()

  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;   suite
;     suite of tests the runner will run
;   parent
;     parent compound test, if present
;-
pro mguttestrunner__define
  compile_opt strictarr

  define = { MGutTestRunner, $
             suite: obj_new(), $
             parent: obj_new() $
           }
end
