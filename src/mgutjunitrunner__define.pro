; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test
; runner. The `MGutJUnitRunner` displays the results in the output XML file.
; For example, a test that normally runs like::
;
;   IDL> mgunit, 'gpuradon_ut'
;   "All tests" test suite starting (1 test suite/case, 2 tests)
;      "gpuradon_ut" test case starting (2 tests)
;         test_basic_backprojection: passed (0.158287 seconds)
;         test_basic_forward: failed "incorrect result" (0.005014 seconds)
;      Results: 1 / 2 tests passed, 0 skipped
;   Results: 1 / 2 tests passed, 0 skipped
;
; With `MGutJUnitRunner`, it would produce output like::
;
;   IDL> mgunit, 'gpuradon_ut', /junit
;   <testsuites name="All tests">
;     <testsuite name="gpuradon_ut">
;       <testcase name="test_basic_backprojection">
;       </testcase>
;       <testcase name="test_basic_forward">
;         <failure>incorrect result</failure>
;       </testcase>
;     </testsuite>
;   </testsuites>
;
; :Private:
;-

;= MGutTestRunner interface

;+
; Report a test suite has begun.
;
; :Params:
;   testsuite : in, required, type=string
;     name of test suite
;
; :Keywords:
;   ntestcases : in, required, type=integer
;     number of test suites/cases contained by the test suite
;   ntests : in, required, type=integer
;     number of tests contained in the hierarchy below this test suite
;   level : in, required, type=integer
;     level of test suite
;-
pro mgutjunitrunner::reportTestSuiteStart, testsuite, $
                                           ntestcases=ntestcases, $
                                           ntests=ntests, $
                                           level=level
  compile_opt strictarr
  
  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)
  tagname = level eq 0L ? 'testsuites' : 'testsuite'
  msg = string(indent, tagname, testsuite, format='(%"%s<%s name=\"%s\">")')
  self->_print, self.lun, msg
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
pro mgutjunitrunner::reportTestSuiteResult, npass=npass, nfail=nfail, $
                                            nskip=nskip, level=level, $
                                            total_nlines=total_nlines, $
                                            covered_nlines=covered_nlines, $
                                            testing_routines=testing_routines
  compile_opt strictarr

  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)
  tagname = level eq 0L ? 'testsuites' : 'testsuite'
  msg = string(indent, tagname, format='(%"%s</%s>")')
  self->_print, self.lun, msg
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
;   level : in, required, type=integer
;     level of test case
;-
pro mgutjunitrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)
  msg = string(indent, testcase, format='(%"%s<testsuite name=\"%s\">")')
  self->_print, self.lun, msg
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
pro mgutjunitrunner::reportTestCaseResult, npass=npass, nfail=nfail, $
                                           nskip=nskip, level=level
  compile_opt strictarr

  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)
  msg = string(indent, format='(%"%s</testsuite>")')
  self->_print, self.lun, msg
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
pro mgutjunitrunner::reportTestStart, testname, level=level
  compile_opt strictarr

  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)
  msg = string(indent, testname, format='(%"%s  <testcase name=\"%s\">")')
  self->_print, self.lun, msg
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
pro mgutjunitrunner::reportTestResult, msg, passed=passed, $
                                       output=output, time=time, $
                                       skipped=skipped, level=level, $
                                       math_errors=math_errors
  compile_opt strictarr

  indent = level eq 0L ? '' : string(bytarr(2 * level) + 32B)

  if (skipped) then begin
    _msg = string(indent, msg, format='(%"%s    <failure>skipped: %s</failure>")')
    self->_print, self.lun, _msg    
  endif else if (~keyword_set(passed)) then begin
    _msg = string(indent, msg, format='(%"%s    <failure>%s</failure>")')
    self->_print, self.lun, _msg
  endif

  self->_print, self.lun, string(indent, format='(%"%s  </testcase>")')
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
pro mgutjunitrunner::reportTestCaseCoverage, covered_routines, tested_routines, $
                                             level=level, $
                                             total_nlines=total_nlines, $
                                             covered_nlines=covered_nlines
  compile_opt strictarr

  ; TODO: implement
end


;+
; Prints a message to a LUN.
;
; :Params:
;   lun : in, required, type=long
;     logical unit number to print to
;   text : in, required, type=string
;     text to print
;
; :Keywords:
;   _extra : in, optional, type=keywords
;     keywords to `PRINTF`
;-
pro mgutjunitrunner::_print, lun, text, _extra=e
  compile_opt strictarr

  printf, lun, text, _extra=e
  if (lun gt 0L) then flush, lun
end


;= lifecycle methods

;+
; Free resources.
;-
pro mgutjunitrunner::cleanup
  compile_opt strictarr

  if (self.lun gt 0) then free_lun, self.lun
  self->mguttestrunner::cleanup
end


;+
; Initialize the test runner.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   filename : in, optional, type=string
;     if present, output is sent that file, otherwise output is sent to
;     `stdout`
;   color : in, optional, type=boolean
;     unused
;   _extra : in, optional, type=keywords
;     keywords to `MGutTestRunner::init`
;-
function mgutjunitrunner::init, filename=filename, color=color, _extra=e
  compile_opt strictarr

  if (~self->mguttestrunner::init(_extra=e)) then return, 0B

  ; make the directory the output file is in if it doesn't exist
  if (n_elements(filename) gt 0) then begin
    dir = file_dirname(filename)
    if (~file_test(dir)) then file_mkdir, dir
  endif

  ; setup the LUN for the output
  if (n_elements(filename) gt 0) then begin
    openw, lun, filename, /get_lun
    self.lun = lun
  endif else begin
    self.lun = -1L
  endelse

  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;   lun
;     the logical unit number to send output to (-1L by default)
;-
pro mgutjunitrunner__define
  compile_opt strictarr
  
  define = { MGutJUnitRunner, inherits MGutTestRunner, $
             lun: 0L $
           }
end