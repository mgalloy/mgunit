; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test
; runner. The `MGutCliRunner` displays the results in the output log or in a
; log file.
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
;   level : in, required, type=level
;     level of test suite
;-
pro mgutclirunner::reportTestSuiteStart, testsuite, $
                                         ntestcases=ntestcases, $
                                         ntests=ntests, $
                                         level=level
  compile_opt strictarr

  indent = level eq 0 ? '' : string(bytarr(level * self.indent) + self.space)
  self->_print, self.logLun, $
                indent + '"' + testsuite $
                  + '" test suite starting (' $
                  + strtrim(ntestcases, 2) + ' test suite' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + '/case' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + ', ' $
                  + strtrim(ntests, 2) + ' test' + (ntests eq 1 ? '' : 's') $
                  + ')', $
                /magenta
end


;+
; Create a string from an array of line numbers.
;
; :Private:
;
; :Returns:
;   string
;
; :Params:
;   lines : in, required, type=long/`lonarr`
;     array of line numbers or a scalar 0
;-
function mgutclirunner::_combineLines, lines
  compile_opt strictarr

  if (size(lines, /n_dimensions) eq 0) then return, ''

  f = '(%"%d")'
  if (n_elements(lines) eq 1L) then return, string(lines[0], format=f)

  diff = lines[1:-1] - lines[0:-2]
  r = string(lines[0], format=f)
  in_range = 0B
  for i = 0L, n_elements(diff) - 1L do begin
    if (diff[i] eq 1L) then begin
      if (~in_range) then r += '-'
      in_range = 1B
    endif else begin
      if (in_range) then r += string(lines[i], format=f)
      in_range = 0B
      r += ', ' + string(lines[i + 1], format=f)
    endelse
  endfor
  if (in_range) then r += string(lines[i], format=f)

  return, r
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
pro mgutclirunner::reportTestSuiteResult, npass=npass, nfail=nfail, $
                                          nskip=nskip, level=level, $
                                          total_nlines=total_nlines, $
                                          covered_nlines=covered_nlines, $
                                          testing_routines=testing_routines

  compile_opt strictarr

  coverage = ''
  if (mg_idlversion(require='8.4')) then begin
    if n_elements(testing_routines) gt 0 then begin
      ; collect the total lines in the testing_routings
      suite_total_nlines = 0L
      suite_covered_nlines = 0L
      indent = string(bytarr(level * self.indent) + self.space)
      for i = 0L, n_elements(testing_routines) - 1L do begin
        r = testing_routines[i]
        if (r.resolved) then begin
          untested_lines = code_coverage(r.name, $
                                         function=r.is_function, nlines=nlines)
          suite_total_nlines += nlines
          if (size(untested_lines, /n_dimensions) eq 0) then begin
            covered_nlines = nlines
            suite_covered_nlines += nlines
          endif else begin
            covered_nlines = nlines - n_elements(untested_lines)
            suite_covered_nlines += covered_nlines
          endelse
          untested_lines_st = self->_combineLines(untested_lines)
          
          self->_print, self.logLun, $
            string(indent, r.name, 100.0 * covered_nlines / nlines, untested_lines_st, $
            format='(%"%s\"%s\" coverage: %0.1f\%, untested lines: %s")')
        endif
      endfor
      
      coverage = string(100.0 * suite_covered_nlines / suite_total_nlines, $
        format='(%" (%0.1f\% coverage)")')

    endif else if total_nlines gt 0L then begin
      coverage = string(100.0 * covered_nlines / total_nlines, $
                        format='(%" (%0.1f\% coverage)")')
    endif
  endif

  format = '(%"%sResults: %d / %d tests passed, %d skipped%s")'
  indent = level eq 0 ? '' : string(bytarr(level * self.indent) + self.space)
  self->_print, self.logLun, $
                string(indent, npass, npass + nfail, nskip, coverage, $
                       format=format), $
                /magenta
end


;+
; Report a test case has begun.
;
; :Params:
;    testcase : in, required, type=string
;       name of test case
;
; :Keywords:
;   ntests : in, required, type=integer
;     number of tests contained in this test case
;   level : in, required, type=level
;     level of test case
;-
pro mgutclirunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  indent = string(bytarr(level * self.indent) + self.space)
  self->_print, self.logLun, $
                indent + '"' + testcase + '" test case starting' $
                  + ' (' + strtrim(ntests, 2) $
                  + ' test' + (ntests eq 1 ? '' : 's') + ')', $
                /blue 
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
pro mgutclirunner::reportTestCaseResult, npass=npass, nfail=nfail, $
                                         nskip=nskip, level=level
  compile_opt strictarr

  format = '(%"%sResults: %d / %d tests passed, %d skipped")'
  indent = string(bytarr(level * self.indent) + self.space)
  self->_print, self.logLun, $
                string(indent, npass, npass + nfail, nskip, format=format), $
                /blue
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
pro mgutclirunner::reportTestCaseCoverage, covered_routines, $
                                           tested_routines, $
                                           level=level, $
                                           total_nlines=total_nlines, $
                                           covered_nlines=covered_nlines
  compile_opt strictarr

  indent = string(bytarr(level * self.indent) + self.space)
  single_indent = string(bytarr(self.indent) + self.space)
  self->_print, self.logLun, $
                string(indent, $
                       100.0 * covered_nlines / total_nlines, $
                       format='(%"%sTest coverage: %0.1f\%")')
  ind = where(tested_routines.untested_lines ne '', n_untested_routines)
  if (n_untested_routines gt 0L) then begin
    self->_print, self.logLun, $
                  string(indent, single_indent, 'Untested lines', $
                         format='(%"%s%s%s")')
  endif
  for i = 0L, n_elements(tested_routines) - 1L do begin
    if (tested_routines[i].untested_lines ne '') then begin
      self->_print, self.logLun, $
                    string(indent, $
                           single_indent, $
                           single_indent, $
                           tested_routines[i].name, $
                           tested_routines[i].untested_lines, $
                           format='(%"%s%s%s%s: lines %s")')
    endif
  endfor
  if (n_elements(covered_routines) gt 0L) then begin
    self->_print, self.logLun, $
                  string(indent, single_indent, 'Completely covered routines', $
                         format='(%"%s%s%s")')
    self->_print, self.logLun, $
                  string(indent, $
                         single_indent, $
                         single_indent, $
                         strjoin(strtrim(covered_routines, 2), ', '), $
                         format='(%"%s%s%s%s")')
  endif
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
pro mgutclirunner::reportTestStart, testname, level=level
  compile_opt strictarr

  indent = string(bytarr((level + 1L) * self.indent) + self.space)
  self->_print, self.logLun, indent + testname + ': ', format='(A, $)'
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
pro mgutclirunner::reportTestResult, msg, passed=passed, $
                                     output=output, time=time, $
                                     skipped=skipped, level=level, $
                                     math_errors=math_errors
  compile_opt strictarr

  if (skipped) then begin
    self->_print, self.logLun, $
                  'skipped' + (msg eq '' ? '' : ' "' + msg + '"'), $
                  /cyan, $
                  format='(A, $)'
  endif else if (passed) then begin
    self->_print, self.logLun, $
                  'passed', $
                  /green, $
                  format='(A, $)'
  endif else begin
    self->_print, self.logLun, $
                  'failed' + (msg eq '' ? '' : ' "' + msg + '"'), $
                  /red, $
                  format='(A, $)'
  endelse

  if (n_elements(math_errors) gt 0L && (math_errors gt 0L)) then begin
    self->_print, self.logLun, $
                  ' (' + mg_math_message(math_errors) + ')', $
                  /yellow, $
                  format='(A, $)'
  endif

  if (size(output, /type) eq 7 && output ne '') then begin
    self->_print, self.logLun, ' [' + output + ']', format='(A, $)'
  endif
  self->_print, self.logLun, string(time, format='(%" (%f seconds)")')
end


;= helper methods

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
;     keywords to `MG_ANSICODE`, i.e., `RED` or `GREEN`
;-
pro mgutclirunner::_print, lun, text, _extra=e
  compile_opt strictarr
  
  if (self.isTty) then begin
    printf, lun, mg_ansicode(text, _extra=e), _extra=e
  endif else begin
    printf, lun, text, _extra=e
  endelse
  
  if (lun gt 0L) then flush, lun
end


;+
; Safe method of determining if the current terminal is TTY.
;
; :Returns:
;   1 if the terminal is TTY, 0 if not
;-
function mgutclirunner::_findIfTty
  compile_opt strictarr
  
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, 0
  endif
  
  return, mg_termIsTty()
end


;= lifecycle methods

;+
; Free resources.
;-
pro mgutclirunner::cleanup
  compile_opt strictarr

  if (self.logLun gt 0) then free_lun, self.logLun
  self->mguttestrunner::cleanup
  !quiet = 0
end


;+
; Initialize the test runner.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   filename : in, optional, type=string
;     if present, output is sent to that file, otherwise output is sent to
;     `stdout`
;   color : in, optional, type=boolean
;     set to print color output
;-
function mgutclirunner::init, filename=filename, color=color, _extra=e
  compile_opt strictarr

  if (~self->mguttestrunner::init(_extra=e)) then return, 0B

  if (n_elements(filename) gt 0) then begin
    logDir = file_dirname(filename)
    if (~file_test(logDir)) then file_mkdir, logDir
  endif

  if (n_elements(filename) gt 0 && strlen(filename) gt 0L) then begin
    openw, logLun, filename, /get_lun
    self.logLun = logLun
  endif else begin
    self.logLun = -1L
  endelse

  self.indent = 2L
  self.space = (byte(' '))[0]
  self.isTty = n_elements(color) gt 0L $
                 ? keyword_set(color) $
                 : (self.logLun lt 0L && self->_findIfTty())

  !quiet = 1
  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;   logLun
;     the logical unit number to send output to (-1L by default)
;   indent
;     number of spaces a single indent should be
;   space
;     byte value of the space character
;   isTty
;     whether the `mgunit` believes it is running in a TTY terminal or not
;-
pro mgutclirunner__define
  compile_opt strictarr

  define = { MGutCliRunner, inherits MGutTestRunner, $
             logLun : 0L, $
             indent : 0L, $
             space : 0B, $
             isTty: 0B $
           }
end
