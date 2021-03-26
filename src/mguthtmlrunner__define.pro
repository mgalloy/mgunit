; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test
; runner. The `MGutHTMLRunner` displays the results in the output HTML file.
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
pro mguthtmlrunner::reportTestSuiteStart, testsuite, $
                                          ntestcases=ntestcases, $
                                          ntests=ntests, $
                                          level=level
  compile_opt strictarr

  self->_print, self.lun, $
                '<ul class="testsuite"><li><span class="suitename">' $
                  + testsuite $
                  + '</span> test suite starting (' $
                  + strtrim(ntestcases, 2) + ' test suite' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + '/case' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + ', ' $
                  + strtrim(ntests, 2) + ' test' + (ntests eq 1 ? '' : 's') $
                  + ')</li>'
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
function mguthtmlrunner::_combineLines, lines
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
pro mguthtmlrunner::reportTestSuiteResult, npass=npass, nfail=nfail, $
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

  format = '(%"<span class=\"results\">Results: %d / %d tests passed, %d skipped%s</span></ul>")'
  self->_print, self.lun, string(npass, npass + nfail, nskip, coverage, format=format)
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
pro mguthtmlrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  self->_print, self.lun, $
                '<ul class="testcase"><li><span class="casename">' + testcase $
                  + '</span> test case starting (' + strtrim(ntests, 2) $
                  + ' test' + (ntests eq 1 ? '' : 's') + ')</li>'
  self->_print, self.lun, '<ol>'
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
pro mguthtmlrunner::reportTestCaseResult, npass=npass, nfail=nfail, $
                                          nskip=nskip, level=level
  compile_opt strictarr

  format = '(%"<span class=\"results\">Results: %d / %d tests passed, %d skipped</span></ul>")'
  self->_print, self.lun, '</ol>'
  self->_print, self.lun, string(npass, npass + nfail, nskip, format=format)
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
pro mguthtmlrunner::reportTestStart, testname, level=level
  compile_opt strictarr

  self->_print, self.lun, '<li>' + testname + ': ', format='(A, $)'
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
pro mguthtmlrunner::reportTestResult, msg, passed=passed, $
                                      output=output, time=time, $
                                      skipped=skipped, level=level, $
                                      math_errors=math_errors
  compile_opt strictarr

  result = skipped $
             ? 'skipped' $
             : (keyword_set(passed) ? 'passed' : 'failed')

  s = string(result, $
             result, $
             (passed && ~skipped) ? '': (msg eq '' ? '' : ' "' + msg + '"'), $
             (size(output, /type) eq 7 && output ne '') ? ('[' + output + ']') : '', $
             time, $
             format='(%"<span class=\"%s\">%s%s</span> %s <span class=\"time\">%f seconds</span></li>")')

  self->_print, self.lun, s
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
pro mguthtmlrunner::reportTestCaseCoverage, covered_routines, tested_routines, $
                                            level=level, $
                                            total_nlines=total_nlines, $
                                            covered_nlines=covered_nlines
  compile_opt strictarr

  self->_print, self.lun, $
                string(100.0 * covered_nlines / total_nlines, $
                       format='(%"<div class=\"coverage\"><h1>Test coverage: %0.1f\%</h1>")')
  ind = where(tested_routines.untested_lines ne '', n_untested_routines)
  if (n_untested_routines gt 0L) then begin
    self->_print, self.lun, $
                  string('<h2>Untested lines</h2><ul>', $
                         format='(%"%s")')
  endif
  for i = 0L, n_elements(tested_routines) - 1L do begin
    if (tested_routines[i].untested_lines ne '') then begin
      self->_print, self.lun, $
                    string(tested_routines[i].name, $
                           tested_routines[i].untested_lines, $
                           format='(%"<li>%s: lines %s</li>")')
    endif
  endfor
  if (n_untested_routines gt 0L) then begin
    self->_print, self.lun, '</ul>'
  endif
  if (n_elements(covered_routines) gt 0L) then begin
    self->_print, self.lun, $
                  string('<h2>Completely covered routines</h2>', $
                         format='(%"%s")')
    self->_print, self.lun, $
                  string(strjoin(strtrim(covered_routines, 2), ', '), $
                         format='(%"<ul><li>%s</li></ul>")')
  endif
  self->_print, self.lun, '</div>'
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
;     keywords to MG_ANSICODE i.e. RED or GREEN
;-
pro mguthtmlrunner::_print, lun, text, _extra=e
  compile_opt strictarr
  
  printf, lun, text, _extra=e
  if (lun gt 0L) then flush, lun
end


;= lifecycle methods

;+
; Free resources.
;-
pro mguthtmlrunner::cleanup
  compile_opt strictarr

  self->_print, self.lun, '<span id="dateline">Test results from ' + systime() + '</span>'
  self->_print, self.lun, '</body></html>'
  
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
;     if present, output is sent that file, otherwise output is sent to `stdout`
;   color : in, optional, type=boolean
;     unused for MGutHtmlRunner
;   _extra : in, optional, type=keywords
;     keywords to `MGutTestRunner::init`
;-
function mguthtmlrunner::init, filename=filename, color=color, _extra=e
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

  self->_print, self.lun, '<html><head>'
  self->_print, self.lun, '<title>Test results</title>'
  self->_print, self.lun, '<style type="text/css" media="all">'

  styleFilename = mg_src_root() + 'style.css'
  styles = strarr(file_lines(styleFilename))
  openr, styleLun, styleFilename, /get_lun
  readf, styleLun, styles
  free_lun, styleLun
  
  self->_print, self.lun, transpose(styles)
  self->_print, self.lun, '</style></head><body>'
  
  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;   lun
;     the logical unit number to send output to (-1L by default)
;-
pro mguthtmlrunner__define
  compile_opt strictarr
  
  define = { MGutHTMLRunner, inherits MGutTestRunner, $
             lun: 0L $
           }
end