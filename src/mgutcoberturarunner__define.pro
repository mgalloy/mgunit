; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test
; runner. The `mgutcoberturarunner` displays the results in the output XML file
; using the "cobertura" format, which is used on the Jenkins CI server for
; code coverage reports.
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
pro mgutcoberturarunner::reportTestSuiteStart, testsuite, $
                                         ntestcases=ntestcases, $
                                         ntests=ntests, $
                                         level=level
  compile_opt strictarr
  
  ; cobertura requires summary statistics at the top
  ; so we can't do anything here
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
pro mgutcoberturarunner::reportTestSuiteResult, npass=npass, nfail=nfail, $
                                          nskip=nskip, level=level, $
                                          total_nlines=total_nlines, $
                                          covered_nlines=covered_nlines, $
                                          testing_routines=testing_routines
  compile_opt strictarr

  if (mg_idlversion(require='8.4')) then begin
    if n_elements(testing_routines) gt 0 then begin
      self->_print, self.lun, '<?xml version="1.0" ?>'
      self->_print, self.lun, "<!DOCTYPE coverage SYSTEM 'http://cobertura.sourceforge.net/xml/coverage-03.dtd'>"
      
      ; get the overall test coverage
      suite_total_nlines = 0L
      suite_covered_nlines = 0L
      for i = 0L, n_elements(testing_routines) - 1L do begin
        r = testing_routines[i]
        if (r.resolved) then begin
          untested_lines = code_coverage(r.name, $
                                         function=r.is_function, nlines=nlines)
          suite_total_nlines += nlines
          if (size(untested_lines, /n_dimensions) eq 0) then begin
            covered_nlines = nlines
          endif else begin
            covered_nlines = nlines - n_elements(untested_lines)
          endelse
          suite_covered_nlines += covered_nlines
        endif
      endfor
      line_coverage = 1.0 * suite_covered_nlines / suite_total_nlines
      self->_print, self.lun, $
        string(line_coverage, line_coverage, systime(1), $
        format='(%"<coverage branch-rate=\"%0.4f\" line-rate=\"%0.4f\" timestamp=\"%d\" version=\"3.7.1\">")')
      self->_print, self.lun, "  <packages>"
      ; there's only one "package"
      self->_print, self.lun, $
        string(line_coverage, line_coverage, $
        format='(%"    <package branch-rate=\"%0.4f\" line-rate=\"%0.4f\" complexity=\"0\" name=\"\">")')
      self->_print, self.lun, "      <classes>"
      
      ; cobertura classes correspond to idl source files
      filepaths = testing_routines.path
      uniqpaths = filepaths[uniq(filepaths, sort(filepaths))]
      
      ; cobertura wants source paths relative to the current working directory
      ; so save that now
      cd, current=cwd
      cwd = cwd + path_sep()
      
      for p = 0L, n_elements(uniqpaths) - 1L do begin
        ; the filepath defines the class
        uniqpath = uniqpaths[p]
        ; get all the routines sharing the filepath
        ind = where(testing_routines.path eq uniqpath, nind)
        if nind eq 0 then begin
          message, "Unexpected path not found: " + uniqpath, /info
          continue
        endif
        ; get routines in the same file
        path_routines = testing_routines[ind]
        ; collect coverage stats on the file
        total_nlines = 0L
        covered_nlines = 0L
        total_executed_lines = []
        total_untested_lines = []
        for i = 0L, n_elements(path_routines) - 1L do begin
          r = path_routines[i]
          if (r.resolved) then begin
            untested_lines = code_coverage(r.name, $
                                           function=r.is_function, nlines=nlines, executed=executed_lines)
            total_nlines += nlines
            total_executed_lines = [total_executed_lines, executed_lines]
            total_untested_lines = [total_untested_lines, untested_lines]
            if (size(untested_lines, /n_dimensions) eq 0) then begin
              covered_nlines += nlines
            endif else begin
              covered_nlines += nlines - n_elements(untested_lines)
            endelse
          endif
        endfor
        
        ; write the class element
        ; the routines are not listed separately
        line_coverage = 1.0 * covered_nlines / total_nlines
        relpath = uniqpath
        cwdpos = strpos(uniqpath, cwd)
        if cwdpos eq 0 then $
          relpath = strmid(uniqpath, strlen(cwd))
        
        self->_print, self.lun, $
          string(line_coverage, line_coverage, relpath, file_basename(uniqpath, '.pro'), $
          format='(%"        <class branch-rate=\"%0.4f\" line-rate=\"%0.4f\" complexity=\"0\" filename=\"%s\" name=\"%s\">")')

        ; no methods
        self->_print, self.lun, "          <methods/>"

        ; write the executed and untested lines
        lines = replicate({ line_number: 0, hit_count: 0}, n_elements(total_executed_lines) + n_elements(total_untested_lines))
        lines[0:n_elements(total_untested_lines)-1].line_number = total_untested_lines
        lines[n_elements(total_untested_lines):*].line_number = total_executed_lines
        lines[n_elements(total_untested_lines):*].hit_count = 1
        lines = lines[sort(lines.line_number)]

        self->_print, self.lun, "          <lines>"

        for l = 0L, n_elements(lines) - 1L do begin
          line = lines[l]
          ; sometimes a line number 0 is reported by code_coverage?
          if line.line_number gt 0 then begin
            self->_print, self.lun, $
              string(line.hit_count, line.line_number, $
              format='(%"            <line hits=\"%d\" number=\"%d\"/>")')
          endif
        endfor
        
        self->_print, self.lun, "          </lines>"
        self->_print, self.lun, "        </class>"
      endfor
      
      self->_print, self.lun, "      </classes>"
      self->_print, self.lun, "    </package>"
      self->_print, self.lun, "  </packages>"
      self->_print, self.lun,"</coverage>"
    endif
  endif
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
pro mgutcoberturarunner::reportTestCaseStart, testcase, ntests=ntests, level=level
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
pro mgutcoberturarunner::reportTestCaseResult, npass=npass, nfail=nfail, $
                                         nskip=nskip, level=level
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
pro mgutcoberturarunner::reportTestStart, testname, level=level
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
;   skipped : in, required, type=boolean
;     indicates whether the test should be counted in the results
;   time : in, required, type=float
;     time for the test to run
;   level : in, required, type=integer
;     level of test case
;   math_errors : out, optional, type=integer
;     bitmask of `CHECK_MATH` return values
;-
pro mgutcoberturarunner::reportTestResult, msg, passed=passed, $
                                     output=output, skipped=skipped, $
                                     time=time, level=level, $
                                     math_errors=math_errors
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
pro mgutcoberturarunner::reportTestCaseCoverage, covered_routines, tested_routines, $
                                           level=level, $
                                           total_nlines=total_nlines, $
                                           covered_nlines=covered_nlines
  compile_opt strictarr

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
pro mgutcoberturarunner::_print, lun, text, _extra=e
  compile_opt strictarr

  printf, lun, text, _extra=e
  if (lun gt 0L) then flush, lun
end


;= lifecycle methods

;+
; Free resources.
;-
pro mgutcoberturarunner::cleanup
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
function mgutcoberturarunner::init, filename=filename, color=color, _extra=e
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
;     the logical unit number to send output to (`-1L` by default)
;-
pro mgutcoberturarunner__define
  compile_opt strictarr

  define = { mgutcoberturarunner, inherits MGutTestRunner, $
             lun: 0L $
           }
end