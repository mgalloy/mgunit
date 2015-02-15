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
; Write the header at the top of the coverage.xml file.
;-
pro mgutcoberturarunner::_writeHeader
  self->_print, self.lun, '<?xml version="1.0" ?>'
  self->_print, self.lun, "<!DOCTYPE coverage SYSTEM 'http://cobertura.sourceforge.net/xml/coverage-03.dtd'>"
end


;+
; Write a <lines> section of coverage.xml. 
;
; :Params:
;   total_executed_lines : in, required, type=`intarr`
;     array of line numbers of code that has been executed
;   total_untested_lines : in, required, type=`intarr`
;     array of line numbers of code that has not been executed
;-
pro mgutcoberturarunner::_writeLines, total_executed_lines, total_untested_lines
  ; merge the executed and untested lines
  lines = replicate({ line_number: 0, hit_count: 0}, n_elements(total_executed_lines) + n_elements(total_untested_lines))
  lines[0:n_elements(total_untested_lines)-1].line_number = total_untested_lines
  lines[n_elements(total_untested_lines):*].line_number = total_executed_lines
  lines[n_elements(total_untested_lines):*].hit_count = 1
  uniqlines = lines[uniq(lines.line_number, sort(lines.line_number))]

  self->_print, self.lun, "          <lines>"

  for l = 0L, n_elements(uniqlines) - 1L do begin
    line = uniqlines[l]
    ; sometimes a line number 0 is reported by code_coverage?
    if line.line_number gt 0 then begin
      self->_print, self.lun, $
        string(line.hit_count, line.line_number, $
        format='(%"            <line hits=\"%d\" number=\"%d\"/>")')
    endif
  endfor

  self->_print, self.lun, "          </lines>"
end


;+
; Compute the fractional line coverage of an array of testing_routines.
; Optionally build and return arrays of executed and untested line numbers.
;
; :Returns:
;   Float between 0 and 1 giving the fractional line coverage across
;   the array of routines.
;   
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be tested.
;   total_executed_lines : out, optional, type=`intarr`
;     array of line numbers of code that has been executed
;   total_untested_lines : out, optional, type=`intarr`
;     array of line numbers of code that has not been executed
;-
function mgutcoberturarunner::_getLineCoverage, testing_routines=testing_routines, $
  total_executed_lines=total_executed_lines, total_untested_lines=total_untested_lines
  ; collect coverage stats on the file
  total_nlines = 0L
  covered_nlines = 0L
  total_executed_lines = []
  total_untested_lines = []
  for i = 0L, n_elements(testing_routines) - 1L do begin
    r = testing_routines[i]
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
  line_coverage = 1.0 * covered_nlines / total_nlines
  return, line_coverage
end


;+
; Write a <class> section of coverage.xml.
;
; :Params:
;   uniqpath : in, required, type=string
;     the complete path to a "class", here any .pro file
;   cwd : in, required, type=string
;     the current working directory of the running program
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writeClass, uniqpath, cwd, testing_routines=testing_routines
  ; get all the routines sharing the filepath
  ind = where(testing_routines.path eq uniqpath, nind)
  if nind eq 0 then begin
    message, "Unexpected path not found: " + uniqpath, /info
    return
  endif
  
  ; get routines in the same file
  path_routines = testing_routines[ind]
  ; collect coverage stats on the file
  line_coverage = self->_getLineCoverage(testing_routines=path_routines, $
    total_executed_lines=total_executed_lines, total_untested_lines=total_untested_lines)

  ; write the class element
  ; the routines are not listed separately
  relpath = uniqpath
  cwdpos = strpos(uniqpath, cwd)
  if cwdpos eq 0 then $
    relpath = strmid(uniqpath, strlen(cwd))

  self->_print, self.lun, $
    string(line_coverage, line_coverage, relpath, file_basename(uniqpath, '.pro'), $
    format='(%"        <class branch-rate=\"%0.4f\" line-rate=\"%0.4f\" complexity=\"0\" filename=\"%s\" name=\"%s\">")')

  ; no methods
  self->_print, self.lun, "          <methods/>"
  
  ; print lines with hit count
  self->_writeLines, total_executed_lines, total_untested_lines

  self->_print, self.lun, "        </class>"
end


;+
; Write a <classes> section of coverage.xml.
;
; :Params:
;   cwd : in, required, type=string
;     the current working directory of the running program
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writeClasses, cwd, testing_routines=testing_routines
  self->_print, self.lun, "      <classes>"
  
  ; cobertura classes correspond to idl source files
  filepaths = testing_routines.path
  uniqpaths = filepaths[uniq(filepaths, sort(filepaths))]

  for p = 0L, n_elements(uniqpaths) - 1L do begin
    ; the filepath defines the class
    uniqpath = uniqpaths[p]
    
    self->_writeClass, uniqPath, cwd, testing_routines=testing_routines
  endfor

  self->_print, self.lun, "      </classes>"
end


;+
; Write a <package> section of coverage.xml.
;
; :Params:
;   package : in, required, type=string
;     the complete directory to a "package", here a directory containing .pro files
;   cwd : in, required, type=string
;     the current working directory of the running program
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writePackage, package, cwd, testing_routines=testing_routines
  ; get all the files within the package
  ind = where(file_dirname(testing_routines.path) eq package, nind)
  if nind eq 0 then begin
    message, "Unexpected package files not found: " + package, /info
    return
  endif
  package_routines = testing_routines[ind]
  
  ; compute package-level coverage
  line_coverage = self->_getLineCoverage(testing_routines=package_routines)

  ; write the package element
  relpath = package + path_sep()
  cwdpos = strpos(relpath, cwd)
  if cwdpos eq 0 then $
    relpath = strmid(package, strlen(cwd))
  self->_print, self.lun, $
    string(line_coverage, line_coverage, relpath, $
    format='(%"    <package branch-rate=\"%0.4f\" line-rate=\"%0.4f\" complexity=\"0\" name=\"%s\">")')
    
  self->_writeClasses, cwd, testing_routines=package_routines
  
  self->_print, self.lun, "    </package>"
end


;+
; Write a <packages> section of coverage.xml.
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writePackages, testing_routines=testing_routines
  ; cobertura wants source paths relative to the current working directory
  ; so save that now
  cd, current=cwd
  cwd = cwd + path_sep()

  self->_print, self.lun, "  <packages>"

  ; cobertura packages correspond to folders in the idl
  packages = file_dirname(testing_routines.path)
  uniqpackages = packages[uniq(packages, sort(packages))]
  for f = 0L, n_elements(uniqpackages) - 1L do begin
    package = uniqpackages[f]
    
    self->_writePackage, package, cwd, testing_routines=testing_routines
  endfor

  self->_print, self.lun, "  </packages>"
end


;+
; Write the <coverage> section of coverage.xml.
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writeCoverage, testing_routines=testing_routines
  ; get the overall test coverage
  line_coverage = self->_getLineCoverage(testing_routines=testing_routines)
  self->_print, self.lun, $
    string(line_coverage, line_coverage, systime(1), $
    format='(%"<coverage branch-rate=\"%0.4f\" line-rate=\"%0.4f\" timestamp=\"%d\" version=\"3.7.1\">")')

  self->_writePackages, testing_routines=testing_routines

  self->_print, self.lun,"</coverage>"
end


;+
; Write the coverage.xml rile with header and all contents.
;
; :Keywords:
;   testing_routines : in, required, type=`structarr`
;     array of structures containing the routines to be reported.
;-
pro mgutcoberturarunner::_writeCoverageXml, testing_routines=testing_routines
  compile_opt strictarr

  if n_elements(testing_routines) gt 0 then begin
    self->_writeHeader
    self->_writeCoverage, testing_routines=testing_routines
  endif
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
    self->_writeCoverageXml, testing_routines=testing_routines
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