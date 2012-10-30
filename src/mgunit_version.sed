; docformat = 'rst'

;+
; Returns `mgunit` version. This file is automatically edited by the build
; process to edit the contents of the version and revision variables below.
;
; :Returns:
;    string
;
; :Keywords:
;    full : in, optional, type=boolean
;       set to return git revision as well
;-
function mgunit_version, full=full
  compile_opt strictarr, hidden

  version = '1.2'
  revision = 'r119'

  return, version + (keyword_set(full) ? (' ' + revision) : '')
end
