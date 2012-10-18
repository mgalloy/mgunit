; docformat = 'rst'

;+
; Converts variables found in event structure fields to a string 
; representation.
;
; :Private:
;
; :Returns: 
;    string
;
; :Params:
;    variable : in, required, type=any found event structure
;       variable found in event structure fields
;-
function mgunit_event_recorder_var2str, variable
  compile_opt strictarr
  
  case size(variable, /type) of
     0: ;not found in event structures
     1: return, strtrim(variable, 2) + 'B'
     2: return, strtrim(variable, 2)
     3: return, strtrim(variable, 2) + 'L'
     4: return, strtrim(variable, 2)
     5: return, strtrim(variable, 2) + 'D'
     6: ;not found in event structures
     7: return, string(variable, format='(%"''%s''")')
     8: ;not found in event structures
     9: ;not found in event structures
    10: ;not found in event structures
    11: ;not found in event structures
    12: return, strtrim(variable, 2) + 'U'
    13: return, strtrim(variable, 2) + 'UL'
    14: return, strtrim(variable, 2) + 'LL'
    15: return, strtrim(variable, 2) + 'ULL'
  endcase
  
  ; return empty string if the variable type is not supported
  return, ''
end


;+
; Converts an event structure to a string.
;
; :Private:
;
; :Returns:
;    string
;
; :Params:
;    event : in, required, type=event structure
;       event structure
;-
function mgunit_event_recorder_struct2str, event
  compile_opt strictarr
  @mgunit_event_common
    
  s = tag_names(event, /structure_name)
  
  tnames = tag_names(event)
  for t = 0L, n_tags(event) - 1L do begin
    value = event.(t)
    if (tnames[t] eq 'ID' || tnames[t] eq 'TOP' || tnames[t] eq 'HANDLER') then begin
      value -= _identifier_offset - 1L
    endif
    s += string(tnames[t], mgunit_event_recorder_var2str(value), $
                format='(%" %s:%s")')
  endfor

  return, s
end


;+
; Record the events for a 
;
; See `gui/` for a description and example of the full system.
;
; :Params:
;    event : in, required, type=event structure
;       widget event
;-
pro mgunit_event_recorder, event
  compile_opt strictarr
  @mgunit_event_common
  
  ; only record if in the recording state
  if (_state eq 1) then begin
    ; record the event in _filename
    openw, lun, _filename, /get_lun, /append
    printf, lun, mgunit_event_recorder_struct2str(event)
    free_lun, lun
  endif
  
  ; call the normal event handler
  call_procedure, _event_handler, event
end
