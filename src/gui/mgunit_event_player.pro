; docformat = 'rst'

;+
; Convert a serialization of an event structure as a string to an actual 
; structure.
;
; :Private:
; 
; :Returns:
;    event structure
;
; :Params:
;    str : in, required, type=string
;       serialization of event structure to restore
;-
function mgunit_event_player_str2struct, str
  compile_opt strictarr
  @mgunit_event_common
  
  tokens = strsplit(str, /extract, count=ntokens)
  s = create_struct(name=tokens[0])
  
  for t = 1L, (n_tags(s) < (ntokens - 1L)) - 1L do begin
    name_value = strsplit(tokens[t], ':', /extract)
    name = name_value[0]
    value = name_value[1]
    if (strmid(value, 0, 1) eq '''') then begin
      value = strmid(value, 1, strlen(value) - 2)
    endif else begin
      type_id = stregex(value, '[[:alpha:]]+', /extract)
      numeric = stregex(value, '[[:digit:].]+', /extract)
      
      case type_id of
        'B': value = byte(long(value))
        'L': value = long(value)
        'D': value = double(value)
        'U': value = uint(value)
        'UL': value = ulong(value)
        'LL': value = long64(value)
        'ULL': value = ulong64(value)
        '': begin
            if (stregex(value, '.', /boolean)) then begin
              value = float(value)
            endif else begin
              value = fix(value)
            endelse
          end
      endcase
    endelse

    widget_id = name eq 'ID' || name eq 'TOP' || name eq 'HANDLER'
    print, t, tokens[t], _identifier_offset, format='(%"tokens[%d]: %s, offset = %d")'
    if (widget_id) then value += _identifier_offset - 1L 
    
    s.(t - 1L) = value
  endfor
  
  return, s
end


;+
; Event handler for a GUI application which will play back a sequence of
; previously recorded events. Setup the playback with `MGUNIT_EVENT_SETUP`
; before using this routine.
;-
pro mgunit_event_player
  compile_opt strictarr
  on_error, 2
  @mgunit_event_common
  
  if (~file_test(_filename)) then message, 'event log file does not exist'
  
  nlines = file_lines(_filename)
  if (nlines le 0L) then return
  
  events = strarr(nlines)
  openr, lun, _filename, /get_lun
  readf, lun, events
  free_lun, lun
  
  for e = 0L, nlines - 1L do begin
    print, _event_handler, events[e], format='(%"calling event handler %s with event %s")'
    event = mgunit_event_player_str2struct(events[e])
    help, event
    call_procedure, _event_handler, event
  endfor
end
