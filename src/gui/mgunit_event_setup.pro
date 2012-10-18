; docformat = 'rst'

;+
; Setup the state of the event recorder/players.
;
; Don't create any widgets between this call and launching your application
; (when recording or playing).
;
; :Keywords:
;    filename : in, optional, type=string
;    event_handler : in, optional, type=string
;    stop : in, optional, type=boolean
;       set to stop playing or recording
;    record : in, optional, type=boolean
;       set to begin recording
;    play : in, optional, type=boolean
;       set to start playback
;-
pro mgunit_event_setup, filename=filename, event_handler=event_handler, $
                        stop=stop, record=record, play=play
  compile_opt strictarr
  @mgunit_event_common

  if (n_elements(filename) gt 0L) then _filename = filename
  if (n_elements(event_handler) gt 0L) then _event_handler = event_handler
  if (n_elements(identifier_offset) gt 0L) then _identifier_offset = identifier_offset

  _identifier_offset = mgunit_next_widget_identifier()

  case 1 of
    keyword_set(stop): _state = 0L
    keyword_set(record): _state = 1L
    keyword_set(play): _state = 2L
    else: _state = 0L
  endcase  
end
