; docformat = 'rst'

;+
; Helper routine useful to convert widget identifiers between sessions for 
; recorder and playback event handlers.
;
; :Private:
;-


;+
; Returns the identifier of the next widget to be created. 
;
; Helper function needed to match widget identifiers between two different 
; application runs. For two launches of an application, the widget identifiers 
; should be the same except for an offset determined by the widget identifier 
; of the first widget created, typically the top-level base.
; 
; :Returns:
;    long
;-
function mgunit_next_widget_identifier
  compile_opt strictarr

  tlb = widget_base()
  widget_control, tlb, /destroy
  
  return, tlb + 1L
end
