; docformat = 'rst'

;+
; Build mgunit docs.
;-
pro mgunit_build_userdocs
  compile_opt strictarr
  
  root = mg_src_root()

  idldoc, root=filepath('', subdir='src', root=root), $
          output=filepath('', subdir='api-userdocs', root='.'), $
          overview=filepath('overview.txt', root=root), $
          /nosource, $
          title=string(mgunit_version(), format='(%"mgunit %s API documentation")'), $
          subtitle='Unit testing framework for IDL', $
          /embed, /user, format_style='rst', markup_style='rst'
end
