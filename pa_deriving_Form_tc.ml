
let () =
  Pa_deriving_tc.register
    ((module Pa_deriving_Form_base.Description),
     (module Pa_deriving_Form_base.Builder))
