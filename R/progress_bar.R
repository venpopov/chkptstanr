progress_bar <- function(setup_info, phase, i){
  paste0(
    "Chkpt: ",
    i,
    " / ",
    setup_info$total_chkpts,
    "; Iteration: ",
    i * setup_info$iter_per_chkpt,
    " / ",
    (setup_info$total_chkpts * setup_info$iter_per_chkpt),
    " (",
    phase,
    ")\n"
  )
}