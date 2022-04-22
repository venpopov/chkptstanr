#' Extract Stan State
#'
#' @param object An object of class \code{cmdstanr}
#' 
#' @param phase Character string indicating the current phase. 
#'              Options include \code{wormup} and \code{sample}/
#'
#' @return A list containing the inverse metric, step size, and last MCMC
#'        draw (to be used as the initial value for the next checkpoint)
#' @export
#' 
#' @examples 
#' \dontrun{
#' library(cmdstanr)
#' 
#' # eight schools example
#' fit_schools_ncp_mcmc <- cmdstanr_example("schools_ncp")
#' 
#' extract_stan_state(fit_schools_ncp_mcmc, "sample")
#' }
extract_stan_state <- function(object, phase){

  chkpt_info <- list()

  draws <- extract_chkpt_draws(object = object,
                               phase = phase)

  hmc_info <- extract_hmc_info(object = object)

  iters <- dim(draws)[1]
  chains <- dim(draws)[2]

  chkpt_info$inv_metric <- hmc_info$inv_metric
  chkpt_info$step_size_adapt <- hmc_info$step_size_adapt

  chkpt_info$inits <- lapply(seq_len(chains), function(i) {
    returned_object <- get_init(draws = draws,
                                max_iter =  iters,
                                chain = i)
    return(returned_object)
  })

  return(chkpt_info)

}
