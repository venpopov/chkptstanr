#' @title Extract HMC Sampler Information
#'
#' @description Extract the inverse metric and step size adaption from
#' \code{CmdStanMCMC} objects.
#'
#' @param object An object of class \code{CmdStanMCMC}
#'
#' @return A list including
#'
#' \itemize{
#'
#' \item \code{inv_metric}: Inverse metric for each chain
#'        (with \code{matrix = FALSE}).
#'
#' \item \code{step_size_adapt}: Step size adaptation for each chain.
#'
#'
#' }
#' 
#' @note This is primarily used internally.
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(cmdstanr)
#' 
#' fit_schools_ncp_mcmc <- cmdstanr_example("schools_ncp")
#' 
#' extract_hmc_info(fit_schools_ncp_mcmc)
#' 
#' }
extract_hmc_info <- function(object){

  if(isFALSE(is(object, "CmdStanMCMC"))){
    stop("class must be CmdStanMCMC")
  }

  inv_metric <- object$inv_metric(matrix=FALSE)

  step_size_adapt <- object$metadata()$step_size_adaptation

  returned_object <- list(inv_metric = inv_metric,
                          step_size_adapt = step_size_adapt)

  return(returned_object)
}
