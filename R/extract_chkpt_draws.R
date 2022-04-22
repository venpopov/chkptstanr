#' @title Extract Draws from \code{CmdStanMCMC} Objects
#'
#' @description A convenience function for extracting the draws from a
#' \code{CmdStanMCMC} object.
#'
#' @param object An object of class \code{CmdStanMCMC}.
#'
#' @param phase Character string. Which phase during checkpointing?
#'              The options included \code{warmup} and \code{sample}.
#'              The latter extracts the draws with
#'              \code{inc_warmup = FALSE}, which is the default in
#'              \code{\link[cmdstanr]{draws}}
#'
#' @note This can be used to extract the draws in general by setting
#'       \code{phase = "sample"} which then only includes the post-warmup
#'       draws.
#'
#' @return A 3-D \code{draws_array} object
#'         (iteration \emph{x} chain \emph{x} variable).
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  library(cmdstanr)
#' 
#' # eight schools example
#' fit_schools_ncp_mcmc <- cmdstanr_example("schools_ncp")
#' 
#' drws <- extract_chkpt_draws(object = fit_schools_ncp_mcmc,
#'                             phase = "sample")
#'
#' # compare to cmdstanr
#' all.equal(drws, fit_schools_ncp_mcmc$draws())
#' }
extract_chkpt_draws <- function(object, phase) {

  if (isFALSE(is(object, "CmdStanMCMC"))) {
    stop("object class must be CmdStanMCMC")
  }

  if (isFALSE(is.character(phase))) {
    stop("phase must be a character string")
  }

  if (phase == "warmup") {

    draws <- object$draws(inc_warmup = TRUE)

  } else if (phase == "sample") {

    draws <- object$draws()

  } else {

    stop("phase must be warmup or sample")
  }

  return(draws)

}
