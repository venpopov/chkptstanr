#' @title  Checkpoint Setup
#'
#' @description Deterimine the number of checkpoints for the warmup and
#' sampling, given the desired number of iterations for each
#' and the iterations per checkpoint.
#'
#' @param iter_sampling (positive integer) The number of post-warmup iterations
#'                    to run per chain. Note: in the CmdStan User's Guide this
#'                    is referred to as num_samples.
#'
#' @param iter_warmup (positive integer) The number of warmup iterations to
#'                    run per chain. Note: in the CmdStan User's Guide this
#'                    is referred to as num_warmup.
#'
#' @param iter_per_chkpt (positive integer) The number of iterations per
#'                       check point.
#'
#' @return A list with the following:
#'
#' \itemize{
#'
#' \item \code{warmup_chkpts}: Number of warmup checkpoints
#'
#' \item \code{sample_chkpts}: Number of sampling checkpoints
#'
#' \item \code{total_chkpts}: Total number of checkpoints
#'                          (warmup_chkpts + sample_chkpts)
#'
#' \item \code{iter_per_chkpt}: Iterations per checkpoint
#'
#' }
#'
#' @export
#'
#' @examples
#' chkpt_setup <- chkpt_setup(
#'   iter_sampling = 5000,
#'   iter_warmup = 2000,
#'   iter_per_chkpt = 10
#' )
#'
#' chkpt_setup
chkpt_setup <- function(iter_sampling,
                        iter_warmup,
                        iter_per_chkpt){


  if (isFALSE(is.numeric(iter_sampling))) {
    stop("iter_sampling must be numeric", call. = FALSE)
  }

  if (isFALSE(is.numeric(iter_warmup))) {
    stop("iter_warmup must be numeric", call. = FALSE)
  }

  if (isFALSE(is.numeric(iter_per_chkpt))) {
    stop("iter_warmup must be numeric", call. = FALSE)
  }

  if (iter_sampling < 0) {
    stop("iter_sampling must be positive", call. = FALSE)
  }

  if (isFALSE(iter_sampling == round(iter_sampling))) {
    stop("iter_sampling must be an integer", call. = FALSE)
  }

  if (iter_warmup < 0) {
    stop("iter_warmup must be positive", call. = FALSE)
  }

  if (isFALSE(iter_warmup == round(iter_warmup))) {
    stop("iter_warmup must be an integer", call. = FALSE)
  }

  if (iter_per_chkpt < 0) {
    stop("iter_per_chkpt must be positive", call. = FALSE)
  }

  if (isFALSE(iter_per_chkpt == round(iter_per_chkpt))) {
    stop("iter_per_chkpt must be an integer", call. = FALSE)
  }

  warmup_chkpts <- iter_warmup / iter_per_chkpt
  sample_chkpts <- iter_sampling / iter_per_chkpt
  total_chkpts <-  warmup_chkpts + sample_chkpts

  check_integer <- c(warmup_chkpts,
                     sample_chkpts,
                     total_chkpts)

  if (isFALSE(all(check_integer == round(check_integer)))) {
    stop(paste0("Invalid combination of iterations. iter_warmup, ", 
                "iter_sampling, and iter_warmup + iter_sampling ",
                "must be all divisible by iter_per_chkpt."),
         call. = FALSE)
  }

  returned_object <- list(
    warmup_chkpts = warmup_chkpts,
    sample_chkpts = sample_chkpts,
    total_chkpts = total_chkpts,
    iter_per_chkpt =  iter_per_chkpt
    )

  class(returned_object) <- c("chkpt_setup")
  return(returned_object)

}

#' Print \code{chkpt_setup} Object
#'
#' @param x An object of class \code{chkpt_setup}.
#' @param ... Currently ignored.
#'
#' @export
#' 
#' @return No return value, and used to print the \code{chkpt_setup} object.
#'
#' @examples
#' chkpt_setup <- chkpt_setup(
#'   iter_sampling = 5000,
#'   iter_warmup = 2000,
#'   iter_per_chkpt = 10
#' )
#'
#'
#' chkpt_setup
print.chkpt_setup <- function(x, ...){
  cat("chkptstanr\n")
  cat("-----\n")
  cat("Checkpoint setup:\n\n")
  cat("Total checkpoints:", x$total_chkpts, "\n")
  cat("Warmup checkpoints:", x$warmup_chkpts, "\n")
  cat("Sampling checkpoints:", x$sample_chkpts, "\n")
  cat(paste0("Iterations per checkpoint: ", x$iter_per_chkpt), "\n")
  cat("-----")
}
