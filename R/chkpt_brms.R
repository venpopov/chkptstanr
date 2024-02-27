#' @title Checkpoint Sampling: brms
#'
#' @description Fit Bayesian generalized (non-)linear multivariate multilevel
#'   models using brms with checkpointing.
#'
#' @param formula An object of class \code{\link[stats]{formula}},
#'   \code{\link[brms]{brmsformula}}, or \code{\link{brms}{mvbrmsformula}}.
#'   Further information can be found in  \code{\link[brms]{brmsformula}}.
#'
#' @param data An object of class \code{data.frame} (or one that can be coerced
#'   to that class) containing data of all variables used in the model.
#'
#' @param iter_warmup (positive integer) The number of warmup iterations to run
#'   per chain (defaults to 1000).
#'
#' @param iter_sampling (positive integer) The number of post-warmup iterations
#'   to run per chain (defaults to 1000).
#'
#' @param iter_per_chkpt (positive integer). The number of iterations per
#'   checkpoint. Note that \code{iter_sampling} is divided by
#'   \code{iter_per_chkpt} to determine the number of checkpoints. This must
#'   result in an integer (if not, there will be an error).
#'
#' @param iter_typical (positive integer) The number of iterations in the
#'   initial warmup, which finds the so-called typical set. This is an initial
#'   phase, and not included in \code{iter_warmup}. Note that a large enough
#'   value is required to ensure convergence (defaults to 150).
#'
#'
#' @param parallel_chains (positive integer) The \emph{maximum number} of MCMC
#'   chains to run in parallel. If parallel_chains is not specified then the
#'   default is to look for the option \code{mc.cores}, which can be set for an
#'   entire R session by \code{options(mc.cores=value)}. If the \code{mc.cores}
#'   option has not been set then the default is \code{1}.
#'
#' @param threads_per (positive integer) Number of threads to use in
#'   within-chain parallelization (defaults to \code{1}).
#'
#' @param chkpt_progress logical. Should the \code{chkptstanr} progress be
#'   printed (defaults to \code{TRUE}) ? If set to \code{FALSE}, the standard
#'   \code{cmdstanr} progress bar is printed for each checkpoint (which does not
#'   actually keep track of checkpointing progress)
#'
#' @param control A named list of parameters to control the sampler's behavior.
#'   It defaults to NULL so all the default values are used. For a comprehensive
#'   overview see \code{\link[rstan]{stan}}.
#'
#' @param brmsfit Logical. Should a \code{brmsfit} object be returned (defaults
#'   to \code{TRUE}).
#'
#' @param seed (positive integer). The seed for random number generation to make
#'   results reproducible.
#'
#' @param stop_after (positive integer). The number of iterations to sample
#'   before stopping. If \code{NULL}, then all iterations are sampled (defaults
#'   to \code{NULL}). Note that sampling will stop at the end of the first
#'   checkpoint which has an iteration number greater than or equal to
#'   \code{stop_after}.
#'
#' @param path Character string. The path to the folder, that is used for saving
#'   the checkpoints (see Details). You can provide either a relative path to
#'   the current working directory or a full path. You no longer need to create
#'   the folder, as this is done automatically.
#'
#' @param reset logical. Should the checkpointing be reset? If \code{TRUE}, then
#'   the model will begin sampling from the beginning (defaults to
#'   \code{FALSE}). WARNING: This will remove all previous checkpointing
#'   information (see [reset_checkpoints()]). If the model is unchanged and previously
#'   compiled, sampling will begin without recompiling the model.
#'
#' @param ... Any additional arguments passed to \code{\link[brms]{brm}},
#'   including, but not limited to, user-defined prior distributions, the
#'   \code{\link[brms]{brmsfamily}} (e.g., \code{family = poisson())}, data2,
#'   custom_families, etc.
#'
#' @return An object of class \code{brmsfit} (with \code{brmsfit = TRUE}) or
#'   \code{chkpt_brms} (with \code{brmsfit = FALSE}).
#'
#' @note
#'
#' A folder specified by \code{path} is created with four subfolders:
#'
#' \itemize{
#'
#' \item \strong{cmd_fit}: The cmdstanr fittted models (one for each checkpoint).
#'
#' \item \strong{cp_info}: Mass matrix, step size, and initial values for
#'                       next checkpoint (last iteration from previous checkpoint).
#'
#' \item \strong{cp_samples}: Samples from the posterior distribution
#'                            (post warmup)
#'
#' \item \strong{stan_model}: Complied \strong{Stan} model
#'
#' }
#'
#' @importFrom brms make_standata make_stancode threading bf
#' @importFrom rstan read_stan_csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(cmdstanr)
#'
#'
#' # "random" intercept
#' fit1 <- chkpt_brms(
#'   bf(
#'     formula = count ~ zAge + zBase * Trt + (1 | patient),
#'     family = poisson()
#'   ),
#'   data = epilepsy, ,
#'   iter_warmup = 1000,
#'   iter_sampling = 1000,
#'   iter_per_chkpt = 250,
#'   path = "chkpt_folder_fit1"
#' )
#'
#' # brmsfit output
#' fit1
#'
#'
#' # remove "random" intercept (for model comparison)
#' fit2 <- chkpt_brms(
#'   bf(
#'     formula = count ~ zAge + zBase * Trt,
#'     family = poisson()
#'   ),
#'   data = epilepsy, ,
#'   iter_warmup = 1000,
#'   iter_sampling = 1000,
#'   iter_per_chkpt = 250,
#'   path = "chkpt_folder_fit2"
#' )
#'
#' # brmsfit output
#' fit2
#'
#' # compare models
#' loo(fit1, fit2)
#'
#'
#' # priors
#' bprior <- prior(constant(1), class = "b") +
#'   prior(constant(2), class = "b", coef = "zBase") +
#'   prior(constant(0.5), class = "sd")
#'
#' # fit model
#' fit3 <-
#'   chkpt_brms(
#'     bf(
#'       formula = count ~ zAge + zBase + (1 | patient),
#'       family = poisson()
#'     ),
#'     data = epilepsy,
#'     path = "chkpt_folder_fit3",
#'     prior = bprior,
#'     iter_warmup = 1000,
#'     iter_sampling = 1000,
#'     iter_per_chkpt = 250,
#'   )
#'
#' # check priors
#' prior_summary(fit3)
#' }
chkpt_brms <- function(formula,
                       data,
                       iter_warmup = 1000,
                       iter_sampling = 1000,
                       iter_per_chkpt = 100,
                       iter_typical = 150,
                       parallel_chains = 2,
                       threads_per = 1,
                       chkpt_progress = TRUE,
                       control = NULL,
                       brmsfit = TRUE,
                       seed = 1,
                       stop_after = NULL,
                       reset = FALSE,
                       path,
                       ...) {
  # TODO: MASSIVElY SIMPLIFY AND REFACTOR ALL CODE BELOW AFTER THE HOTFIX IS OUT
  stan_phase = ""
  args <- c(as.list(environment()), list(...))

  withr::defer({
    if (stan_phase %in% c("sample", "complete")) {
      if (is.null(returnValue())) {
        message("\nSampling aborted. You can examine the results or continue sampling by rerunning the same code.")
        return(return_object(
          brmsfit = brmsfit,
          formula = formula,
          data = data,
          path = path,
          ...
        ))
      }
    } else {
      message("\nInterupted before or during warmup. No samples available.")
    }
  })

  reset_checkpoints(path, isTRUE(reset))

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Please install the '", "cmdstanr", "' package.")
  }

  if (!is.null(stop_after)) {
    if (stop_after > iter_warmup + iter_sampling) {
      stop_after <- NULL
    }
    stop_at_checkpoint <- ceiling(stop_after / iter_per_chkpt)
    message("Sampling will stop after checkpoint ", stop_at_checkpoint)
  }

  path <- .use_checkpoint_folder(path)


  # TODO: DON"T REMAKE STAN CODE AND DATA IF EXECUTABLE EXISTS
  if (threads_per == 1) {
    stan_data <- brms::make_standata(formula = formula, data = data, ...)
    stan_code <- brms::make_stancode(formula = formula, data = data, ...)
  } else {
    stan_data <- brms::make_standata(
      formula = formula,
      data = data,
      threads = brms::threading(threads_per),
      ...
    )
    stan_code <- brms::make_stancode(
      formula = formula,
      data = data,
      threads = brms::threading(threads_per),
      ...
    )
  }

  stan_code_path <- cmdstanr::write_stan_file(
    code = stan_code,
    dir = paste0(path, "/stan_model"),
    basename = "model"
  )
  
  # TODO: THIS NEEDS A BETTER FIX, FOR NOW, QUICK PATCH
  args_exist <- file.exists(paste0(path, "/stan_model/args.rds"))
  if (args_exist) {
    initial_args <- readRDS(paste0(path, "/stan_model/args.rds"))
    exclude_args <- c('stop_after', 'reset')
    diffs = waldo::compare(args[!names(args) %in% exclude_args], 
                           initial_args[!names(initial_args) %in% exclude_args],
                           ignore_function_env = TRUE,
                           ignore_formula_env = TRUE)
    
    if (length(diffs) > 0) {
      stop("Important arguments have been changed. Please completely reset the checkpointing via reset_checkpoints(path, recompile = TRUE).", call. = FALSE)
    }
  } else {
    saveRDS(args, paste0(path, "/stan_model/args.rds"))
  }
  
  stan_m3 <- cmdstanr::cmdstan_model(
    stan_file = stan_code_path,
    cpp_options = list(stan_threads = TRUE)
  )

  chkpt_set_up <- chkpt_setup(
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    iter_per_chkpt = iter_per_chkpt
  )

  warmup_chkpts <- chkpt_set_up$warmup_chkpts

  total_chkpts <- chkpt_set_up$total_chkpts

  cmd_args <- list(
    data = stan_data,
    chains = parallel_chains,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per
  )

  if (is_zero(list.files(paste0(path, "/cp_info")))) {
    last_chkpt <- 0

    message("Initial Warmup (Typical Set)")

    sample_chunk <- chkpt_typical(
      seed = seed,
      control = control,
      model = stan_m3$sample,
      iter_typical = iter_typical,
      cmd_args = cmd_args,
      progress = chkpt_progress
    )

    stan_state <- extract_stan_state(sample_chunk, "warmup")
  } else {
    cp_fn_list <- list.files(paste0(path, "/cp_info"))

    checkpoints <-
      as.numeric(gsub(".*info_(.+).rds.*", "\\1", cp_fn_list))

    last_chkpt <- max(checkpoints)

    stan_state <- readRDS(file = paste0(
      path, "/cp_info/",
      cp_fn_list[which.max(checkpoints)]
    ))
  }

  if (last_chkpt == total_chkpts) {
    message("Checkpointing complete")
    return(return_object(
      brmsfit = brmsfit,
      formula = formula,
      data = data,
      path = path,
      ...
    ))
  } else {
    # TODO: remove unnecessary else clause
    cp_seq <- seq(last_chkpt + 1, total_chkpts)
  }

  for (i in cp_seq) {
    if (!is.null(stop_after) && i > stop_at_checkpoint) {
      message("Stopping after ", stop_at_checkpoint, " checkpoints")
      if (i > warmup_chkpts + 1) {
        return(return_object(
          brmsfit = brmsfit,
          formula = formula,
          data = data,
          path = path,
          ...
        ))
      }
      stan_phase <- "warmup"
      return(invisible(NULL))
    }

    stan_phase <- c("warmup", "sample")[1 + (i > warmup_chkpts)]
    sample_chunk <- chkpt_sample(
      model = stan_m3$sample,
      cmd_args = cmd_args,
      control = control,
      progress = chkpt_progress,
      cp_cmd_args = cp_cmd_args(
        seed = seed + i,
        phase = stan_phase,
        stan_state = stan_state,
        iter_per_chkpt = iter_per_chkpt,
        path = path,
        checkpoint = i
      )
    )
    
    if (stan_phase == "sample") {
      sample_chunk$save_object(paste0(
        path, "/cp_samples/",
        "samples_", i, ".rds"
      ))
    }

    stan_state <- extract_stan_state(sample_chunk, stan_phase)

    saveRDS(
      object = stan_state,
      file = paste0(path, "/cp_info/cp_info_", i, ".rds")
    )

    cat(progress_bar(chkpt_set_up, phase = stan_phase, i = i))

    if (i == total_chkpts) {
      message("Checkpointing complete")
      stan_phase <- "complete"
      return(return_object(
        brmsfit = brmsfit,
        formula = formula,
        data = data,
        path = path,
        ...
      ))
    }
  }
}


return_object <- function(brmsfit, ...) {
  dots <- list(...)
  args <- dots$args
  dots$args <- NULL
  if (brmsfit) {
    out <- brms::do_call(make_brmsfit, dots)
  } else {
    out <- list(args = args)
    class(out) <- "chkpt_brms"
  }
  out$path <- dots$path
  out
}




#' @title Print \code{chkpt_brms} Objects
#'
#' @param x Object of class \code{chkpt_brms}
#' @param ... Currently ignored
#'
#' @note
#'
#' This function mainly avoids printing out a list,
#' and it is only used when \code{brmsfit = "FALSE"} in \code{\link[chkptstanr]{chkpt_brms}}.
#'
#' Typically, after fitting, the posterior draws should be summarized with
#' \code{\link[chkptstanr]{combine_chkpt_draws}} (assuming \code{brmsfit = "FALSE"}).
#'
#' @return No return value, and used to print the \code{chkpt_brms} object.
#'
#' @export
print.chkpt_brms <- function(x, ...) {
  cat("chkptstanr \n")
  cat("----- \n")
  cat("Date:", date(), "\n")
  cat("(see 'combine_chkpt_draws') \n")
}
