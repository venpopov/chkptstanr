#' @title Make \code{brmsfit} Object
#' 
#' @description This is primarily used internally, wherein the \code{cmdstanr} 
#' object is converted into a \code{brmsfit} object.
#' 
#' @param object An object of class \code{chkpt_brms}
#' 
#' @param formula An object of class \code{\link[stats]{formula}}, 
#'                \code{\link[brms]{brmsformula}}, or \code{\link{brms}{mvbrmsformula}}.
#'                Further information can be found in  \code{\link[brms]{brmsformula}}.
#'                
#' 
#' @param data  An object of class \code{data.frame} (or one that can be coerced to that class) 
#'             containing data of all variables used in the model.
#' 
#' @param path Character string. The path to the folder, that is used for 
#'             saving the checkpoints.
#'             
#' @param ... Additional arguments to be passed to \code{brm}.
#' 
#' @importFrom brms brm 
#' 
#' @return An object of class \code{brmsfit}
#' 
#' @note This is primarily an internal function that constructs
#' a \code{brmsfit} object.
#' 
#' @export
make_brmsfit <- function(object, formula = NULL, data = NULL, path, ...) {
  
  if(is.null(formula)){
    formula <- object$args$formula
  }
  
  if(is.null(data)){
    data <- object$args$data
  }

  fit <- brms::brm(formula = formula,
                   data = data, 
                   empty = TRUE,
                   ...)

  file_names <- list.files(paste0(path, "/cmd_fit/"))
  
  checkpoints <- length(file_names)
  
  ordered_file_names <- paste0(path, "/cmd_fit/cmd_fit_",
                               sort(as.numeric(
                                 gsub(".*fit_(.+).rds.*", "\\1", file_names)
                               )), ".rds")
  
  stanfit_to_brms <- readRDS(ordered_file_names[1])
  
  chains <- stanfit_to_brms@sim$chains
  
  for (i in seq_len(chains)) {
    stanfit_to_brms@sim$samples[[i]] <-
      do.call(rbind, lapply(seq_len(checkpoints), function(x) {
        readRDS(ordered_file_names[x])@sim$samples[[i]]
      }))
    
  }
  
  stanfit_to_brms@sim$n_save <- stanfit_to_brms@sim$n_save * checkpoints
  
  fit$fit <- stanfit_to_brms
  
  fit <- brms::rename_pars(fit)
  
  return(fit)
}

# if multiple stan fits are saved to the same folder, the read_stan_csv_multiple,
# function can be used to read them all at once. It recognizes different chains' files
# that belong to the same model fit.
# @param files A character vector of file paths
# @param idpattern A regular expression pattern to identify the files that 
#   belong to the same model fit
# @return A list of stanfit objects
read_stan_csv_multiple <- function(files, idpattern='output_[0-9]+_') {
  fits <- list()
  ids <- unique(stringr::str_extract(files,idpattern))
  for (i in ids) {
    fits[[i]] <- rstan::read_stan_csv(files[grepl(i, files)])
  }
  fits
}

combine_samples <- function(files, warmup_chkpts, save_warmup = FALSE) {
  # a list of stanfit objects for each checkpoint
  checkpoint_fits <- read_stan_csv_multiple(files, idpattern = 'output_[0-9]+_')
  n_chkpts <- length(checkpoint_fits)
  available_checkpoints <- 1:n_chkpts
  
  # an initial object to store the combined checkpoint samples
  out <- checkpoint_fits[[1]]
  chains <- out@sim$chains
  
  # calculate total number of iterations and warmup
  iter <- out@sim$iter * n_chkpts
  warmup <- out@sim$warmup * min(warmup_chkpts, n_chkpts)
  n_save <- out@sim$n_save * n_chkpts - warmup * !save_warmup
  warmup2 <- rep(warmup, chains)  - warmup * !save_warmup
  save_checkpoints <- available_checkpoints
  if (!save_warmup) {
    save_checkpoints <- save_checkpoints[-seq_len(min(warmup_chkpts, n_chkpts))]
  }
  
  if (length(save_checkpoints) == 0) {
    stop("No checkpoints to combine", call. = FALSE)
  }
  
  # recode initial info
  out@model_name <- "model"
  out@stanmodel@model_name <- "model"
  out@sim$iter <- iter
  out@sim$warmup <- warmup
  out@sim$n_save <- n_save
  out@sim$warmup2 <- warmup2
  for (chain in 1:chains) {
    out@stan_args[[chain]]$model <- "model"
    out@stan_args[[chain]]$iter <- iter
    out@stan_args[[chain]]$warmup <- warmup
    out@stan_args[[chain]]$save_warmup <- as.numeric(save_warmup)
    out@stan_args[[chain]]$start_datetime <- checkpoint_fits[[1]]@stan_args[[1]]$start_datetime
  }
  
  # combine samples
  samples <- list()
  for (i in seq_len(chains)) {
    samples[[i]] <- do.call(rbind, lapply(save_checkpoints, function(x) {
      strip_attributes(checkpoint_fits[[x]]@sim$samples[[i]])
    }))
    
    # add attributes
    attr(samples[[i]], "sampler_params") <- 
      do.call(rbind, lapply(save_checkpoints, function(x) {
        attr(checkpoint_fits[[x]]@sim$samples[[i]], "sampler_params")
      }))
    
    adinfo <- attr(checkpoint_fits[[n_chkpts]]@sim$samples[[i]], "adaptation_info")
    attr(samples[[i]], "adaptation_info") <- adinfo
    out@stan_args[[i]]$adaptation_info <- adinfo
    
    attr(samples[[i]], "args") <- 
      attr(checkpoint_fits[[1]]@sim$samples[[i]], "args")
    
    elapsed_time <- sapply(available_checkpoints, function(x) {
      attr(checkpoint_fits[[x]]@sim$samples[[i]], "elapsed_time")
    })
    elapsed_time <- rowSums(elapsed_time)
    attr(samples[[i]], "elapsed_time") <- elapsed_time
    time_info <- glue::glue("#  Elapsed Time: {elapsed_time[1]} seconds (Warm-up)",
                            "#                {elapsed_time[2]} seconds (Sampling)",
                            "#                0.016 seconds (Total)",
                            "# ",
                                               .sep = '|')
    out@stan_args[[i]]$time_info <- strsplit(time_info,"\\|")[[1]]
    
    
    mean_pars <- sapply(save_checkpoints, function(x) {
      attr(checkpoint_fits[[x]]@sim$samples[[i]], "mean_pars")
    })
    attr(samples[[i]], "mean_pars") <- rowMeans(mean_pars)
    
    mean_lp <- sapply(save_checkpoints, function(x) {
      attr(checkpoint_fits[[x]]@sim$samples[[i]], "mean_lp")
    })
    attr(samples[[i]], "mean_lp") <- mean(mean_lp)
    
    # combine permutations
    perms <- lapply(save_checkpoints, function(x) checkpoint_fits[[x]]@sim$permutation[[i]])
    shift <- rep(0:(length(perms) - 1)*length(perms[[1]]), each=length(perms[[1]])) 
    out@sim$permutation[[i]] <- unlist(perms) + shift
   

  }
  out@sim$samples <- samples
  out
}

