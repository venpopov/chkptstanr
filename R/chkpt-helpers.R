#' @importFrom utils capture.output packageVersion

# wrapper around file path ignoring null values
file_path2 <- function(...) {
  dots <- list(...)
  dots <- dots[!sapply(dots, is.null)]
  do.call(file.path, dots)
}

#' @title Delete Checkpoint Folders containing samples, keep the model
#' @description Deletes all checkpoint files and folders under path except for
#'   stan_model/model.stan and stan_model/model.exe. This allows you to restart 
#'   the sampling from 0 without recompiling the model.
#' @param path (character) The path to the checkpoint folder.
#' @param reset (logical) If TRUE, only the checkpoint folders are deleted
#' @param recompile (logical) If TRUE, the entire folder is deleted allowing for 
#'  a fresh start. If both \code{reset} and \code{recompile} are \code{FALSE},
#'  nothing is done.
#' @return NULL
#' @export
reset_checkpoints <- function(path, reset = TRUE, recompile = FALSE) {
  if (reset && !recompile) {
    to_remove <- file_path2(path, c("cmd_fit", "cp_info", "cp_samples","cmd_output"))
    unlink(to_remove, recursive = TRUE)
  } else if (recompile) {
    unlink(path, recursive = TRUE)
  }
  return(invisible(reset))
}

# utility function for setting testing options
# defaults are to reset the checkpoints but keep the model to avoid recompiling
# and reset checkpoints at the end of the test
setup_model_testing <- function(dir = NULL, 
                                recompile = getOption('test_recompile', FALSE),
                                path = getOption('test_checkpoint_path', tempdir())) {
  wdir <- getwd()
  setwd(here::here())
  withr::defer_parent(setwd(wdir))
  path = file_path2(path, dir)
  if (recompile) {
    unlink(path, recursive = TRUE)
  } else {
    reset_checkpoints(path)
  }
  withr::defer_parent(reset_checkpoints(path))
  return(path)
}


rstring <- function(n = 10, char_set = c(letters, LETTERS, 0:9), seed = NULL) {
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  withr::with_seed(seed, {
    return(paste0(sample(char_set, n, replace = TRUE), collapse = ""))
  })
}

# find so-called `typical set`
chkpt_typical <- function(model,
                          cmd_args,
                          control,
                          iter_typical,
                          seed,
                          progress) {
  
  cmd_args$iter_sampling <- 0
  cmd_args$seed <- seed
  cmd_args$save_warmup <- TRUE
  cmd_args$iter_warmup <- iter_typical
  
  if (progress) {
    capture_output <- capture.output({
      suppressMessages({
        sample_chunk <- do.call(what = model,
                                args = c(cmd_args, control))
        
      })
      
    })
    
  } else {
    sample_chunk <- do.call(what = model,
                            args = c(cmd_args, control))
    
    
  }
  
  return(sample_chunk)
}


# check point sampling
chkpt_sample <- function(model, 
                         cmd_args, 
                         cp_cmd_args, 
                         control, 
                         progress){
  
  if(progress){
    
    capture_output <- capture.output({
      suppressMessages({
        sample_chunk <-
          do.call(model, args = c(cmd_args, cp_cmd_args, control))
      })
    })
    
  } else {
    
    sample_chunk <-
      do.call(model, args = c(cmd_args, cp_cmd_args, control))
  }
  
  return(sample_chunk)
}


# sampling cmdstanr arguments
cp_cmd_args <- function(seed,
                        phase,
                        stan_state,
                        iter_per_chkpt,
                        path,
                        checkpoint){
  
  isWarmup <- phase == "warmup"
  list(
    iter_sampling = ifelse(isWarmup, 0, iter_per_chkpt),
    iter_warmup = ifelse(isWarmup, iter_per_chkpt, 0),
    seed = seed,
    save_warmup = isWarmup,
    adapt_engaged = FALSE,
    init = stan_state$inits,
    inv_metric = stan_state$inv_metric,
    step_size = stan_state$step_size_adapt,
    output_dir = paste0(path, "/cmd_output"),
    output_basename = paste0("output_", checkpoint, "_chain")
  )
}

# matrix initial values
get_init <- function(draws, max_iter, chain){
  
  last_draw <- draws[max_iter, chain, -1]
  
  param_names <- dimnames(last_draw)$variable
  
  flat_draws <- as.numeric(last_draw)
  
  unique_names <- unique(gsub(pattern = "\\[.*?\\]",
                              replacement =  "",
                              x = param_names))
  
  returned_object <-
    lapply(seq_along(unique_names), function(x) {
      ids <- grepl(pattern = paste0("\\b", unique_names[x] , "\\b"),
                   x = param_names)
      
      bracket_check <-
        regmatches(x = param_names[ids],
                   m = gregexpr(pattern = "\\[.*?\\]",
                                text = param_names[ids]))
      
      if (length(unlist(bracket_check)) == 0) {
        
        return(flat_draws[ids])
        
      } else {
        
        indices <- sapply(strsplit(
          gsub(
            pattern = "\\[|\\]",
            replacement = "",
            x = bracket_check
          ),
          split = ","
        ), as.numeric)
        
        if (is(indices, "matrix")) {
          
          return(matrix(
            data = flat_draws[ids],
            nrow = max(indices[1, ]),
            ncol = max(indices[2, ])
          ))
          
        } else {
          
          return(flat_draws[ids])
          
        }
        
      }
      
    })
  
  names(returned_object) <- unique_names
  
  return(returned_object)
  
}

check_for_model <- function(x, path) {
  return(any(x == list.files(paste0(
    path, "/stan_model"
  ))))
}

is_zero <- function(x){
  return(length(x) == 0)
}

check_restart <- function(initial_args, restart_args) {
  return(isTRUE(suppressWarnings({
    all.equal(initial_args, restart_args)
  })))
  
}


stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


create_testing_samples <- function(path, cleanup_rest = TRUE, seed = 1234) {
  withr::local_seed(seed)
  unlink(path, recursive = TRUE)
  formula <- brms::bf(formula = count ~ zAge + zBase)
  family <- poisson()
  fit <- chkpt_brms(
    formula = formula,
    family = family,
    data = brms::epilepsy,
    iter_warmup = 100,
    iter_sampling = 400,
    iter_per_chkpt = 100,
    path = path,
    seed = seed
  )
  if (cleanup_rest) {
    unlink(file.path(path, c("stan_model","cmd_fit")), recursive = TRUE)
  }
}

strip_attributes <- function(x, protect = c("names", "row.names", "class")) {
  to_remove <- names(attributes(x))
  to_remove <- to_remove[!to_remove %in% protect]
  attributes(x)[to_remove] <- NULL
  return(x)
}