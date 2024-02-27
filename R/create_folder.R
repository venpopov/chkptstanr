#' @title Create Folder for Checkpointing (Deprecated)
#'
#' @description 
#' 
#'   create_folder() is deprected. Provide a path directly in chkpt_brms()', 
#'   or chkpt_stan() instead and a folder will be created automatically.'
#'   
#'   # -------------------
#' 
#'   Create the folder for checkingpointing, which will 'house'
#'   additional folders for the \code{.stan} model, checkpointing information,
#'   and draws from the posterior distribution.
#'
#' @param folder_name Character string. Desired name for the 'parent' folder
#'   (defaults to \code{cp_folder}).
#'
#' @param path Character string, when specified. Defaults to \code{NULL}, which
#'   then makes the folder in the working directory.
#'
#' @note
#'
#' This creates a directory with four folders:
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
#' @export
#'
#' @return the path to the main parent folder containing the four subfolders.
#'   This path should be used as the \code{path} argument in
#'   \code{\link{chkpt_brms}}. If \code{return_relative = TRUE}, the relative
#'   path to the current working directory is returned. If \code{path} is
#'   specified or \code{return_relative = FALSE}, the full path is returned.
#'
#' @examples
#' # create initial folder
#' path <- create_folder(folder_name = 'cp_folder')
#' path
#' unlink('cp_folder', recursive = TRUE) # remove folder
#' 
#' # remove folder
#' unlink('cp_folder', recursive = TRUE)
#' identical(dir(path), character(0))
#'
#' # repeat - no warning
#' path <- create_folder(folder_name = 'cp_folder')
#'
#' # repeat - warning, but folders are kept
#' path <- create_folder(folder_name = 'cp_folder')
#' identical(dir(path), c('cmd_fit', 'cp_info', 'cp_samples', 'stan_model'))
#'
#' unlink('cp_folder', recursive = TRUE)
#'
#' # specify nested folder
#' path <- create_folder(folder_name = 'nested_folder/cp_folder')
#' path
#' unlink('nested_folder', recursive = TRUE) # remove folder
create_folder <- function(folder_name = "cp_folder", path = NULL) {
  .Deprecated(msg = paste0('create_folder() is deprected. Provide a path directly in chkpt_brms()', 
              ' or chkpt_stan() instead and a folder will be created automatically.'))
  path <- file_path2(path, folder_name)
  .use_checkpoint_folder(path)
}

.use_checkpoint_folder <- function(path) {
  subfolders <- c("cmd_fit", "cp_info", "cp_samples", "stan_model", "cmd_output")
  fs::dir_create(path, subfolders)
  path
}
