#' @title Create Folder for Checkpointing
#'
#' @description Create the folder for checkingpointing, which
#'              will "house" additional folders for the \code{.stan}
#'              model, checkpointing information, and draws from the
#'              posterior distribution.
#'
#' @param folder_name Character string. Desired name for the "parent"
#'                    folder (defaults to \code{checkpoint}).
#' 
#' @param path Character string, when specified. Defaults to \code{NULL},
#'             which then makes the folder in the working directory.
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
#' @return No return value, and instead creates a directory with
#' folders that will contain the checkpointing samples and other 
#' information.
#' 
#' @examples 
#' path <- create_folder(folder_name = "cp_folder")
#' 
#' # remove folder
#' unlink("cp_folder", recursive = TRUE)
create_folder <- function(folder_name = "cp_folder", 
                          path = NULL) {
  
  if (is.null(path)) {
    
    path <- paste0(getwd(),  paste0("/", folder_name))
    
    if (dir.exists(paths = path)) {
      stop("folder already exists")
      
    }
    
    dir.create(path)
    
    dir.create(paste0(path, "/stan_model"))
    
    dir.create(paste0(path, "/cp_info"))
    
    dir.create(paste0(path, "/cp_samples"))
    
    dir.create(paste0(path, "/cmd_fit"))
    
    return(path)
    
  } else {
    
    if(isFALSE(is.character(path))){
      stop("path must be a character string.")
    }
    
    path <- paste0(path,  paste0("/", folder_name))
    
    if (dir.exists(paths = path)) {
      stop("folder already exists")
      
    }
    
    dir.create(path)
    
    dir.create(paste0(path, "/stan_model"))
    
    dir.create(paste0(path, "/cp_info"))
    
    dir.create(paste0(path, "/cp_samples"))
    
    dir.create(paste0(path, "/cmd_fit"))
    
    return(path)
    
  }
  
}


