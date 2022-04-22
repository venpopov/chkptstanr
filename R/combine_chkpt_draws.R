#' Combine Checkpoint Draws
#'
#' @param object An object of class \code{brmsfit} or \code{chkpt_stan}.
#' 
#' @param ... Currently ignored.
#'
#' @return An object of class \code{draws_array}.
#'
#' @importFrom abind abind
#' 
#' @export
#' 
#' @examples 
#'  
#' \dontrun{  
#' path <- create_folder(folder_name = "chkpt_folder_fit1")
#' 
#' stan_code <- "
#' data {
#'  int<lower=0> n;
#'   real y[n];
#'   real<lower=0> sigma[n];
#' }
#' parameters {
#'   real mu;
#'   real<lower=0> tau;
#'   vector[n] eta;
#' }
#' transformed parameters {
#'   vector[n] theta;
#'   theta = mu + tau * eta;
#' }
#' model {
#'   target += normal_lpdf(eta | 0, 1);
#'   target += normal_lpdf(y | theta, sigma);
#' }
#' "
#' 
#' stan_data <- schools.data <- list(
#'   n = 8,
#'   y = c(28,  8, -3,  7, -1,  1, 18, 12),
#'   sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
#' )
#' 
#' fit2 <- chkpt_stan(model_code = stan_code,
#'                    data = stan_data,
#'                    iter_warmup = 1000,
#'                    iter_sampling = 1000,
#'                    iter_per_chkpt = 250,
#'                    path = path)
#' 
#' draws <- combine_chkpt_draws(object = fit2)
#' 
#' draws
#' }
combine_chkpt_draws <- function(object,...){
  
  if (is(object, "brmsfit")) {
    
    path <- object$path
    
  } else if (is(object, "chkpt_stan")) {
    
    path <- object$args$path
    
  } else {
    
    stop("class not supported. must be 'brmsfit' or 'chkpt_stan'.")
    
  }
  
  if (isFALSE(is.character(path))) {
    stop("path must be a character string.")
  }
 
  if (isFALSE(dir.exists(path))) {

    stop("directory not found.")
  }
  
  samp_files <- list.files(paste0(path, "/cp_samples/"))

  cp_order <- sort(as.numeric(gsub(".*?([0-9]+).*", "\\1", samp_files)))
  
  draws <- lapply(seq_along(samp_files) , function(x) {
    returned_object <-
      readRDS(file = paste0(paste0(path, "/cp_samples/samples_"),
                            cp_order[x],
                            ".rds"))$draws()
    return(returned_object)
  })

  draws_dim <- dim(draws[[1]])

  draws_per_cp <- draws_dim[1]

  chains <- draws_dim[2]

  draws_total <- draws_per_cp * length(draws)

  param_names <- dimnames(draws[[1]])$variable

  param_total <- length(param_names)

  draws_abind <- abind(draws, along = 1)

  draws_array <- array(
    data = 0,
    dim = c(draws_total, chains, param_total),
    dimnames = list(
      iteration = c(1:draws_total),
      chain = c(1:chains),
      variable = param_names
    )
  )

  for (i in seq_along(param_names)) {

    draws_array[, , i] <- draws_abind[, , param_names[i]]

  }

  class(draws_array) <- c("draws_array", 
                          "draws", 
                          "array")
  return(draws_array)

}
