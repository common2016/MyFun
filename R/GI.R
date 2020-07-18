#' Generalized IRF
#'
#' Compute GIRF of linear VAR by Koop et al. (1996)
#'
#' @param ma a list, it's MA coeffients from \code{ar2ma}
#' @param sig_u a cov matrix from VAR models. Note the order of variables in \code{sig_u}
#' is same with one of \code{ma[[i]]}.
#' @param imp_var a numerical scalor which specifies the impulse variable.
#' @return a data frame, its row is variables and its coloumn is horizons.
#' @import magrittr
#' @export
#' @examples
#' # see ar2ma function


GI <- function(ma, sig_u, imp_var = 1){

  GI_single <- function(ans, imp_var){
    ell <- matlab::zeros(nrow(ans),1)
    ell[imp_var] <- 1
    # GVAR toolbox p140, A.22
    return((ans %*% sig_u %*% ell)/(as.numeric(sqrt(t(ell) %*% sig_u %*% ell))))
  }

  irf <- lapply(ma, GI_single, imp_var = imp_var)
  ans <- dplyr::bind_cols(irf) %>% as.data.frame()
  row.names(ans) <- row.names(ma[[2]])
  return(ans)
}
