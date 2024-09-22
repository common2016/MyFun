#’ Generalized FEVD
#'
#' Compute GFEVD of linear VAR by Pesaran and Shin (1998)
#'
#' @inheritParams GI
#' @param res_var a numerical scalor which specifies the imponse variable.
#' @param cholesky logical value. Decomposition of prediction error variance
#' obtained using Cholesky decomposition or not?
#' @return a vector whose length are horizons.
#' @export
#' @examples
#' # see ar2ma function


GFEVD <- function(ma, sig_u, imp_var, res_var, cholesky = FALSE){
  # Pesaran and Shin (1998) P20
  if (cholesky){# 普通的正交冲击
    fz <- lapply(ma, function(MA) (MA %*% t(chol(sig_u)))^2) |>
      lapply(function(x, row, col) x[row,col], row = res_var, col = imp_var) |>
      unlist() |> cumsum()
  } else {
    fz <- lapply(ma, function(MA) (MA %*% sig_u)^2/sig_u[res_var, res_var]) |>
      lapply(function(x, row, col) x[row,col], row = res_var, col = imp_var) |>
      unlist() |> cumsum()
  }

  fm <- lapply(ma, function(MA) MA %*% sig_u %*% t(MA)) |>
    lapply(function(x, row, col) x[row,col], row = res_var, col = res_var) |>
    unlist() |> cumsum()
  return(fz/fm)
}

