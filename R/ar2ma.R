#' @title ar2ma
#' @description  Convert AR coefficients to MA coefficients
#' @param ar AR coefficients matrix which is k x kp dimenstion, k is numbers of variables,
#'   and no constant.
#' @param p lags orders of AR
#' @param n lags orders of MA generated
#' @param CharValue logitcal value, wheater compute character value
#' @details the formula is,
#'  \deqn{A_s = F_1 * A_{s-1} + F_2 * A_{s-2} + ... + F_p * A_{s-p}}
#'   where A is MA coeffiencts, F is AR coeffiencts.
#' @return a matrix which is MA coefficients
#' @import magrittr
#' @export
#' @examples
#' data(Canada, package = 'vars')
#' ar <- vars::Bcoef(vars::VAR(Canada, p = 2, type = "none"))
#' ar
#' ma <- ar2ma(ar, p = 2)
#' # GIRF
#' fit <- vars::VAR(Canada, p = 2, type = "none")
#' sig_u <- t(residuals(fit)) %*% residuals(fit)
#' GI(ma, sig_u, imp_var = 1)
#' # 预测第二个变量时，第一个变量所占比重
#' GFEVD(ma, sig_u, imp_var = 1, res_var = 2)

ar2ma <- function(ar,p,n = 11, CharValue = T){
  # get ar coefficiets
  Fmr <- list()
  In <- list() # eye matrix
  for (i in 1:p) {
    Fmr[[i]] <- ar[,(nrow(ar)*(i-1) + 1):(nrow(ar)*i)]
    if (i >= 2) In[[i - 1]] <- matlab::eye(nrow(ar))
    # construct Phi matrix(Hamilton, 1999, p293)
    ifelse (i == 1, phin <- Fmr[[i]], phin <- cbind(phin, Fmr[[i]]))
  }

  # compute F matrix to get its character value
  if (CharValue){
    if (length(In) == 1){
      Fchar <- rbind(phin,cbind(In[[1]],matlab::zeros(nrow(ar))))
    }else if(length(In) == 0){
      sprintf('The det AR is %f',det(ar)) %>% print()
      Fchar <- Fmr[[1]]
    }else  Fchar <- matlab::zeros(nrow(Matrix::bdiag(In)),ncol(Fmr[[1]])) %>%
        cbind(Matrix::bdiag(In),`.`) %>% rbind(phin,`.`)
    # show max characteristic root. If it is more than 1, the var is not stationary
    sprintf('The max characteristic root is %f',max(Mod(eigen(Fchar)$values))) %>% print()
    print(Mod(eigen(Fchar)$values))
  }
  # compute ma cofficients
  A <- list()
  A[['0']] <- matlab::eye(nrow(ar))
  for (i in 1:n) {
    A[[as.character(i)]] <- matlab::zeros(nrow(ar))
    j <- 1
    while (i - j >= 0 & j <= p) {
      A[[as.character(i)]] <- A[[as.character(i)]] + Fmr[[j]] %*% A[[as.character(i-j)]]
      j <-  j + 1
    }
  }
  return(A)
}
