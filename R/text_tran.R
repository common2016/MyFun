#' Texts Translatation
#'
#' @param rawtext a text which is translated
#' @param appid from BaiDu
#' @param keyimp form BaiDu
#' @param from what language \code{rawtext} belong to, and default is \code{'en'} which is English
#' @param to the translated target language, and default is \code{'zh'} which is Chinese
#'
#' @details call BaiDu App to translate texts. See https://api.fanyi.baidu.com/product/113. Free
#' version is limited to calling one time by one second.
#' @importFrom magrittr '%>%'
#' @export
#' @examples
#'
#' # write your appid and key
#' # text_tran('apple',appid = '', keyimp = '')

text_tran <- function(rawtext, appid, keyimp, from = 'en', to = 'zh'){
  rmdnum <- stats::rnorm(1)
  ans <- paste(appid,rawtext,rmdnum,keyimp, sep = '')
  sn <- digest::digest(ans, algo = 'md5',serialize = FALSE)

  webadd <- paste('http://api.fanyi.baidu.com/api/trans/vip/translate?q=apple&from=en&to=zh&appid=',appid,
              '&salt=',rmdnum,'&sign=',sn,sep = '')
  return(RCurl::getURL(webadd) %>% RJSONIO::fromJSON() %>% .[['trans_result']])
}
