#' Translate English from Chinese
#'
#' @param a object from selenium module in Python. See examples.
#' @param rawtext Chinese which is translated
#' @param website don't change it.
#'
#' @examples
#' library(reticulate)
#' rawtext <- '我是中国人'
#' sl <- import('selenium')
#' drv <- sl$webdriver$Chrome()
#' cn2en(drv, rawtext)
#' drv$quit()


cn2en <- function(drv, rawtext, website = 'http://fanyi.youdao.com/'){
  # wait for 10s for any operation
  drv$implicitly_wait(10)
  drv$get(website)

  # Chinese to English
  drv$find_element_by_xpath('//*[@id="langSelect"]/span')$click()
  drv$find_element_by_xpath('//*[@id="languageSelect"]/li[2]/a')$click()

  # clear contents
  drv$find_element_by_xpath('//*[@id="inputOriginal"]')$clear()
  # input text
  drv$find_element_by_xpath('//*[@id="inputOriginal"]')$send_keys(rawtext)

  # get results
  Sys.sleep(2)
  tran_text <- drv$find_element_by_xpath('//*[@id="transTarget"]')$text

  # tran_text couldn't be null
  j <- T
  while (j) {
    if (nchar(tran_text) == 0){
      Sys.sleep(2)
      tran_text <- drv$find_element_by_xpath('//*[@id="transTarget"]')$text
    }else break
  }
  return(tran_text)
}
