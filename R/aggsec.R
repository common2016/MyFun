#' 按行按列合并制定的部门
#'
#' @param ans
#' @param aggcol 指定要合并的部门
#' @param after 合并后部门的名字

aggsec <- function(ans, aggcol = c('01T02','03','05T06','07T08','09'), after = 'A-B'){
  # browser()
  # 按列并
  ans <- ans[,aggcol] %>% rowSums() %>% cbind(dplyr::select(ans, -aggcol), .) %>% plyr::rename(c('.' = after))
  # 按行并
  sec <- ans$sec[!(ans$sec %in% aggcol)] %>% c(.,after)
  ans <- ans[ans$sec %in% aggcol,-1] %>% colSums() %>% rbind(ans[!(ans$sec %in% aggcol),-1],.) %>%
    cbind(sec,.)
  return(ans)
}
