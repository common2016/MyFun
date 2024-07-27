#' @title Tidyicio
#'
#'
#'  @description This function is a function of the input-output table from the OECD
#'  involving China and Mexico in the processing trade part, re-combined into the original
#'  China and Mexico in the corresponding components of the table. The main operations include：
#'  1. Delete CHN and MEX from the official website's csv data
#'  2. Merge CN1, CN2 as CHN; MX1, MX2 as MEX, and put CHN and MEX a right positions.
#'  3. Merge tax subsidies into value added, and check if rows and columns correspond one to one.
#'
#' @param icio A rdata file from official websites
#' @param edition `2022` 2022 denotes the version of ICIO issued by OECD in 2022. `2021`
#' denotes the previous version of the former.
#' @return A modified ICIO.
#' @export
#' @examples
#' # load('/Users/yangnay/elements2/RawE/24_DataBase/投入产出表/OECD/1995_2020/oecdcsv.rdata')
#' # icio <- list()
#' # for (i in 1:length(oecdicio)) {
#' #  icio[[names(oecdicio)[i]]] <- tidyicio(oecdicio[[i]], edition = 2022)
#' #}
#' # oecdicio <- icio
#' # save(oecdicio, file = 'data-raw/icio2022.rdata')
#'
tidyicio <- function(icio, edition = 2021){
  icio <- plyr::rename(icio, c('V1' = 'sec')) %>% as.data.frame()

  if (edition == 2021){

    # 删掉CHN，MEX的行业，包括行和列。它们都是0.
    icio <- icio[,!str_detect(names(icio),'CHN_[0-9]')]
    icio <- icio[!str_detect(icio$sec,'CHN_[0-9]'),]
    icio <- icio[,!str_detect(names(icio),'MEX_[0-9]')]
    icio <- icio[!str_detect(icio$sec,'MEX_[0-9]'),]

    browser()

    # 合并CN1，CN2为CHN，MX1，MX2为MEX
    sec <- str_split_fixed(icio$sec,'_',2) %>% .[,2] %>% unique()
    sec <- sec[str_detect(sec,'^[A-S]|^T$')]
    for (i in 1:length(sec)) {
      aggcol <- paste('CN',as.character(1:2),'_',sec[i],sep = '')
      after <- paste('CHN_',sec[i],sep = '')
      icio <- aggsec(icio, aggcol = aggcol, after = after)
      aggcol <- paste('MX',as.character(1:2),'_',sec[i],sep = '')
      after <- paste('MEX_',sec[i],sep = '')
      icio <- aggsec(icio, aggcol = aggcol, after = after)
    }

    # CHN放在KHM_97T98和HRV01T02之间，MEX放在LUX_97T98和NLD_01T02之间
    icio <- cbind(select(icio, sec:LUX_97T98),
                  (select(icio, CHN_01T02:MEX_97T98) %>% select(contains('MEX'))), # 插入MEX
                  select(icio, NLD_01T02:KHM_97T98),
                  (select(icio, CHN_01T02:MEX_97T98) %>% select(contains('CHN'))), # 插入CHN
                  select(icio, HRV_01T02:TOTAL))

    icio <- c(1:which(icio$sec %in% 'LUX_97T98'),
              which(str_detect(icio$sec,'MEX_[0-9]')), # 插入行MEX
              which(icio$sec %in% 'NLD_01T02'):which(icio$sec %in% 'KHM_97T98'),
              which(str_detect(icio$sec,'CHN_[0-9]')), # 插入行CHN
              which(icio$sec %in% 'HRV_01T02'):which(icio$sec %in% 'OUTPUT')) %>% icio[.,]

    # 把税收补贴并入增加值
    ans <- colSums(icio[which(icio$sec %in% 'AUS_TAXSUB'):which(icio$sec %in% 'VALU'),-1]) %>%
      t() %>% as.data.frame() %>% cbind(data.frame(sec = 'VALU'),.)
    icio <- rbind(icio[1:which(icio$sec %in% 'ROW_97T98'),],
                  ans,
                  icio[icio$sec %in% 'OUTPUT',])

    # 检验行和列是否一一对应
    print(identical(icio$sec[1:which(icio$sec %in% 'ROW_97T98')],names(icio)[2:(which(icio$sec %in% 'ROW_97T98') + 1)]))
  }
  if (edition == 2022){

    # 删掉CHN，MEX的行业，包括行和列。它们都是0.
    icio <- icio[,!str_detect(names(icio),'CHN_[A-T]$|CHN_[A-T][0-9]')] # 注意避免把最终需求的CHN和MEX删掉
    icio <- icio[!str_detect(icio$sec,'CHN_[A-T]$|CHN_[A-T][0-9]'),]
    icio <- icio[,!str_detect(names(icio),'MEX_[A-T]$|MEX_[A-T][0-9]')]
    icio <- icio[!str_detect(icio$sec,'MEX_[A-T]$|MEX_[A-T][0-9]'),]

    # browser()
    # 合并CN1，CN2为CHN，MX1，MX2为MEX
    sec <- str_split_fixed(icio$sec,'_',2) %>% .[,2] %>% unique()
    sec <- sec[str_detect(sec,'^[A-S]|^T$')]
    for (i in 1:length(sec)) {
      aggcol <- paste('CN',as.character(1:2),'_',sec[i],sep = '')
      after <- paste('CHN_',sec[i],sep = '')
      icio <- aggsec(icio, aggcol = aggcol, after = after)
      aggcol <- paste('MX',as.character(1:2),'_',sec[i],sep = '')
      after <- paste('MEX_',sec[i],sep = '')
      icio <- aggsec(icio, aggcol = aggcol, after = after)
    }

    # CHN放在CHL_T和CIV_A01_02之间，MEX放在MAR_T和MLT_A01_02之间
    icio <- cbind(select(icio, sec:CHL_T),
                  (select(icio, CHN_A01_02:MEX_T) %>% select(contains('CHN'))), # 插入CHN
                  select(icio, CIV_A01_02:MAR_T),
                  (select(icio, CHN_A01_02:MEX_T) %>% select(contains('MEX'))), # 插入MEX
                  select(icio, MLT_A01_02:OUT))

    icio <- c(1:which(icio$sec %in% 'CHL_T'),
              which(str_detect(icio$sec,'CHN_[A-S]|CHN_T$')), # 插入行CHN
              which(icio$sec %in% 'CIV_A01_02'):which(icio$sec %in% 'MAR_T'),
              which(str_detect(icio$sec,'MEX_[A-S]|MEX_T$')), # 插入行MEX
              which(icio$sec %in% 'MLT_A01_02'):which(icio$sec %in% 'OUT')) %>% icio[.,]

    # 把税收补贴并入增加值
    ans <- colSums(icio[which(icio$sec %in% 'ARG_TLS'):which(icio$sec %in% 'VA'),-1]) %>%
      t() %>% as.data.frame() %>% cbind(data.frame(sec = 'VA'),.)
    icio <- rbind(icio[1:which(icio$sec %in% 'ROW_T'),],
                  ans,
                  icio[icio$sec %in% 'OUT',])

    # 检验行和列是否一一对应
    print(identical(icio$sec[1:which(icio$sec %in% 'ROW_T')],names(icio)[2:(which(icio$sec %in% 'ROW_T') + 1)]))
  }

  return(icio)
}
