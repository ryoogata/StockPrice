#' 指定した年の株式情報を取得
#'
#' @param CODE numeric
#' @param YEAR numeric
#'
#' @return data.frame 過去 300 日分の株式情報
#'
#' @export
getYearly <- function(CODE, YEAR){
  TABLES <- paste0("https://kabuoji3.com/stock/", CODE, "/", YEAR, "/") %>%
    xml2::read_html(.)

  table1 <- TABLES %>%
    html_node(xpath = "//*[@id=\"base_box\"]/div/div[4]/div/div/div/table") %>%
    html_text() %>%
    stringr::str_replace_all(., pattern = "\n", replacement = ",") %>%
    stringr::str_replace_all(., pattern = " ", replacement = "") %>%
    stringr::str_split(., pattern = ",") %>%
    extract2(1) %>%
    matrix(., nrow = 7) %>%
    t %>%
    data.frame(., stringsAsFactors = FALSE)

  names(table1) <- table1[1,]

  table1 <- table1[-c(1, nrow(table1)),]

  rownames(table1) <- NULL

  # 日付を POSIXct に変換
  table1$"日付" <- table1$"日付" %>% as.POSIXct()

  # 日付以外を numeric 型に変換
  table1 <- mutate_if(table1, is.character, funs(as.numeric))

  # 古い順にソート
  table1 %<>% rownames %>% as.numeric %>% order(., decreasing = TRUE) %>% table1[.,]
  rownames(table1) <- NULL

  names(table1) <- c("timeStamp", "Opening", "High", "Low", "Closing", "Yield", "Adjustment")

  return(table1)
}
