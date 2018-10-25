#' 過去 300 日分の株式情報を取得
#'
#' @param CODE numeric
#'
#' @return data.frame 過去 300 日分の株式情報
#'
#' @export
getLatest300 <- function(CODE){

  TABLES <- paste0("https://kabuoji3.com/stock/", CODE, "/") %>%
    xml2::read_html(.)

  # データ前半部分の取得と整形
  table1 <- TABLES %>%
    html_node(xpath = "//*[@id=\"base_box\"]/div/div[3]/div/div/div/table[1]") %>%
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

  # データ後半部分の取得と整形
  table2 <- TABLES %>%
    html_node(xpath = "//*[@id=\"base_box\"]/div/div[3]/div/div/div/table[2]") %>%
    html_text() %>%
    stringr::str_replace_all(., pattern = "\n", replacement = ",") %>%
    stringr::str_replace_all(., pattern = " ", replacement = "") %>%
    stringr::str_split(., pattern = ",") %>%
    extract2(1) %>%
    matrix(., nrow = 7) %>%
    t %>%
    data.frame(., stringsAsFactors = FALSE)

  names(table2) <- table2[1,]

  table2 <- table2[-c(1, nrow(table2)),]

  rownames(table1) <- NULL

  # データの結合と整形
  all <- rbind(table1, table2)

  rownames(all) <- NULL

  # 日付を POSIXct に変換
  all$"日付" <- all$"日付" %>% as.POSIXct()

  # 日付以外を numeric 型に変換
  all <- mutate_if(all, is.character, funs(as.numeric))

  # 古い順にソート
  all %<>% rownames %>% as.numeric %>% order(., decreasing = TRUE) %>% all[.,]
  rownames(all) <- NULL

  names(all) <- c("timeStamp", "Opening", "High", "Low", "Closing", "Yield", "Adjustment")

  return(all)
}
