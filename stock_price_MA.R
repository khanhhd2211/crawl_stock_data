# install.packages("devtools")
# devtools::install_github("phamdinhkhanh/VNDS", force = TRUE)
# library(VNDS)
library(tidyverse)
library(httr)
library(rvest)

#' @param v vector can convert
#' @export

###############################  chung cho cac ham ############################
removeBlankCol <- function(df){
  df[,which(unlist(lapply(df, function(x) sum(nchar(as.vector(x)))>0)))]
}
###############################  getBusinessReport ########################################
#Loai bo dau ',' giua cac chu so va convert ve dang numeric
convertNumber <- function(v) {
  str_replace_all(v,",","") %>% as.numeric()
}


############################# getPrice VNDIRECT ########################################
myexport <- function(...) {
  arg.list <- list(...)
  names <- all.names(match.call())[-1]
  for (i in seq_along(names)) assign(names[i],arg.list[[i]],.GlobalEnv)
}


dateChar <- function(dateTime){
  dateTime %<>%  as.Date() %>% format(.,format = "%d/%m/%Y")
  return(dateTime)
}

############################## getPrice CP 68 ########################################
#ham convertDate
convertDate <- function(dt){
  paste0(str_sub(dt,7,10),"-",str_sub(dt,4,5),"-",str_sub(dt,1,2))
}

#ham thay the dau comma
subComma <- function(v) {
  str_replace_all(v,",","")
}

#chuyen percent dang text sang numeric
convertPercent <- function(v){
  str_replace_all(v,"%","") %>% as.numeric()*0.01
}

tq_get_cafef <- function (symbol, from, to, ...) 
{
  url <- paste0("http://s.cafef.vn/Lich-su-giao-dich-", symbol, 
                "-1.chn")
  cname1 <- c("date", "adjusted", "close", "change.percent", 
              "match.volume", "match.value", "reconcile.volume", "reconcile.value", 
              "open", "high", "low", "volume.round1", "volume.round2", 
              "volume.round3")
  cname2 <- c("date", "adjusted", "close", "average", "change.percent", 
              "match.volume", "match.value", "reconcile.volume", "reconcile.value", 
              "reference", "open", "high", "low")
  symbolData <- matrix(nrow = 0, ncol = 14, byrow = TRUE, 
                       dimnames = list(c(), cname1))
  body <- list(`ctl00$ContentPlaceHolder1$scriptmanager` = "ctl00$ContentPlaceHolder1$ctl03$panelAjax|ctl00$ContentPlaceHolder1$ctl03$pager2", 
               `ctl00$ContentPlaceHolder1$ctl03$txtKeyword` = symbol, 
               `ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate1$txtDatePicker` = dateChar(from), 
               `ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate2$txtDatePicker` = dateChar(to), 
               `__EVENTTARGET` = "ctl00$ContentPlaceHolder1$ctl03$pager2", 
               `__EVENTARGUMENT` = 1, `__ASYNCPOST` = "true")
  resp <- POST(url, user_agent("Mozilla"), body = body, endcode = "form")
  tmp <- resp %>% read_html() %>% html_nodes(xpath = "//*[@id=\"GirdTable2\"]") %>% 
    html_table(fill = TRUE) %>% as.data.frame()
  if (nrow(tmp) != 0) {
    table_id <- "GirdTable2"
  }
  else {
    table_id <- "GirdTable"
  }
  for (i in 1:10000) {
    body <- list(`ctl00$ContentPlaceHolder1$scriptmanager` = "ctl00$ContentPlaceHolder1$ctl03$panelAjax|ctl00$ContentPlaceHolder1$ctl03$pager2", 
                 `ctl00$ContentPlaceHolder1$ctl03$txtKeyword` = symbol, 
                 `ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate1$txtDatePicker` = dateChar(from), 
                 `ctl00$ContentPlaceHolder1$ctl03$dpkTradeDate2$txtDatePicker` = dateChar(to), 
                 `__EVENTTARGET` = "ctl00$ContentPlaceHolder1$ctl03$pager2", 
                 `__EVENTARGUMENT` = i, `__ASYNCPOST` = "true")
    resp <- POST(url, user_agent("Mozilla"), body = body, 
                 endcode = "form")
    tmp <- resp %>% read_html() %>% html_nodes(xpath = glue::glue("//*[@id=\"{table_id}\"]")) %>% 
      html_table(fill = TRUE) %>% as.data.frame()
    if (nrow(tmp) == 2) {
      break
    }
    if (table_id == "GirdTable2") {
      if (ncol(tmp) == 14) 
        tmp <- data.frame(X1 = tmp[-c(1:2), c(1)], reference = NA, 
                          tmp[-c(1:2), -c(1, 4)])
      if (ncol(tmp) == 15) 
        tmp <- tmp[-c(1:2), c(-5)]
    }
    else {
      if (ncol(tmp) == 13) {
        tmp <- data.frame(tmp[-c(1:2), c(1:3)], average = NA, 
                          tmp[-c(1:2), c(4:13)])
      }
      else {
        tmp <- tmp[-c(1:2), ]
      }
    }
    if (!exists("convertDate", mode = "function")) {
      source("R/utils.R")
    }
    tmp[, 1] <- as.Date(convertDate(tmp[, 1]), format = "%Y-%m-%d")
    if (min(tmp[, 1]) > to) {
      next
    }
    if (max(tmp[, 1]) < from) {
      break
    }
    if (!exists("subComma", mode = "function")) {
      source("R/utils.R")
    }
    symbolData <- rbind(symbolData, tmp)
  }
  if (table_id == "GirdTable2") {
    colnames(symbolData) <- cname1
    symbolData <- data.frame(symbolData, row.names = symbolData[, 
                                                                1])
    symbolData <- symbolData[, c(1, 4, 9:11, 3, 2, 5, 7, 
                                 6, 8, 12:14)]
    symbolData[, 3:14] <- lapply(symbolData[, 3:14], subComma)
    symbolData[, 3:14] <- lapply(symbolData[, 3:14], as.numeric)
  }
  else {
    symbolData <- symbolData[, -6]
    colnames(symbolData) <- cname2
    symbolData <- data.frame(symbolData, row.names = symbolData[, 
                                                                1])
    symbolData[, c(6:9)] <- lapply(symbolData[, c(6:9)], 
                                   subComma)
    symbolData[, c(2:4, 6:13)] <- lapply(symbolData[, c(2:4, 
                                                        6:13)], as.numeric)
    symbolData <- symbolData[, c(1, 5, 11:13, 3:4, 2, 10, 
                                 6, 8, 7, 9)]
  }
  symbolData <- subset(symbolData, date >= from & date <= 
                         to)
  symbolData <- tibble::as_tibble(symbolData)
  cat(paste0("#", symbol, " from ", from, " to ", to, " already cloned \n"))
  invisible(symbolData)
}



get_data <- function (Symbol) {
  tq_get_cafef(
    symbol = Symbol,
    src = "CAFEF",
    from = '2015-01-01',
    to = '2023-05-19'
  ) |> 
  write.csv(paste0(Symbol, '.csv'), row.names = F)
    
}





# Bluechip: VNM, VIC, VCB, FPT, MWG
get_data("VIC")
get_data("VCB")
get_data("FPT")
get_data("MWG")


# Rr: HVN, UDC, MCG, NVL
get_data("HVN")
get_data("UDC")
get_data("MCG")
get_data("NVL")

# Midcap: LDG, FCN, PVD, LCG, TPB
get_data("LDG")
get_data("FCN")
get_data("PVD")
get_data("LCG")
get_data("TPB")