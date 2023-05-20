library(rvest)
library(tidyverse)

get_balance_sheet <- function(symbol, year, idName) {
  URL <-
    paste0(
      "https://s.cafef.vn/bao-cao-tai-chinh/",
      symbol,
      "/BSheet/",
      year,
      "/4/0/0/",
      idName,
      ".chn"
    )
  
  read_html(URL) |>
    html_node('table#tableContent') |>
    html_table(na.strings = '') |>
    select(1:5) |>
    rename(
      name = 1,
      Q1 = 2,
      Q2 = 3,
      Q3 = 4,
      Q4 = 5
    ) |>
    gather(key = "Quarter", value = "Value", 2:5) |>
    drop_na() |> 
    mutate(Quarter = paste0(year, "-", Quarter), Value = as.numeric(gsub(",", "", Value)))
}


get_all <- function(symbol, idName, year_range = 2015:2023) {
  for (year in year_range) {
    if (exists("DATA_RAW")) {
      DATA_RAW <- DATA_RAW |>  rbind(
        get_balance_sheet(symbol, year, idName)
      )
    } else {
      DATA_RAW <- get_balance_sheet(symbol, year, idName)
    }
  }
  
  DATA_RAW <- DATA_RAW |> 
    mutate(
      symbol = symbol
    )
  
  write.csv(DATA_RAW, paste0(symbol, ".csv"), row.names = F)
}

# Bluechip: VNM, VIC, VCB, FPT, MWG
get_all("VNM", "bao-cao-tai-chinh-cong-ty-co-phan-sua-viet-nam")
get_all("VIC", "ket-qua-hoat-dong-kinh-doanh-tap-doan-vingroup-cong-ty-co-phan")
get_all("VCB", "ket-qua-hoat-dong-kinh-doanh-ngan-hang-thuong-mai-co-phan-ngoai-thuong-viet-nam")
get_all("FPT", "bao-cao-tai-chinh-cong-ty-co-phan-fpt")
get_all("MWG", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-dau-tu-the-gioi-di-dong")

# Rr: HVN, UDC, MCG, NVL
get_all("HVN", "ket-qua-hoat-dong-kinh-doanh-tong-cong-ty-hang-khong-viet-nam-ctcp")
get_all("UDC", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-xay-dung-va-phat-trien-do-thi-tinh-ba-riavung-tau")
get_all("MCG", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-nang-luong-va-bat-dong-san-mcg")
get_all("NVL", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-tap-doan-dau-tu-dia-oc-no-va")

# Midcap: LDG, FCN, PVD, LCG, TPB
get_all("LDG", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-dau-tu-ldg")
get_all("FCN", "ket-qua-hoat-dong-kinh-doanh-cong-ty-co-phan-fecon")
get_all("PVD", "ket-qua-hoat-dong-kinh-doanh-tong-cong-ty-co-phan-khoan-va-dich-vu-khoan-dau-khi")
get_all("LCG", "bao-cao-tai-chinh-cong-ty-co-phan-lizen")
get_all("TPB", "bao-cao-tai-chinh-ngan-hang-thuong-mai-co-phan-tien-phong")
