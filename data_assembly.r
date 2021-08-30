# ------------------------------- Data assembly and treatment R script ------------------------------ 


# Libraries --------------------
library(readxl)   
library(fs)
library(openxlsx)
library(tidyverse)
library(fable)
library(dplyr)
library(openxlsx)
library(magrittr)
library(Cairo)
library(ggplot2)
library(lubridate)
library(scales)
library(caret)



################################################################################
########################## Read data    #######################################
################################################################################


read_df <- function(string_name) {
  #' Function to read and format csv files
  #'@param string_name: String, name of csv file
  #'
  df <- read.csv2(string_name, encoding="UTF-8")
  colnames(df) <- "raw"
  df %<>% as_tibble() 
   df %<>% mutate(år       =  as.numeric(sapply(strsplit(raw, ","), `[`, 1)),
                 frekvens = as.numeric(sapply(strsplit(raw, ","), `[`, 2)),
                 glattet_frekvens = slider::slide_dbl(frekvens, mean,
                                                    .before = 4, .after = 4, .complete = TRUE)) %>% 
     dplyr::select(år, frekvens, glattet_frekvens)
  return (df)
}


files <- dir_ls(path = "1810-2013 data", regexp = "csv") # Get file names of all .csv files


split_name <- function(x) {
  #' Splits a character string by '/' and '.'
  ((x %>% str_split(., "/"))[[1]][2] %>% strsplit(., ".", fixed = TRUE))[[1]][1]
}


manual_combination <- function(fileList, startyear) {
  #' Read files and combines timeseries into a single dataframe.
  #' The new dataframe is organized by rows (as a pivot_longer in tidyverse lingo)
  df <- NULL
  for(file in fileList) {
    df <- df %>% bind_rows(read_df(file) %>% mutate(søkeuttrykk = split_name(file))                  ) 
  }
  df %<>% 
    as_tibble() %>% 
    filter(år >= startyear) %>% 
    dplyr::select(søkeuttrykk, år, frekvens, glattet_frekvens)
  return (df)
}




total_df <- manual_combination(files, 1920) # Assemble into dataframe



## Pivot wider into easily accessible column-based dataset

wide_total_df <- total_df %>% dplyr::select(søkeuttrykk, år, frekvens) %>%  pivot_wider(names_from = "søkeuttrykk", 
                                        values_from = c("frekvens"))

## Export as csv file

write.csv(wide_total_df, "samlet_datasett.csv", row.names = FALSE)

## Export as excel file

openxlsx::write.xlsx(wide_total_df, file =  "samlet_datasett.xlsx", row.names = FALSE)
