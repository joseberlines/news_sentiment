#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
#  
# 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# libraries ---------------------------------------------------------------
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()


# only install if necessary
#install.packages(c("httr", "rvest", "tidyverse", "glue", "lubridate"))


# load libraries
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)
library(glue)











#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# get urls from archive ---------------------------------------------------
# 
# Spiegel's archive is pretty straightforward, so URLs look like 
# `http://www.spiegel.de/nachrichtenarchiv/artikel-01.01.2000.html`.
# Rather than taking the extra step I did in stern_scraper.R of scraping the 
# month-year links, here I'll just create a range of dates covering all possible
# dates, and then deal with errors as they arise.
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()


# dates for everyday between date given and today, for each medium, formatted
# appropriately
dates_art <- seq(as_date("2000-01-01"), today(), by = "day") %>% 
    format("%d.%m.%Y")
dates_vid <- seq(as_date("2008-01-02"), today(), by = "day") %>% 
    format("%d.%m.%Y")
dates_fot <- seq(as_date("2003-09-01"), today(), by = "day") %>% 
    format("%d.%m.%Y")


# insert dates into URL
urls_art <- paste0("http://www.spiegel.de/nachrichtenarchiv/artikel-",
                   dates_art,
                   ".html"
                   )
urls_vid <- paste0("http://www.spiegel.de/nachrichtenarchiv/artikel-",
                   dates_vid,
                   ".html"
                   )
urls_fot <- paste0("http://www.spiegel.de/nachrichtenarchiv/artikel-",
                   dates_fot,
                   ".html"
                   )


# pack everything in a dataframe and remove above variables
urls_df <- bind_rows(tibble(medium = "article", url = urls_art),
                     tibble(medium = "video", url = urls_vid),
                     tibble(medium = "photo", url = urls_fot),
                     )

rm(dates_art, dates_vid, dates_fot, urls_art, urls_vid, urls_fot)


# 

for (date in date_range) {
    
}
