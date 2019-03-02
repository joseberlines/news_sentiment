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


# create data frame of links if one doesn't already exist, otherwise generate 
# a new one
if (file.exists("spiegel_archive_urls_df.csv.gz")) {
    archive_urls_df <- read_csv("spiegel_archive_urls_df.csv.gz")
} else {
    # dates for everyday between date given and today, for each medium,
    # formatted appropriately
    dates_art <- seq(as_date("2000-01-01"), today(), by = "day")
    dates_vid <- seq(as_date("2008-01-02"), today(), by = "day")
    dates_fot <- seq(as_date("2003-09-01"), today(), by = "day")
    
    
    # insert dates into URL
    urls_art <-
        paste0(
            "http://www.spiegel.de/nachrichtenarchiv/artikel-",
            format(dates_art, "%d.%m.%Y"),
            ".html"
        )
    urls_vid <-
        paste0(
            "http://www.spiegel.de/nachrichtenarchiv/artikel-",
            format(dates_vid, "%d.%m.%Y"),
            ".html"
        )
    urls_fot <-
        paste0(
            "http://www.spiegel.de/nachrichtenarchiv/artikel-",
            format(dates_fot, "%d.%m.%Y"),
            ".html"
        )
    
    
    # pack everything in a dataframe
    archive_urls_df <-
        bind_rows(
            tibble(
                medium = "article",
                date = dates_art,
                url = urls_art
            ),
            tibble(
                medium = "video",
                date = dates_vid,
                url = urls_vid
            ),
            tibble(
                medium = "photo",
                date = dates_fot,
                url = urls_fot
            )
        )
    
    
    # shuffle observations → I'm not sure, but I think if scrape is too
    # sequential it might more likely become target of rate limiting
    archive_urls_df <- sample_frac(archive_urls_df, 1)
    
    
    # remove everything not needed anymore
    rm(dates_art,
       dates_vid,
       dates_fot,
       urls_art,
       urls_vid,
       urls_fot)
    
    
    # write to compressed CSV
    write_csv(archive_urls_df,
              gzfile("spiegel_archive_urls_df.csv.gz"))
}


# 
html_raw_list <- vector(mode = "list", length = nrow(urls_df))

for (i in 1:nrow()) {
    
}
