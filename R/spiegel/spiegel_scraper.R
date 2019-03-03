#
#
#
#
#



# Libraries ---------------------------------------------------------------


# only install if necessary
#install.packages(c("httr", "rvest", "tidyverse", "glue", "lubridate"))


# load libraries
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)
library(glue)


source("spiegel_scraper_funcs.R")











# Get urls from archive ---------------------------------------------------
#
# Spiegel's archive is pretty straightforward, so URLs look like
# `http://www.spiegel.de/nachrichtenarchiv/artikel-01.01.2000.html`.
# 
# Rather than taking the extra step I did in `stern_scraper.R` of scraping the
# month-year links, here I'll just create a range of dates covering all possible
# dates, and then deal with errors as they arise.


if (!dir.exists("cont_urls_df_full/")){
    if (file.exists("spiegel_cont_urls_df.csv.gz")) {
        cont_urls_df <-
            read_csv2("spiegel_cont_urls_df.csv.gz", col_types = "ccccTc")
    } else {
        # load dataframe of links if one exists, otherwise generate a new one
        if (file.exists("spiegel_archive_urls_df.csv.gz")) {
            archive_urls_df <- read_csv("spiegel_archive_urls_df.csv.gz")
        } else {
            # dates for everyday between date given and today, for each medium,
            # formatted appropriately
            dates_art <-
                seq(as_date("2000-01-01"), today(), by = "day")
            dates_vid <-
                seq(as_date("2008-01-02"), today(), by = "day")
            dates_fot <-
                seq(as_date("2003-09-01"), today(), by = "day")
            
            
            # insert dates into URL
            urls_art <-
                paste0(
                    "http://www.spiegel.de/nachrichtenarchiv/artikel-",
                    format(dates_art, "%d.%m.%Y"),
                    ".html"
                )
            urls_vid <-
                paste0(
                    "http://www.spiegel.de/nachrichtenarchiv/videos-",
                    format(dates_vid, "%d.%m.%Y"),
                    ".html"
                )
            urls_fot <-
                paste0(
                    "http://www.spiegel.de/nachrichtenarchiv/fotos-",
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
            
            
            # add logical for scrape status and write to compressed CSV
            archive_urls_df$scraped <- F
            
            write_csv(archive_urls_df,
                      gzfile("spiegel_archive_urls_df.csv.gz"))
        }
        
        
        # initialize list to hold urls, headlines, etc.
        url_df_list <-
            vector(mode = "list", length = nrow(archive_urls_df))
        names(url_df_list) <- archive_urls_df$url
        
        
        # get list of user agents
        user_agents <- read_lines("../../user_agents.txt")
        
        
        # initialize variable to keep track of errors
        error_count <- 0
        
        
        for (i in 1:nrow(archive_urls_df)) {
            # continue if already scraped
            if (archive_urls_df[[i, "scraped"]]) {
                next
            }
            
            
            # get URL, print status, and send request
            url <- archive_urls_df[[i, "url"]]
            cat(glue("Scraping {i} of {nrow(archive_urls_df)}… "))
            
            t1 <- Sys.time()
            req_obj <- GET(url, user_agent(sample(user_agents, 1)))
            t2 <- Sys.time()
            
            
            # if there is an HTTP error, increment error count and continue to
            # next URL break with error message if there were 5 consecutive
            # errors
            if (http_error(req_obj)) {
                error_count <- error_count + 1
                
                if (error_count < 5) {
                    cat(
                        glue(
                            "Status {req_obj$status_code}. ",
                            "Request took {round(t2-t1, 2)} seconds. ",
                            "Continuing to next URL…"
                        ),
                        "\n"
                    )
                    next
                } else {
                    cat(
                        glue(
                            "Status {req_obj$status_code}. ",
                            "Request took {round(t2-t1, 2)} seconds. ",
                            "Too many errors, breaking…"
                        )
                    )
                    break
                }
            } else {
                # reset error count if request successful
                cat(
                    glue(
                        "Status {req_obj$status_code}. ",
                        "Request took {round(t2-t1, 2)} seconds. "
                    ),
                    "\n"
                )
                error_count <- 0
            }
            
            
            # extract URLs, titles, etc., then add medium and date from archive
            # URLs dataframe and add to URLs dataframes list, skip if data frame
            # is empty
            url_df <-
                extract_urls(req_obj, archive_urls_df[[i, "medium"]])
            
            
            # update scraped status
            archive_urls_df[[i, "scraped"]] <- T
            
            # skip rest if no content, otherwise add medium and date, and add to
            # list of dataframes
            if (is.null(url_df)) {
                next
            }
            
            url_df$medium <- archive_urls_df[[i, "medium"]]
            url_df$date <- archive_urls_df[[i, "date"]]
            url_df_list[[url]] <- url_df
            
            
            # save progress every 250 iterations
            if (i %% 250 == 0) {
                cat("\n\n Saving progress… \n\n")
                write_rds(url_df_list, "url_df_list.rds")
                write_rds(archive_urls_df, "archive_urls_df.rds")
            }
        }
        
        
        ## overwrite RDS files with compressed versions
        write_rds(url_df_list, "url_df_list.rds", compress = "gz")
        write_rds(archive_urls_df, "archive_urls_df.rds", compress = "gz")
        
        
        # remove unnecessary variables
        rm(t1, t2, i, url, error_count, req_obj, archive_urls_df)
    }
}











# Combine and clean up dataframes, write to disk by category --------------
#
# Bind list elements of url_df_list (dataframes) into single dataframe
# `cont_urls_df`.
#
# Clean up and get timestamps, categories, etc. Remove any content that isn't
# tied to `spiegel.de` domain (don't know why, but there's some non-Spiegel
# content mixed in).
#
# Drop km42 subdomain (i.e. km42.spiegel.de), which seems to be some kind of
# interactive map.


if (!dir.exists("cont_urls_df_full/")){
    # combine individual dataframes found in list url_df_list and clean up individual
    # columns
    cont_urls_df <- bind_rows(url_df_list) %>% 
        # get rid of parentheses, then separate into category and time
        mutate(categ_time = str_remove_all(categ_time, "[\\(\\)]")) %>% 
        separate(categ_time, c("categ", "time"), sep = ",") %>%
        # combine date and time and turn into timestamp
        unite(timestamp, c(date, time), sep = "") %>%
        mutate(timestamp = ymd_hm(timestamp, tz = "CET", quiet = T),
               categ = str_to_lower(categ),
               # only add domain where there isn't one
               url = ifelse(!str_detect(url, "^http"),
                            paste0("http://www.spiegel.de", url),
                            url
                            )
        ) %>% 
        # remove any non-Spiegel content and km42.spiegel.de content
        filter(str_detect(url, "spiegel\\.de"),
               !str_detect(url, "km42\\.spiegel")
               ) %>% 
        arrange(medium)
    
    
    # create directory to hold content urls by medium
    dir.create("cont_urls_df_full")
    
    
    # get out mediums and save dataframes by medium in directory
    medium_vec <- unique(cont_urls_df$medium)
    
    for (m in medium_vec) {
        fpath <- glue("cont_urls_df_full/spiegel_cont_urls_df_full_{m}.csv.gz")
        cat(glue("Writing `{fpath}`…"), "\n")
        cont_urls_df %>% 
            filter(medium == m) %>% 
            write_csv2(gzfile(fpath))
    }
    
    
    # remove superfluous variables
    rm(medium_vec, m, fpath, url_df_list)
}











# Take sample from category dataframes ------------------------------------
# 
# Remove some unnecessary bits like photo series, paid content, etc., and generate
# subset of URLs containing 1/6 of each category.

if (file.exists("cont_urls_df_samp.csv.gz")) {
    cont_urls_df_samp <-
        read_csv2("cont_urls_df_samp.csv.gz", col_types = "ccccTcl")
} else {
    if (!("cont_urls_df" %in% ls())) {
        
        cont_urls_df <- tibble()
        
        for (df in dir("cont_urls_df_full/")){
            cont_urls_df <- bind_rows(cont_urls_df,
                      read_csv2(paste0("cont_urls_df_full/", df),
                                col_types = "ccccTc"
                                )
                      )
        }
        
        rm(df)
    }
    
    
    # Drop any photo/video as they have little to no text content. Remove paid
    # content ("spiegel+") as well. Take sample as 1/6 of each category, then 
    # ungroup and shuffle dataframe. Then write sample dataframe to disk
    set.seed(3902)
    
    cont_urls_df_samp <- cont_urls_df %>% 
        filter(medium == "article" & categ != "spiegel+") %>% 
        group_by(categ) %>% 
        sample_frac(1/6) %>% 
        ungroup() %>% 
        sample_frac(1) %>% 
        mutate(scraped = F)
    
    write_csv2(cont_urls_df_samp, gzfile("cont_urls_df_samp.csv.gz"))
    
    rm(cont_urls_df)
}












# Scrape content ----------------------------------------------------------
# 
# Scrape content using subset of URL data.


