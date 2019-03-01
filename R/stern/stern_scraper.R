#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
# This script first scrapes urls, and then content from www.stern.de. Functions
# are sourced from scrape_stern_funcs.R. 
# 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# libraries ---------------------------------------------------------------
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()

## only install if necessary
#install.packages(c("httr", "rvest", "tidyverse", "glue"))

## load libraries
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)
library(glue)

## load functions
source("stern_scraper_funcs.R")

## load user agents
user_agents <- read_lines("../../user_agents.txt")





#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# get archive URLs from sitemap -------------------------------------------
# 
# get_sitemap_urls function is from script stern_scraper_funcs.R,
# pass in URL for sitemap + css selector for relevant links
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()

## only run if no saved URLs for Stern content
if (!file.exists("cont_urls_df.rds")){
    url <- "https://www.stern.de/sonst/stern-de-sitemap-3346806.html"
    css_sel <- ".rte--list a"
    
    archive_urls_df <- get_sitemap_urls(url, css_sel, sample(user_agents, 1))
}





#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# get year-month urls -----------------------------------------------------
# 
# get urls covering all available years and months for each category,
# getting available URLs is more efficient than incrementing until break,
# function month_urls_df can be found in stern_scraper_funcs.R
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()


## only run if no saved URLs for Stern content
if (!file.exists("cont_urls_df.rds")){
    ## initialize empty list corresponding to URLs in archive_urls_df, initialize
    ## CSS selector for node extraction
    ym_urls_list <- vector(mode = "list", length = nrow(archive_urls_df))
    names(ym_urls_list) <- archive_urls_df$categ
    css_sel <- ".month a"
    
    
    
    ## download and extract available years/months for each category and add to 
    ## list
    for (i in 1:nrow(archive_urls_df)){
        
        month_urls_df <- get_month_urls(archive_urls_df[[i, "url"]],
                       archive_urls_df[[i, "categ"]],
                       css_sel,
                       sample(user_agents, 1)
                       )
        
        ym_urls_list[[archive_urls_df[[i, "categ"]]]] <- month_urls_df
    }
    
    
    
    ## bind data frames found in list ym_urls_list and add pageNum= to URL strings
    ym_urls_df <- bind_rows(ym_urls_list)
    ym_urls_df$url <- paste0(ym_urls_df$url, "&pageNum=")
}






#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# scrape content URLs -----------------------------------------------------
# 
# content URLs are URLs from ym_urls_df *plus* offset, goal is to get all content
# URLs for each month of each year, get_content_urls function can be found in 
# stern_scraper_funcs.R
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()

## only run if no saved URLs for Stern content
if (!file.exists("cont_urls_df.rds")){
    ## set up empty list to hold category urls, initialize CSS selector for content
    ## URLs
    cont_df_list <- vector(mode = "list", length = nrow(ym_urls_df))
    names(cont_df_list) <- ym_urls_df$url
    css_sel <- ".o-teaser-catchline"
    iter_num <- 0
    
    
    
    ## scrape HTML for pages 0 through n and parse content URLs
    for (i in 1:nrow(ym_urls_df)){
        
        ## just skip to next url if already scraped
        if (ym_urls_df[[i, "scraped"]]){
            next
        }
        
        
        ## get category and month + year from url
        categ <- ym_urls_df[[i, "categ"]]
        month_year <- str_extract(ym_urls_df[[i, "url"]], "month=\\d+&year=\\d+") %>% 
            str_replace("&", " ")
    
            
        ## print status
        cat(glue(paste0("\nGetting content URLs:",
                        "\n - {categ}\n - {month_year}\n - 0 \n - {tdiff}"
                        )
                 ),
            "\n"
            )
        
        ## get url from ym_urls_df, add offset 0 (first page), and scrape
        url <- paste0(ym_urls_df[[i, "url"]], 0)
        req_obj <- GET(url, user_agent(sample(user_agents, 1)))
        
        ## extract urls from page 0
        cont_urls_df <- get_content_urls(req_obj)
        
        ## extract highest page from pagination node
        max_page <- get_max_page(req_obj)
        
        ## if there is pagination set up empty list for each page and enter new 
        ## for-loop to get each page
        if (max_page > 1){
            
            ## set up new empty list and add content from page 0 (from above)
            page_urls_df <- vector(mode = "list", length = max_page)
            page_urls_df[[1]] <- cont_urls_df
            
            
            ## for loop to get remaining pages when pagination > 0, pagination is 
            ## 0-indexed, so subtract one from max_range beforehand, loop speed 
            ## adapts to request speed
            for (ii in 1:(max_page-1)) {
                
                
                
                ## print status
                cat(glue(paste0("\nGetting content URLs:\n",
                                " - {categ}\n - {month_year}\n - {ii}\n - {tdiff}"
                                )
                         ),
                    "\n"
                    )
    
                
                ## get url from ym_urls_df, add offset ii, and scrape
                url <- paste0(ym_urls_df[[i, "url"]], ii)
                
                t1 <- Sys.time()
                req_obj <- GET(url, user_agent(sample(user_agents, 1)))
                t2 <- Sys.time()
    
                           
                ## end time for request
                tdiff <- t2 - t1
                
                ## extract urls from page ii
                cont_urls_df <- get_content_urls(req_obj)
                page_urls_df[[ii+1]] <- cont_urls_df
    
            }
        } else{
            page_urls_df <- list(cont_urls_df)
        }
        
        
        ## add data frame list page_urls_df to cont_df_list
        cont_df_list[[ym_urls_df[[i, "url"]]]] <- bind_rows(page_urls_df)
        
        ## mark category as scraped
        ym_urls_df[[i, "scraped"]] <- T
    }
    
    
    
    ## turn character strings found in timestamp into datetimes, then save as csv,
    ## note that setting timezone as CET will cause write_csv2 to write as UTC, i.e.
    ## one hour will be deducted from each datetime
    cont_urls_df <- bind_rows(cont_df_list) %>% 
        mutate(timestamp = ymd_hm(timestamp, tz = "CET"))
    
    
    
    ## generate UUIDs using random alpha-numeric string and datetime as integer
    uuid <- vector(mode = "character", length = nrow(cont_urls_df))
    
    uuid <- sapply(uuid,
           function (x) {
               paste(sample(c(0:9, letters, LETTERS), 11, replace = T), collapse = "")
           }
           )
    
    cont_urls_df$uuid <- paste(as.integer(cont_urls_df$timestamp), uuid, sep = "-")
    
    
    write_rds(cont_urls_df, "cont_urls_df.rds", compress = "gz")
}






#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# scrape content ----------------------------------------------------------
# 
# contet URLs are from cont_df_list
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()


## load from csv if not already in environment
if(!("cont_urls_df" %in% ls()) && !dir.exists("cont_df_list/")) {
    
    ## data frame containing urls, titles, scraped status, etc.
    cont_urls_df <- read_rds("cont_urls_df.rds")
    
    ## updated scraped status (saved as separate rds because it's the only part
    ## of cont_urls_df that's updated in loop, and saving whole data frame would
    ## be slower)
    cont_urls_df$scraped <- read_rds("scraped.rds")
}



## if cont_df_list.rds exists, i.e. some scraping has already been done, simply
## load this, otherwise initialize empty list to hold parsed data, each element
## will contain dataframe with exactly one row
if (!dir.exists("cont_df_list/")) {
    if (file.exists("cont_df_list.rds")) {
    cont_df_list <- read_rds("cont_df_list.rds")
} else {
    ## initialize empty list and error count
    cont_df_list <- vector(mode = "list", length = nrow(cont_urls_df))
    error_count <- 0
    
    ## scrape content using URLs from cont_urls_df, change scraped status to
    ## TRUE when scraped
    for (i in 1:nrow(cont_urls_df)) {
        
        ## skip if already scraped, makes it easier to stop and start where you
        ## left off
        if (cont_urls_df[[i, "scraped"]]){
            next
        }
        
        ## in while loop GET request made max 5 times, then breaks with error
        while(TRUE){
            ## function get_content sends GET request, parses data, and return
            ## data frame along with bool for request error and tdiff giving
            ## request duration
            cont_req_list <- get_content(cont_urls_df[[i, "url"]],
                                         sample(user_agents, 1)
                                         )
    
                    
            if (http_error(cont_req_list$status) && 
                cont_req_list$status != 404 &&
                error_count < 4) {
                error_count <- error_count + 1
            } else if (http_error(cont_req_list$status) && error_count == 4) {
                error_count <- error_count + 1
                break
            } else {
                error_count <- 0
                break
            }
        }
        
        if (error_count > 4) {
            cat("\n\nThere's a problem. Breaking…")
            break
        }
        
        ## print status
        cat(glue("Processed {i} of {nrow(cont_urls_df)}",
             " ({round((i / nrow(cont_urls_df))*100, 1)}%)",
             " - Status {cont_req_list$status}",
             " - Request duration {round(cont_req_list$tdiff, 2)}"), "\n")
        
        ## give new data frame the appropriate UUID, then add to cont_df_list and
        ## mark url in cont_urls_df as scraped == TRUE
        cont_req_list$df$uuid <- cont_urls_df[[i, "uuid"]]
        cont_df_list[[i]] <- cont_req_list$df
        cont_urls_df[[i, "scraped"]] <- TRUE
        
        ## save every 300 requests in case of trouble
        if (i%%300 == 0){
            cat("\nSaving…\n\n")
            write_rds(cont_urls_df$scraped, "scraped.rds")
            write_rds(cont_df_list, "cont_df_list.rds")
        }
    }
    
    
    ## finial write should be compressed to save space
    write_rds(cont_df_list, "cont_df_list.rds", compress = "gz")
    
    
    ## remove some variables
    rm(error_count, scraped)
}
    
    
    ## initialize start and end vectors for list chunks
    end <- ceiling(seq(40000, length(cont_df_list), length.out = 10))
    start <- c(0, end[1:(length(end)-1)]) + 1
    
    
    ## create directory for data frame list of content if it doesn't exist
    if(!dir.exists("cont_df_list/")) {
        dir.create("cont_df_list")
    }
    
    
    ## 
    for (i in 1:length(start)) {
        file_name <- paste0("cont_df_list/cont_df_list_",
                            formatC(i, width = 2, format = "d", flag = "0"),
                            ".rds"
        )
        cat(glue("Saving `{file_name}`…"), "\n")
        write_rds(cont_df_list[start[i]:end[i]], file_name, compress = "gz")
    }
    
    
    ## remove full list (too big to keep)
    file.remove("cont_df_list.rds")
}


## remove user agents
rm(user_agents)






#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()
#
# combine and clean content -----------------------------------------------
# 
# cont_df_list is bound by row in cont_df, then joined with cont_urls_df to 
# make complete data frame.
# 
# strings are cleaned up (remove leading and trailing whitespace) and a couple
# of superfluous variables are removed
# 
#-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()-()


##
if (!dir.exists("cont_df/")) {
    
    ## initialize empty cont_df_list and append each list chunk in directory 
    ## `cont_df_list`
    cont_df_list <- list()
    for (f in dir("cont_df_list")) {
        cat(glue("Reading `{f}`…"), "\n")
        cont_df_list <- c(cont_df_list, read_rds(paste0("cont_df_list/", f)))
    }
    
    rm(f)
    
    
    ## load dataframe of content urls
    cont_urls_df <- read_rds("cont_urls_df.rds")
    
    
    ## combine and clean up content data frame
    cont_df <- bind_rows(cont_df_list) %>% 
        right_join(cont_urls_df, ., by = "uuid") %>% 
        mutate(catchline = str_trim(catchline),
               catchline = ifelse(nchar(catchline) == 0, NA, catchline),
               headline = str_trim(headline),
               headline = ifelse(nchar(headline) == 0, NA, headline),
               kicker = str_trim(kicker),
               kicker = ifelse(nchar(kicker) == 0, NA, kicker),
               press_source = str_trim(press_source),
               press_source = ifelse(nchar(press_source) == 0, NA, press_source),
               author = str_trim(author),
               author = ifelse(nchar(author) == 0, NA, author),
               author_guest = str_trim(author_guest),
               author_guest = ifelse(nchar(author_guest) == 0, NA, author_guest),
               article_intro = str_trim(article_intro),
               article_intro = ifelse(nchar(article_intro) == 0, NA, article_intro),
               article = str_trim(article),
               article = ifelse(nchar(article) == 0, NA, article)
               ) %>% 
        select(-scraped, -datetime)
    
    
    ## create directory for dataframes of content by year, add year variable to 
    ## content dataframe
    dir.create("cont_df/")
    cont_df$year <- year(cont_df$timestamp)
    
    for (y in sort(unique(cont_df$year))) {
        
        ## subset dataframe for year, given by y
        sub_df <- cont_df %>%
            filter(year == y) %>% 
            select(-year)
        
        ## print status and save dataframe subset
        cat(glue("Writing CSV for year {y}…"), "\n")
        write_csv2(sub_df, gzfile(glue("cont_df/cont_df_{y}.csv.gz")))
    }
    
    rm(sub_df, cont_df_list, cont_urls_df, y)
}