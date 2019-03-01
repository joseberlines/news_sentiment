#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
# Functions used in scrape_stern.R to scrape content of www.stern.de.
# 
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



## function takes URL for sitemap and css selector for relevant links, returns
## data frame containing categories and urls

get_sitemap_urls <- function(url, css_sel, ua){
    
    ## print status message
    cat("Getting archive urls from `www.stern.de`…\n")
    
    ## get raw html
    req_obj <- GET(url, user_agent(ua))
    
    ## simply stop script at this point if http error
    stop_for_status(req_obj)
    
    ## extract archiv links
    archiv_urls_vec <- content(req_obj) %>%
        html_nodes(css_sel) %>% 
        html_attr("href") %>% 
        str_match(".*archiv/$") %>% 
        .[!is.na(.)]
    
    ## generate tibble of archive links with category variable
    archiv_urls_tibb <- tibble(categ = str_replace(archiv_urls_vec,
                                                   ".*/(\\w+)/archiv/",
                                                   "\\1"
                                                   ),
                               url = archiv_urls_vec
                               )
    
    archiv_urls_tibb
}





get_month_urls <- function(url, categ, css_sel, ua){
    
    ## print status message
    cat(glue("\n\nGetting month urls for archive `{categ}`…"))
    
    ## get raw html, extract links
    req_obj <- GET(url, user_agent(ua))
    
    ## simply stop script at this point if http error
    stop_for_status(req_obj)

    urls_vec <- content(req_obj) %>% 
        html_nodes(css_sel) %>% 
        html_attr("href")
    
    ## generate df of links, along with category and whether or not scraped
    ## (FALSE for everything at the moment)
    urls_df <- tibble(categ = categ, url = urls_vec, scraped = F)
    
    urls_df
}





get_content_urls <- function(req_obj){
    
    catchline_nodes <- content(req_obj) %>% 
        html_nodes(css_sel)
    
    cont_urls <- catchline_nodes %>% 
        html_node("a") %>% 
        html_attr("href")
    
    timestamps <- catchline_nodes %>% 
        html_nodes("time") %>% 
        html_attr("datetime") %>% 
        str_trim()
    
    catchlines <- catchline_nodes %>% 
        html_nodes(".a-headline") %>% 
        html_text() %>% 
        str_trim()
    
    cont_urls_df <- tibble(categ = ym_urls_df[[i, "categ"]],
                           url = cont_urls,
                           timestamp = timestamps,
                           catchline = catchlines,
                           scraped = F
                           )
    cont_urls_df
}





get_max_page <- function(req_obj){
    max_page <- content(req_obj) %>% 
        html_nodes(".m-pagination__number") %>% 
        html_text() %>% 
        str_trim()
    
    max_page <- ifelse(length(max_page) > 0,
                       as.integer(max_page[length(max_page)]), 0)
    
    max_page
}





get_content <- function(url, ua){
    
    ## get raw html and assign to cont
    t1 <- Sys.time()
    req_obj <- GET(url, user_agent(ua),
                   add_headers("accept-encoding" = "gzip, deflate, br",
                               "accept-language" = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7"
                               )
                   )
    t2 <- Sys.time()
    
    
    ## tdiff is gives request duration
    tdiff <- t2 - t1
    
    
    ## return only tdiff and error if error
    if (http_error(req_obj)){
        return(list(df = NULL, tdiff = tdiff, status = req_obj$status_code))
    }
    
    
    ## get out content
    cont <- content(req_obj)

    
    ## extract article text without section headings
    article <- cont %>% 
        html_nodes(".rtf-content-wrapper") %>% 
        html_nodes("p") %>% 
        html_text() %>% 
        paste(., collapse = "\n")
    
    article_intro <- cont %>% 
        html_node(".article-intro p") %>% 
        html_text()
    
    
    ## extract author, guest author, or press source, any of which may or may
    ## not exist
    author <- cont %>% 
        html_nodes(".name span") %>% 
        html_text()
    
    author_guest <- cont %>% 
        html_nodes(".guest-authors") %>% 
        html_text()

    press_source <- cont %>% 
        html_nodes(".article-content .a-text-block") %>% 
        html_text()
    
    
    ## extract datetime from `time` node with class
    datetime <- cont %>% 
        html_node(".a-datetime") %>% 
        html_attr("datetime")
    
    
    ## extract kicker and headline
    kicker <- cont %>% 
        html_node("#mainTitle") %>% 
        html_attr("data-kicker")
    
    headline <- cont %>% 
        html_node("#mainTitle") %>% 
        html_attr("data-headline")
    
    
    ## put data in dataframe and return
    out_df <- tibble(headline = ifelse(length(headline) > 0, headline, NA),
                     kicker = ifelse(length(kicker) > 0, kicker, NA),
                     datetime = ifelse(length(datetime) > 0, datetime, NA),
                     press_source = ifelse(length(press_source) > 0, press_source, NA),
                     author = ifelse(length(author) > 0, author, NA),
                     author_guest = ifelse(length(author_guest) > 0, author_guest, NA),
                     article_intro = ifelse(length(article_intro) > 0, article_intro, NA),
                     article = ifelse(length(article) > 0, article, NA)
                     )
    
    
    ## return dataframe, time difference (request length), and request status
    list(df = out_df, tdiff = tdiff, status = req_obj$status_code)
}