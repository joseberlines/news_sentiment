#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
# functions used in spiegel_scraper.R -------------------------------------
#
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/





# Extracts URLs from archive pages. Used in spiegel_scraper.R.
extract_urls <- function(req_obj, medium) {
    # extract only class `column-wide`, which contains all URLs
    column_wide <- content(req_obj) %>% html_node(".column-wide")
    
    # extract link nodes, then extract titles and urls from link nodes,
    # unique necessary for titles and urls because videos and photos use multiple
    # `a` nodes, just return NULL if there are no link nodes
    if (medium %in% c("video", "photo")){
        link_nodes <- column_wide %>% html_nodes(".article-title a")
    } else {
        link_nodes <- column_wide %>% html_nodes("a")
    }
    
    if (length(link_nodes) == 0) {return(NULL)}
    
    titles <- link_nodes %>% html_attr("title")
    urls <- link_nodes %>% html_attr("href")
    
    
    
    # extract blurbs below video links (if there)
    vid_intros <- column_wide %>%
        html_nodes(".teaser") %>%
        html_text() %>%
        str_trim() %>% 
        str_replace("mehr...$", "") %>% 
        str_extract("\n.*") %>% 
        str_trim()
    
    if (length(vid_intros) == 0) {
        vid_intros <- NA_character_
    }

    # extract category and time (if there)
    categ_times <- column_wide %>%
        html_nodes(".headline-date") %>%
        html_text() %>%
        str_trim()
    
    if (length(categ_times) == 0) {
        categ_times <- NA_character_
    }
    
    
    # pack variable in dataframe and return
    out_df <- tibble(
        title = titles,
        url = urls,
        vid_intro = vid_intros,
        categ_time = categ_times
    )
    
    # return dataframe
    out_df
}











extract_content <- function(raw_article) {
    
    # drop HTML using regex so <i> remains, which indicates source
    article <- raw_article %>% 
        html_nodes("#js-article-column p") %>% 
        as.character() %>% 
        # remove paragraph HTML
        str_remove_all("</?p>") %>% 
        str_trim() %>% 
        # remove section headings
        str_remove("^<b>.*</b>$") %>% 
        # remove link HTML
        str_remove_all("</?a.*?>") %>% 
        # drop empty strings
        .[nchar(.) > 0]
    
    
    # Return NULL if no content.
    if (length(article) == 0) {
        return(NULL)
    }
    
    
    # If final element of article vector contains <i> it is a source. Assign to
    # source, then subset article to exclude source. Source is otherwise NA.
    if (str_detect(article[length(article)], "</?i>")){
        source <- str_remove_all(article[length(article)], "</?i>")
        article <- article[1:(length(article)-1)]
    } else {
        source <- NA_character_
    }
    
    
    # Collapse article into single character string with line breaks.
    article <- paste(article[1:(length(article)-1)], collapse = "\n")

    
    # Extract headline, headline intro, article intro, timestamp, and author.
    headline <- raw_article %>% 
        html_nodes(".headline") %>% 
        html_text() %>% 
        paste(., collapse = "\n")
    
    headline_intro <- raw_article %>% 
        html_nodes(".headline-intro") %>% 
        html_text()
    
    article_intro <- raw_article %>% 
        html_nodes(".article-intro") %>% 
        html_text()
    
    timestamp <- raw_article %>% 
        html_nodes(".timeformat") %>% 
        html_attr("datetime") %>% 
        as_datetime(tz = "CET")
    
    author <- raw_article %>% 
        html_nodes(".author") %>% 
        html_text()
    
    
    # Put all extracted into list, iterate over list and replace anything useless
    # with NA, then collpase into dataframe and return
    elem_list <- list(headline = headline,
                      headline_intro = headline_intro,
                      article_intro = article_intro,
                      article = article,
                      source = source,
                      timestamp = timestamp,
                      author = author
                      )
    
    for (n in names(elem_list)) {
        elem <- elem_list[[n]]
        if(length(elem) == 0 || nchar(elem) == 0 || is.na(elem)){
            elem_list[[n]] <- NA_character_
        }
    }
    
    cont_df <- bind_cols(elem_list)
    
    # return dataframe
    cont_df
    
}

