#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#
# functions used in spiegel_scraper.R -------------------------------------
#
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


extract_urls <- function(req_obj, medium) {
    ## extract only class `column-wide`, which contains all URLs
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
    
    # return
    out_df
}
