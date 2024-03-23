## functions to automate collection of wikipedia articles --------

# search Wikipedia with search term
searchWikiFun <- function(term = NULL, limit = 100, title.only = TRUE, wordcount.min = 500, language = "en") {
  # API doc at https://www.mediawiki.org/wiki/API:Search
  term <- URLencode(term)
  url <- sprintf(paste0("https://", language, ".wikipedia.org/w/api.php?action=query&list=search&srsearch=%s&srlimit=%d&format=json"), term, limit)
  wiki_search_parsed <- jsonlite::fromJSON(url)$query$search
  wiki_search_parsed <- dplyr::filter(wiki_search_parsed, wordcount >= wordcount.min)
  if(title.only == FALSE) {
    return(wiki_search_parsed)
  } else{
    return(wiki_search_parsed$title)
  }
}

# search for categories and pages within these categories
searchWikiCatsFun <- function(pages = NULL, categories = NULL, language = "en", project = "wikipedia", limit = 100, output = c("pages", "cats"), subcats = FALSE, max.numcats = 5) {
  if(output == "cats") {
    # search categories
    cats <- lapply(pages, function(x) { try(categories_in_page(language, project, pages = x, clean_response = TRUE, limit = limit)[[1]]$categories$title, silent = TRUE)   }) %>% unlist %>% table %>% sort(decreasing = TRUE) %>% extract(1:max.numcats) %>% names() %>%  unique %>% extract(str_detect(., "^Category")) %>% str_replace("Category:", "")
    # identify subcategories in categories
    if(subcats == TRUE){
      subcats <- lapply(cats, function(x) { try(pages_in_category(language, project, categories = x, type = "subcat", limit = limit, clean_response = TRUE)$title, silent = TRUE)   }) %>% unlist %>% unique
      subcats <- subcats[str_detect(subcats, "^Category")] %>% str_replace("Category:", "")
      # combine categories
      cats_all <- c(cats, subcats) %>% unique
    }else{
      cats_all <- cats
    }
    return(cats_all)
  }
  if(output == "pages") {
    # find pages in all categories
    cats_all <- categories
    pages_in_cats <- lapply(cats_all, function(x) { try(pages_in_category(language, project, categories = x, type = "page", limit = limit, clean_response = TRUE)$title, silent = TRUE)   }) %>% unlist %>% unique
    # return output
    return(pages_in_cats)
  }
}

# quick selection of pages with minimum recent daily pageviews
pagesMinPageviews <- function(pages = NULL, start = "2016090100", end = "2016093000", min.dailyviewsavg = 50, project = "en.wikipedia") {
  pageviews_list <- list()
  pageviews_mean <- numeric()
  for (i in seq_along(pages)) {
    pageviews_list[[i]] <- try(article_pageviews(project = project, article = URLencode(str_replace_all(pages[i], " ", "_")), start = start, end = end, reformat = TRUE))
    pageviews_mean[i] <- try(mean(pageviews_list[[i]]$views, na.rm = TRUE))
  }
  pageviews_mean_df <- data.frame(page = pages, pageviews_mean = as.numeric(pageviews_mean), stringsAsFactors = FALSE)
  pages_minviews <- dplyr::filter(pageviews_mean_df, pageviews_mean >= min.dailyviewsavg) %>% extract2("page")
  return(pages_minviews)
}

# download pageview statistics with wikipediatrend package
pageviewsDownload <- function(package = "pageviews", pages = NULL, folder = "~", from = "2008-01-01", to = "2013-12-31", language = "en", return.failed.pages = FALSE, progress = TRUE) {
  pageviews_list <- list()
  pageviews_filenames_raw <- vector()
  for (i in seq_along(pages)) {
    if(progress == TRUE){
      pb <- txtProgressBar(min = 0, max = length(pages), style = 3)
      setTxtProgressBar(pb, i)
    }
    filename_cleaned <- pages[i] %>% str_replace_all("_", "")  %>% str_replace_all("/", "-")
    filename <- paste0(filename_cleaned, ".csv")
    pageviews_list[[i]]<-NA
    if (!file.exists(paste0(folder, filename))) {
      if(package == "wikipediatrend"){
        pageviews_list[[i]] <- try(wp_trend(URLencode(pages[i]), from = from, to = to, lang = language))
      }
      if(package == "pageviews"|class(pageviews_list[[i]])=="try-error"){
        pageviews_list[[i]] <- try(article_pageviews(project = paste0(language, ".wikipedia"), article = URLencode(str_replace_all(pages[i], " ", "_")), start = str_replace_all(paste0(from, "00"), "-", ""), end = str_replace_all(paste0(to, "00"), "-", ""), reformat = TRUE))
      }
      if(!(package %in% c("wikipediatrend", "pageviews"))){
        stop("Argument 'package' incorrectly specified.")
      }
      
      try(write.csv(pageviews_list[[i]], file = paste0(folder, filename), row.names = FALSE))
    }
  }
  if(return.failed.pages == TRUE){
    pages[sapply(pageviews_list, class) == "try-error"]
  }
}


# get outward links for vector of pages 
pages_links_out <- function(x, ...) {
  require(magrittr)
  cl <- makeCluster(4L)
  pblapply(x, 
           function(z) {
             require(magrittr)
             links <- WikipediR::page_links(page = z, 
                                            project = "wikipedia", 
                                            limit = 500, 
                                            clean_response = TRUE, 
                                            namespaces = 0, 
                                            ...)
             out <- lapply(links[[1]]$links, "[", "title") %>% 
               unlist %>% 
               as.character()
             out
           },
           cl = cl
  )
}

# get inward links for vector of pages 
pages_links_in <- function(x, ...) {
  require(magrittr)
  cl <- makeCluster(4L)
  pblapply(x, 
           function(z) {
             require(magrittr)
             links <- WikipediR::page_backlinks(page = z, 
                                                project = "wikipedia", 
                                                limit = 500, 
                                                clean_response = TRUE, 
                                                namespaces = 0, 
                                                ...)
             out <- lapply(links, "[", "title") %>% 
               unlist %>% 
               as.character()
             out
           },
           cl = cl
  )
}



# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout_with_fr) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

anomalies_plot<-function(page,from_date="01-01-2009",data,print=F)
{
  dat <- dplyr::select(dat, articles)
  dat$date <- index(pageviews_climate_xts)
  dat_gather <- tidyr::gather(dat, key = "page", value = "views", -date)
  
  # sort by popularity
  dat_gather$page <- factor(dat_gather$page,levels = unique(dat_gather$page))
  dat_gather$date<-as.POSIXct.Date(dat_gather$date)
  anomalies<-anomalies_climate_view[articles][[1]]$anoms
  anomalies$min<-anomalies$timestamp-as.difftime( 0, units="days" )
  anomalies$max<-anomalies$timestamp+as.difftime( 25, units="days" )
  
  p <- ggplot() +
    geom_rect(data=anomalies[anomalies$timestamp>from_date,],aes(xmin=anomalies$min[anomalies$timestamp>from_date], xmax=anomalies$max[anomalies$timestamp>from_date], ymin=-Inf, ymax=+Inf), fill="red", alpha=0.15)+
    geom_line(data = dat_gather[dat_gather$date>from_date,], aes(x = date, y = views),color = "#00AFBB", size = .5)+
    theme_ipsum_rc() +
    theme(strip.placement = "outside",
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          strip.text = element_text(size = 8),
          panel.spacing = unit(.5, "lines"),
          plot.margin=unit(c(.2,.2,.2,.2),"cm"))+
    xlab("Year") + ylab("Daily page views") +
    labs(title = articles)
  ggsave(p,file=paste0("../figures/",articles,"_anomalies.png"), width = 4, height = 2, limitsize = FALSE)
  
  if (print==T) {
    print(p)
  }
  return(p)
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


weekday_adapt <- function(x, factor = 1){
  out <-   ifelse(weekdays(x) == "Monday", 1*factor,
                  ifelse(weekdays(x) == "Tuesday", 5*factor,
                         ifelse(weekdays(x) == "Wednesday", 3*factor, 
                                ifelse(weekdays(x) == "Thursday", 6*factor, 
                                       ifelse(weekdays(x) == "Friday", 2*factor,
                                              ifelse(weekdays(x) == "Saturday", 7*factor,
                                                     4*factor))))))
  out
}