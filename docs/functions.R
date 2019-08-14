### R sitemap example, Jeroen Ooms, 2016
#
# This code demonstrates the new multi-request features in curl 2.0. It creates
# an index of all files on a web server with a given prefix by recursively following
# hyperlinks that appear in HTML pages.
#
# For each URL, we first perform a HTTP HEAD (via curlopt_nobody) to retrieve
# the content-type header of the URL. If the server returns 'text/html', then we
# perform a subsequent request which downloads the page to look for hyperlinks.
#
# The network is stored in an environment like this: env[url] = (vector of links)
#
# WARNING: Don't target small servers, you might accidentally take them down and
# get banned for DOS. Hits up to 300req/sec on my home wifi.

library(curl)
library(xml2)

stopifnot(packageVersion('xml2') >= 1.0)
stopifnot(packageVersion('curl') >= 2.0)

# Extracts hyperlinks from HTML page
get_links <- function(html, url){
  tryCatch({
    doc <- xml2::read_html(html)
    nodes <- xml2::xml_find_all(doc, "//a[@href]")
    links <- xml2::xml_attr(nodes, "href")
    links <- xml2:::url_absolute(links, url)
    links <- grep("^https?://", links, value = TRUE)
    links <- sub("#.*", "", links)
    links <- sub("/index.html$", "/", links)
    unique(sub("/$", "", links))
  }, error = function(e){character()})
}

crawl <- function(root, timeout = 300){
  total_visited = 0
  pages <- new.env()
  pool <- curl::new_pool(total_con = 50, host_con = 6, multiplex = TRUE)

  crawl_page <- function(url){
    pages[[url]] <- NA
    h <- curl::new_handle(failonerror = TRUE, nobody = TRUE)
    curl_fetch_multi(url, handle = h, pool = pool, done = function(res){
      total_visited <<- total_visited + 1
      if(!identical(url, res$url)){
        pages[[url]] = res$url
        url = res$url
      }
      headers <- curl::parse_headers(res$headers)
      ctype <- headers[grepl("^content-type", headers, ignore.case = TRUE)]
      cat(sprintf("[%d] Found '%s' @ %s\n", total_visited, ctype, url))
      if(isTRUE(grepl("text/html", ctype))){
        handle_setopt(h, nobody = FALSE, maxfilesize = 1e6)
        curl::curl_fetch_multi(url, handle = h, pool = pool, done = function(res){
          total_visited <<- total_visited + 1
          links <- get_links(res$content, res$url)
          cat(sprintf("[%d] Extracted %d hyperlinks from %s\n", total_visited, length(links), url))
          followlinks <- grep(paste0("^", root), links, value = TRUE)
          pages[[url]] <- followlinks
          lapply(followlinks, function(href){
            if(is.null(pages[[href]]))
              crawl_page(href)
          })
        }, fail = function(errmsg){
          cat(sprintf("Fail: %s (%s)\n", url, errmsg))
        })
      }
    }, fail = function(errmsg){
      cat(sprintf("Fail: %s (%s)\n", url, errmsg))
    })
  }
  crawl_page(root)
  curl::multi_run(pool = pool, timeout = timeout)
  as.list(pages)
}


#########

# Now sitemap from https://stackoverflow.com/questions/12255359/how-to-create-a-sitemap-xml-file-using-r-and-the-xml-package, user https://stackoverflow.com/users/340844/jverzani, modified by me to put in a fn and change schedule




GenerateSitemap <- function(base_url="https://www.brianomeara.info") {
  links <- unique(c(base_url, unname(unlist(crawl(base_url)))))
  links <- links[!is.na(links)]
  require(whisker)
  require(httr)

  tpl <- '
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
 {{#links}}
   <url>
      <loc>{{{loc}}}</loc>
      <lastmod>{{{lastmod}}}</lastmod>
      <changefreq>{{{changefreq}}}</changefreq>
      <priority>{{{priority}}}</priority>
   </url>
 {{/links}}
</urlset>
'



  map_links <- function(l) {
    tmp <- GET(l)
    d <- tmp$headers[['last-modified']]
    formatted_date <- NA
    try(formatted_date <- format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")))
    if(is.na(formatted_date)) {
      formatted_date <- "2019-08-14"
    }
    list(loc=l,
         lastmod=formatted_date,
         changefreq="monthly",
         priority=ifelse(grepl("pptx|pdf",l), "0.6", "0.8"))
  }

  links <- lapply(links, map_links)

  return(whisker.render(tpl))
}
