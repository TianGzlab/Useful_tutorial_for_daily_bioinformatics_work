library(rvest)
library(tidyverse)

download_source_pkg = function(pkg_names, release = "3.18") {
  
  base_url = paste0("https://bioconductor.org/packages/", release,"/bioc/")
  
  html = read_html(base_url)
  avail_pkg = html_table(html) %>% pluck(1) %>% pull(Package)
  walk(pkg_names, function(pkg) {
    if (! pkg %in% avail_pkg) {
      warning(paste0(pkg, " is not found in the Bioconductor (release:", release, ")"), call. = FALSE, immediate. = TRUE)
    }
    else {
      pkg_url = paste0("https://bioconductor.org/packages/", release, "/bioc/html/", pkg, ".html")
      pkg_html = read_html(pkg_url)
      pkg_name = pkg_html %>% html_elements("td") %>% html_elements("a") %>% html_text2()
      pkg_name = pkg_name[str_detect(pkg_name, "\\.tar\\.gz")]
      
      pkg_url = paste0("https://bioconductor.org/packages/", release, "/bioc/src/contrib/", pkg_name)
      # download package
      download.file(pkg_url, destfile = pkg_name)
      message("Finish downloading ", pkg_name)
    }  
  }
  )
}


download_source_pkg(pkg_names = c("SingleR", "scmap"))


