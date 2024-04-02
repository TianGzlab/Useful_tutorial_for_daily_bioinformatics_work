library(tidyverse)
library(rvest)
library(httr)

options(timeout = 1200)

download_webpathology_1 = function(section, time_out = 1200) {

  print("start download_webpathology_1")
  base_url = "https://www.webpathology.com/"
  html = base_url %>%
    httr::GET(., timeout(time_out)) %>%
    read_html()

  section_urls = html %>%
    html_elements("#category-group") %>%
    html_elements("a") %>%
    html_attr("href")

  section_urls = section_urls[!is.na(section_urls)]
  section_urls = paste0("https://www.webpathology.com", section_urls)

  target_url = switch(section,
         Neuropath = section_urls[1],
         Breast = section_urls[2],
         Head_Neck = section_urls[3],
         Mediastinum = section_urls[4],
         Peritoneum = section_urls[5],
         Genitourinary = section_urls[6],
         Cancer_Genomics = section_urls[7],
         HemPath = section_urls[8],
         Gynecologic = section_urls[9],
         Orthopedic = section_urls[10],
         DermPath = section_urls[11],
         Endocrine = section_urls[12],
         GI_Path = section_urls[13],
         Soft_Tissue = section_urls[14],
         Pulmonary = section_urls[15],
         Cardiac = section_urls[16],
         Infectious_Disease = section_urls[17],
         Pediatric = section_urls[18],
         CytoPath = section_urls[19],
         Genetic_Disorders = section_urls[20]
         )


 section_html = target_url %>%
   httr::GET(., timeout(time_out)) %>%
   read_html()

 section_sub_urls = section_html %>%
   html_elements(".list-titles") %>%
   html_elements("a") %>%
   html_attr("href")

 section_sub_names = section_html %>%
   html_elements(".list-titles") %>%
   html_elements("a") %>%
   html_text()

 section_sub_names = paste0(section, "/", section_sub_names)

 return(list(urls = section_sub_urls, names = section_sub_names))

}

download_webpathology_2 = function(urls, names, time_out = 1200) {

  print("start download_webpathology_2")

  base_url = "https://www.webpathology.com/"

  map2(urls,
       names,
       function(url1, name) {
        sub_html = paste0(base_url, url1) %>%
          httr::GET(., timeout(time_out)) %>%
          read_html()

        slide_urls = sub_html %>%
         html_elements(".list-group") %>%
         html_elements(".list-links") %>%
         html_elements("div") %>%
         html_elements("a") %>%
         html_attr("href")


       slide_counts = sub_html %>%
         html_elements(".slides-count") %>%
         html_elements("b") %>%
         html_text2() %>%
         str_extract("\\d+") %>%
         as.numeric()

       slide_urls = slide_urls[slide_counts != 0]

       slide_names = sub_html %>%
         html_elements(".list-group") %>%
         html_elements(".list-links") %>%
         html_elements(".case-title-truncate") %>%
         html_text2() %>%
         unique()

       slide_names = paste0(name, "/", slide_names)

       list(urls = slide_urls, names = slide_names)
     }
  )

}

download_webpathology_3 = function(ls, time_out = 1200) {
  print("start download_webpathology_3")
  lgl = map_lgl(ls, ~ length(.x[[1]]) != 0)
  ls = ls[lgl]
  df = map(ls, ~ data.frame(urls = .x[[1]], names = .x[[2]]))
  df = purrr::reduce(df, rbind)

  base_url = "https://www.webpathology.com/"

  map2(df$urls,
       df$names,
      function(url, name) {
         html = paste0(base_url, url) %>%
           httr::GET(., timeout(time_out)) %>%
           read_html()

         picture_urls = html %>%
           html_elements(".module_group") %>%
           html_elements(".module_case") %>%
           html_elements("a") %>%
           html_attr("href") %>%
           unique()

         picture_names = as.character(seq(length(picture_urls)))
         picture_names = paste0(name, "/", picture_names)

         list(urls = picture_urls, names = picture_names)
       }
      )

}

download_webpathology_4 = function(ls, time_out = 1200) {

  print("start download_webpathology_4")


  base_url = "https://www.webpathology.com/"
  lgl = map_lgl(ls, ~ length(.x[[1]]) != 0)
  ls = ls[lgl]
  df = map(ls, ~ data.frame(urls = .x[[1]], names = .x[[2]]))
  df = purrr::reduce(df, rbind)

  map2(df$urls,
       df$names,
       function(url, name) {

         html = paste0(base_url, url) %>%
           httr::GET(., timeout(time_out)) %>%
           read_html()

         p_site =  html %>%
           html_elements("#image-main") %>%
           html_elements("img") %>%
           html_attr("src")

         p_site = p_site[str_detect(p_site, ".*\\.(jpg|png|tiff)")]
         p_name = p_site %>% str_split("/") %>% pluck(1)
         p_name = p_name[length(p_name)]

         dir.create(name, recursive = T)

         # write url
         try(
           write_lines(
             paste0(base_url, p_site),
             file = paste0(name, "/", str_remove(p_name, "\\.(jpg|png|tiff)"),
                                       "_url.txt"),
             sep = "")
           )

         signal = try(
           html %>%
           html_elements("#image") %>%
           html_elements("p") %>%
           html_text2()
         )

         if(class(signal) != "try-error") {

           text = html %>%
             html_elements("#image") %>%
             html_elements("p") %>%
             html_text2()

            # write comments
           try(
               write_lines(
                 text,
                 file = paste0(name, "/", str_remove(p_name, "\\.(jpg|png|tiff)"), ".txt"),
                 sep = "")
             )

         }


         #p_url = paste0(base_url, p_site)
         #download.file(p_url, destfile = paste0(name, "/", p_name), timeout = timeout)

       }
       )
}


sections = c("Neuropath", "Breast", "Head_Neck", "Mediastinum",
             "Peritoneum", "Genitourinary", "Cancer_Genomics",
             "HemPath", "Gynecologic", "Orthopedic",
             "DermPath", "Endocrine", "GI_Path",
             "Soft_Tissue", "Pulmonary", "Cardiac",
             "Infectious_Disease", "Pediatric",
             "CytoPath", "Genetic_Disorders")

# download picture urls and the corresponding text comments
map(sections,
    function(section) {
      test1 = download_webpathology_1(section) # 只需要改NeuroPath
      test2 = download_webpathology_2(test1$urls, test1$names)
      test3 = download_webpathology_3(test2)
      test4 = download_webpathology_4(test3) # 真正下载的步骤
    }
    )



# summary the downloading result
real_number = c(351, 581, 512, 205, 123,
                2248, 33, 1518, 1169, 626,
                821, 494, 1839, 1539, 492,
                245, 314, 16, 12, 0)

download_number = map_int(sections, ~ length(list.files(.x, ".+url.+", recursive = T)))

summary_table = tibble(type = sections,
                       real_number = real_number,
                       download_number = download_number)

write_csv(summary_table, "summary.csv")

# write all picture urls into one file
urls = map(sections, ~ list.files(.x, ".+url.+", recursive = T, full.names = T))
urls = unlist(urls)
urls2 = map_chr(urls, ~ read_lines(.x))
urls3 = map_chr(urls2, ~ str_replace(.x, "https://www.webpathology.com//", "https://www.webpathology.com/"))

write_lines(urls3, "all_urls.txt")

# get the file path of each picture url
url_table = tibble(
  location = map_chr(urls, function(x) {
    res = str_split(x, "/") %>% pluck(1)
    res = res[1:(length(res)-1)]
    return(str_flatten(res, collapse  = "/"))
  }),
  url = urls3
)

write_csv(url_table, "loc2url.csv")

# calculate how many text comments are not be scraped
# due to encoding problem
comments = map(sections, ~ list.files(.x, ".txt", recursive = T, full.names = T))
comments = unlist(comments)
comments = comments[! str_detect(comments, "_url")]

sum(real_number) - length(comments)
