library(tidyverse)



url <- 'https://github.com/PoliticaArgentina/data_warehouse/tree/master/geoAr/data_raw/censos'

## FAIL SAFELEY

check <- httr::GET(url)

httr::stop_for_status(x = check,
                      task = "Fail to download data. Source is not available // La fuente de datos no esta disponible")


# Get list of files from github data repo
pg <- xml2::read_html(url)


censos <- rvest::html_nodes(pg, "a") %>%
  rvest::html_attr(name = "href" ) %>%
  stringr::str_match('.*geojson') %>%
  stats::na.omit() %>%
  tibble::as_tibble(.name_repair = "minimal")  %>%
  dplyr::rename(name = 1) %>%
  dplyr::mutate(name = stringr::str_remove(name, pattern = "/PoliticaArgentina/data_warehouse/blob/master/geoAr/data_raw/censos/"),
                name = stringr::str_remove(name, pattern = ".geojson")) %>%
  dplyr::transmute(censo = as.integer(stringr::str_remove(name, pattern = "censo_")))







usethis::use_data(censos, overwrite = TRUE)
