#' Equivalencias de codigos de identificacion geografica
#'
#' @name full_geo_metadata
#' @docType data
#' @author Juan Pablo Ruiz Nicolini
#' @keywords data


# GET PROVINCE NAMES AND (ELECTORAL CODES)
provCode <- readr::read_csv("https://raw.githubusercontent.com/electorArg/PolAr_Data/master/geo/provCode.csv")

# GET ELECTORAL AND ADMISNITRATIVE CODES (PROVINCIAL AND DEPARTAMENTAL)
dine_indec <- readr::read_csv("https://raw.githubusercontent.com/electorArg/PolAr_Data/master/geo/dine_indec.csv") %>%
  dplyr::mutate(codprov = stringr::str_pad(codprov, 2, "left", 0),
                coddepto = stringr::str_pad(coddepto, 3, "left", 0),
                codprov_censo = stringr::str_pad(codprov_censo, 2, "left", 0),
                coddepto_censo = stringr::str_pad(coddepto_censo, 3, "left", 0))

### ISO codes

iso <- readr::read_csv("https://raw.githubusercontent.com/electorArg/PolAr_Data/master/geo/iso_codes.csv") %>%
  dplyr::rename(codprov = number,
                codprov_iso = code,
                name_iso = name)


# JOIN WITH PROVINCE NAMES
geo_metadata <- dine_indec %>%
  dplyr::left_join(provCode) %>%
  dplyr::left_join(iso)


### GET NAMES FOR GRIDS
usethis::use_data(geo_metadata, overwrite = TRUE, internal = TRUE)
