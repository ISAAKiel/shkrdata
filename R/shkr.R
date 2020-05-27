

#' \code{shkr2010_download} downloads the shkr database from 2010 from the JMA data exchange platform
#'
#' @param path path to the folder were the shkr database has to be stored
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return NULL
#' @export
#'
#' @examples
#' shkr2010_download()
shkr2010_download <- function(path = "."){
    download.file("https://www.jma.uni-kiel.de/en/research-projects/data-exchange-platform/data/shkr-db", paste(path, "/shkr-db.zip", sep=""))
    unzip(paste(path, "/shkr-db.zip", sep=""), exdir = path)

    file.rename(paste(path, "/db", sep=""), paste(path, "/shkr2010", sep=""))
    file.remove(paste(path, "/shkr-db.zip", sep=""))
}


#' \code{load_shkr2010} Load shkr data
#'
#' The function \code{load_shkr2010} loads shkr data from
#' the downloads shkr 2010 data set. It is required
#' to downloads the data from https://www.jma.uni-kiel.de/en/research-projects/data-exchange-platform/shkr
#' and unzip them.
#'
#' @param path Path to the downloads and unzips shkr folder
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return a list of tibbles of the different data.
#' @export
#'
#' @examples
#' shkr <- load_shkr2010()
load_shkr2010 <- function(path = "./shkr2010/"){
  require(readr)
  require(dplyr)
  loc_01 <- readr::read_delim(paste(path, "loc_01.csv", sep = ""), ";", col_types = cols(loc01_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_02 <- readr::read_delim(paste(path, "loc_02.csv", sep = ""), ";", col_types = cols(loc02_id = col_integer(), loc02_loc01_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_03 <- readr::read_delim(paste(path, "loc_03.csv", sep = ""), ";", col_types = cols(loc03_id = col_integer(), loc03_loc02_id = col_integer()),  escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_04 <- readr::read_delim(paste(path, "loc_04.csv", sep = ""), ";", col_types = cols(loc04_id = col_integer(), loc04_loc03_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_05 <- readr::read_delim(paste(path, "loc_05.csv", sep = ""), ";", col_types = cols(loc05_id = col_integer(), loc05_loc04_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_06 <- readr::read_delim(paste(path, "loc_06.csv", sep = ""), ";", col_types = cols(loc06_id = col_integer(), loc06_loc05_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_07 <- readr::read_delim(paste(path, "loc_07.csv", sep = ""), ";", col_types = cols(loc07_id = col_integer(), loc07_loc06_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_08 <- readr::read_delim(paste(path, "loc_08.csv", sep = ""), ";" , col_types = cols(loc08_id = col_integer(), loc08_loc07_id = col_integer()),  escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_09 <- readr::read_delim(paste(path, "loc_09.csv", sep = ""), ";", col_types = cols(loc09_id = col_integer(), loc09_loc08_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  loc_10 <- readr::read_delim(paste(path, "loc_10.csv", sep = ""), ";", col_types = cols(loc10_id = col_integer(), loc10_loc09_id = col_integer()), escape_double = FALSE, locale = locale(date_names = "de"), trim_ws = TRUE)
  thes_d <- readr::read_delim(paste(path, "thes_d.csv", sep = ""), ";", escape_double = FALSE, trim_ws = TRUE)
  thes_s<- readr::read_delim(paste(path, "thes_s.csv", sep = ""), ";", escape_double = FALSE, trim_ws = TRUE)

  grab  <- readr::read_delim(paste(path, "grab.csv", sep = ""),  ";", escape_double = FALSE, trim_ws = TRUE)
  huegel  <- readr::read_delim(paste(path, "huegel.csv", sep = ""),  ";", escape_double = FALSE, trim_ws = TRUE)
  siedl_komplex  <- readr::read_delim(paste(path, "siedl_komplex.csv", sep = ""),  ";", escape_double = FALSE, trim_ws = TRUE)
  siedl_objekt  <- readr::read_delim(paste(path, "siedl_objekt.csv", sep = ""),  ";", escape_double = FALSE, trim_ws = TRUE)

  loc_10 %>%
    dplyr::filter(!is.na(loc10_id)) %>%
    dplyr::filter(!is.na(loc10_loc09_id)) -> loc_10

  loc_09 %>%
    dplyr::filter(!is.na(loc09_id)) %>%
    dplyr::filter(!is.na(loc09_loc08_id)) -> loc_09

  loc_08 %>%
    dplyr::filter(!is.na(loc08_id)) %>%
    dplyr::filter(!is.na(loc08_loc07_id)) -> loc_08

  loc_07 %>%
    dplyr::filter(!is.na(loc07_id)) %>%
    dplyr::filter(!is.na(loc07_loc06_id)) -> loc_07

  loc_06 %>%
    dplyr::filter(!is.na(loc06_id)) %>%
    dplyr::filter(!is.na(loc06_loc05_id))  %>%
    dplyr::filter(!loc06_org_geloescht==TRUE)  -> loc_06

  loc_05 %>%
    dplyr::filter(!is.na(loc05_id)) %>%
    dplyr::filter(!is.na(loc05_loc04_id)) -> loc_05

  loc_04 %>%
    dplyr::filter(!is.na(loc04_id)) %>%
    dplyr::filter(!is.na(loc04_loc03_id)) -> loc_04

  loc_03 %>%
    dplyr::filter(!is.na(loc03_id)) %>%
    dplyr::filter(!is.na(loc03_loc02_id)) -> loc_03

  loc_02 %>%
    dplyr::filter(!is.na(loc02_id)) %>%
    dplyr::filter(!is.na(loc02_loc01_id)) -> loc_02

  loc_01 %>%
    dplyr::filter(!is.na(loc01_id)) -> loc_01

  return(list(loc_01,loc_02,loc_03,loc_04,loc_05,loc_06,loc_07,loc_08,loc_09,loc_10,thes_d,thes_s,grab,huegel,siedl_komplex,siedl_objekt))
}


#' \code{shkr_filter_loc10} filters certain types of artefacts
#'
#' The function shkr_filter_loc10 filters certain artefact types
#' from the table of artefacts. SHRK uses a facet classification.
#' Facet a contains material, facet b shape, facet c decoration technique
#' and facet d decoration elements. Inside the facets a
#' hierarchical classification is used. The classification includes
#' isomorph branches which have the same next feature at the same
#' place in different branches. The wild-card "." can be used to
#' filter for the same feature in different branches.
#'
#' @param data list of shkr tibbles as produced by \code{load_shkr2010}
#' @param a text string of material type to filter, starts with the letter A
#' @param b text string of shape type to filter, starts with the letter b
#' @param c text string of decoration technique type to filter, starts with the letter c
#' @param d text string of decoration element type to filter, starts with the letter d
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return tibble/dataframe containing the filtered loc_10 information
#' @export
#'
#' @examples
#' a <- "A"
#' b <- "B31"
#' c <- "C"
#' d <- "D"
#' shkr_fibulae <- shkr_filter_loc10(shkr, a, b, c, d)
shkr_filter_loc10 <- function(data, a = "A", b = "B", c = "C", d = "D"){
  require(stringr)
  require(magrittr)
  data[[10]] %>%
    dplyr::select(loc10_id, loc10_loc09_id, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b, loc10_typ_typ_c, loc10_typ_typ_d, loc10_text_beschr, loc10_text_bem, loc10_text_lit) %>%
    dplyr::filter(stringr::str_detect(loc10_typ_typ_a, a), stringr::str_detect(loc10_typ_typ_b, b), stringr::str_detect(loc10_typ_typ_c, c), stringr::str_detect(loc10_typ_typ_d, d)) ->
    artefacts
  return(artefacts)
}


#' \code{shkr_comp_loc10} completes information in loc10
#'
#' The function \code{shkr_comp_loc10} transferes information from
#' loc_01-loc_09 to loc_10 to make them directly available at loc_10
#' level
#'
#' @param loc10sel artefact tibble as produced by \code{shkr_filter_loc10}
#' @param data list of shkr tibbles as produced by \code{load_shkr2010}
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return a tibble based on but containing information from all
#' other loc levels in data in one object
#' @export
#'
#' @examples
#' shkr_fibulae_comp <- shkr_comp_loc10(shkr_fibulae, shkr)
shkr_comp_loc10 <- function(loc10sel, data){
  require(dplyr)
  require(magrittr)

  loc_01 <- data[[1]]
  loc_02 <- data[[2]]
  loc_03 <- data[[3]]
  loc_04 <- data[[4]]
  loc_05 <- data[[5]]
  loc_06 <- data[[6]]
  loc_07 <- data[[7]]
  loc_08 <- data[[8]]
  loc_09 <- data[[9]]
  loc_10 <- data[[10]]
  thes_d <- data[[11]]
  thes_s <- data[[12]]

  loc10sel  %>%
    dplyr::inner_join(loc_09, by = c("loc10_loc09_id" = "loc09_id")) %>%
    dplyr::select(loc10_id, loc10_loc09_id, loc09_loc08_id, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_typ_typ_c,loc10_typ_typ_d,loc10_text_beschr, loc10_text_bem, loc10_text_lit) %>%
    rename(loc09_id = loc10_loc09_id)   %>%
    dplyr::inner_join(loc_08, by = c("loc09_loc08_id" = "loc08_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc09_loc08_id, loc08_loc07_id, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit,loc08_typ) %>%
    rename(loc08_id = loc09_loc08_id)   %>%
    dplyr::inner_join(loc_07, by = c("loc08_loc07_id" = "loc07_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc08_loc07_id, loc07_loc06_id, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit,loc08_typ,loc07_typ) %>%
    rename(loc07_id = loc08_loc07_id) %>%
    dplyr::inner_join(loc_06, by = c("loc07_loc06_id" = "loc06_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc07_loc06_id, loc06_loc05_id, x, y, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit) %>%
    rename(loc06_id = loc07_loc06_id) %>%
    rename(x_fs = x)  %>%
    rename(y_fs = y)  %>%
    dplyr::inner_join(loc_05, by = c("loc06_loc05_id" = "loc05_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc06_id, loc06_loc05_id, loc05_loc04_id, x_fs, y_fs, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x, y) %>%
    rename(loc05_id = loc06_loc05_id) %>%
    rename(x_ort = x)  %>%
    rename(y_ort = y)  %>%
    dplyr::inner_join(loc_04, by = c("loc05_loc04_id" = "loc04_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc06_id, loc05_id, loc05_loc04_id,loc04_loc03_id, x_fs, y_fs, x_ort, y_ort, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, loc04_nam) %>%
    rename(loc04_id = loc05_loc04_id)  %>%
    dplyr::inner_join(loc_03, by = c("loc04_loc03_id" = "loc03_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc06_id, loc05_id, loc04_id, loc04_loc03_id, loc03_loc02_id, x_fs, y_fs, x_ort, y_ort, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, loc04_nam) %>%
    rename(loc03_id = loc04_loc03_id)  %>%
    dplyr::inner_join(loc_02, by = c("loc03_loc02_id" = "loc02_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc06_id, loc05_id, loc04_id, loc03_id, loc03_loc02_id, loc02_loc01_id, x_fs, y_fs, x_ort, y_ort, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, loc04_nam, loc02_nam) %>%
    rename(loc02_id = loc03_loc02_id)  %>%
    dplyr::inner_join(loc_01, by = c("loc02_loc01_id" = "loc01_id")) %>%
    dplyr::select(loc10_id, loc09_id, loc08_id, loc07_id, loc06_id, loc05_id, loc04_id, loc03_id, loc02_id, loc02_loc01_id, x_fs, y_fs, x_ort, y_ort, loc10_nam, loc10_typ_typ_a, loc10_typ_typ_b,loc10_text_beschr, loc10_text_bem, loc10_text_lit, loc08_typ,loc07_typ,loc06_nam, loc06_typ, loc06_dat_allg, loc06_wp, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, loc04_nam, loc02_nam, loc01_nam) %>%
    rename(loc01_id = loc02_loc01_id) ->
    loc_10_complete

  return(loc_10_complete)
}

#' \code{shkr_coord_trans} transferes place coordinates to the sites level
#' for unknown site locations
#'
#' The function \code{shkr_coord_trans} fills the gaps in the site locations.
#' In some cases the place is known but not the exat site location. Then
#' the place coordinates are used to fill the empty site coordinates.
#'
#' @param data artefact tibble as produced by \code{shkr_filter_loc10}
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return the same tibble provided as input but with completed coordinates
#' @export
#'
#' @examples
#' shkr_fibulae_comp_coord <- shkr_coord_trans(shkr_fibulae_comp)
shkr_coord_trans <- function(data){
  data$x_fs[is.na(data$x_fs)] <- data$x_ort[is.na(data$x_fs)]
  data$y_fs[is.na(data$y_fs)] <- data$y_ort[is.na(data$y_fs)]
  data$x_fs[data$x_fs == 0] <- data$x_ort[data$x_fs == 0]
  data$y_fs[data$y_fs == 0] <- data$y_ort[data$y_fs == 0]
  return(data)
}



#' \code{shkr_filter_region} filter a shkr tibble/dataframe to a certain spatial extent
#'
#' @param data shkr tibble as produced by \code{shkr_comp_loc10}
#' @param ftype filter type: "coords", "loc01", "loc02", "loc03", "loc04", "loc05", "loc06"
#' @param a integer loc_xx_id number
#' @param x_range vector: c(minimal, maximal)
#' @param y_range vector: c(minimal, maximal)
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return a tibble as used as input but restricted to a certain area
#' @export
#'
#' @examples
#' shkr_fibulae_comp_coord <- shkr_filter_region(shkr_fibulae_comp_coord)
shkr_filter_region <- function(data, ftype = "loc01", a = 11, x_range = c(0,3597713), y_range = c(0,5664752)){
  require(magrittr)

  if(ftype == "coords"){
    data %>%
      dplyr::filter(x_fs > x_range[1], x_fs < x_range[2],y_fs > y_range[1], y_fs < y_range[2]) ->
      fdata
  }

  if(ftype == "loc01"){
    data %>%
      dplyr::filter(loc01_id == a) ->
      fdata
  }

  if(ftype == "loc02"){
    data %>%
      dplyr::filter(loc02_id == a) ->
      fdata
  }

  if(ftype == "loc03"){
    data %>%
      dplyr::filter(loc03_id == a) ->
      fdata
  }

  if(ftype == "loc04"){
    data %>%
      dplyr::filter(loc04_id == a) ->
      fdata
  }

  if(ftype == "loc05"){
    data %>%
      dplyr::filter(loc05_id == a) ->
      fdata
  }

  if(ftype == "loc06"){
    data %>%
      dplyr::filter(loc06_id == a) ->
      fdata
  }

  return(fdata)
}


#' \code{shkr_filter_loc10_agg} filters returns only artefacts from graves, settlements or fortifications
#'
#' @param data artefact tibble as produced by \code{shkr_filter_loc10}
#' @param focus filter category, "graves", "settlements", "fortifications"
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return the same tibble provided as input but restricted to artefacts from specific aggregation types only
#' @export
#'
#' @examples
#' shkr_fibulae_agg <- shkr_filter_loc10_agg(shkr_fibeln_comp_coord)
shkr_filter_loc10_agg <- function(data, focus = "graves"){
  require(magrittr)

    fdata <- c()

  if(focus == "graves"){
  data %>%
    dplyr::filter(loc08_typ == 152 | loc08_typ == 153 | loc08_typ == 154) ->
    fdata
  }
  if(focus == "settlements"){
    data %>%
      dplyr::filter(loc08_typ == 150 | loc08_typ == 156 | loc08_typ == 157 | loc08_typ == 158) ->
      fdata
  }
  if(focus == "fortifications"){
    data %>%
      dplyr::filter(loc08_typ == 150) ->
      fdata
  }
  return(fdata)
}

#' \code{shkr_filter_dat} filter a shkr tibble/dataframe to a certain dating
#'
#' @param data shkr tibble as produced by \code{shkr_comp_loc10}
#' @param dating filter type: "dating", numerical according to thesaurus
#' @param range logical, TRUE: dating names all desired dates (default), FALSE: names undesired names
#'
#' @author Franziska Faupel <\email{ffaupel@@ufg.uni-kiel.de}>
#'
#' @return a tibble as used as input but restricted to a certain dating
#' @export
#'
shkr_filter_dat <- function(data, dating, range=TRUE){
    require(magrittr)
    if(range == TRUE){
        data %>%
            dplyr::filter(loc06_dat_allg %in% dating) ->
            fdata
    }

    if(range == FALSE){
        `%not_in%` <- purrr::negate(`%in%`)
        data %>%
            dplyr::filter(loc06_dat_allg %not_in% dating) ->
            fdata
    }

    return(fdata)
}

#' helper-function to normalize divides the values in a row by the sum of the values
#'
#' @param a tibble or dataframe
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return the same dataframe with normalized values
#'
#' @examples
#'
normalize <- function(a){a / sum(a, na.rm=TRUE)}


#' \code{shkr_ts} type spectra calculation
#'
#' The function \code{shkr_ts} calculates type spectra of types (=TS) being a contingency table with graves, sites or something else as rows and types as columns. TS are calculated for one typological facet only, since the comparison of two facets is difficult and might be biased. The aggregation level, the units for which the occurrence of types is counted can be "grave", "site", "place" and "agg". In case of "grave" loc_08_id is used as key and in case of "site" loc_06_id is used. The user has to make sure, that the used features are restricted to graves. In case of "aggr", the user has to create a column named aggr which is used for this purpose.
#'
#' @param features artefact tibble as produced by \code{shkr_filter_loc10}
#' @param facet typological facet, "a", "b", "c" or "d"
#' @param aggr aggregation level, "grave", "site", "place", aggr"
#' @param normal boolean, if TRUE, the contingency table will be normalized (sum of columns = 1)
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return type spectra (=contingency table) with rows = graves or sites and columns = types
#' @export
#'
#' @examples
#' ts <- shkr_ts(shkr_fibulae_graves)
shkr_ts <- function(features, facet = "b", aggr = "grave", normal = TRUE){
    require(moin)

    if(aggr == "grave"){
        nodes <- features$loc08_id
    }
    if(aggr == "site"){
        nodes <- features$loc06_id
    }
    if(aggr == "place"){
        nodes <- features$loc05_id
    }
    if(aggr == "aggr"){
        nodes <- features$aggr
    }

    if(facet == "a"){
        features_agg <- data.frame(type = features$loc10_typ_typ_a, nodes = nodes)
        graves_typelist <- moin::create_type_generator(features, type_col = "loc10_typ_typ_a", pre_size = 1)
        graves_type_spectra <- moin::create_typespectra(aggr_fea=features_agg, graves_typelist)
    }

    if(facet == "b"){
        features_agg <- data.frame(type = features$loc10_typ_typ_b, nodes = nodes)
        graves_typelist <- moin::create_type_generator(features, type_col = "loc10_typ_typ_b", pre_size = 1)
        graves_type_spectra <- moin::create_typespectra(aggr_fea=features_agg, graves_typelist)
    }

    if(facet == "c"){
        features_agg <- data.frame(type = features$loc10_typ_typ_c, nodes = nodes)
        graves_typelist <- moin::create_type_generator(features, type_col = "loc10_typ_typ_c", pre_size = 1)
        graves_type_spectra <- moin::create_typespectra(aggr_fea=features_agg, graves_typelist)
    }

    if(facet == "d"){
        features_agg <- data.frame(type = features$loc10_typ_typ_d, nodes = nodes)
        graves_typelist <- moin::create_type_generator(features, type_col = "loc10_typ_typ_d", pre_size = 1)
        graves_type_spectra <- moin::create_typespectra(aggr_fea=features_agg, graves_typelist)
    }

    if(normal == TRUE){
        graves_type_spectra[2:length(graves_type_spectra[1,])] <- t(apply(graves_type_spectra[2:length(graves_type_spectra[1,])], 1, normalize))
    }

    return(graves_type_spectra)
}



#' \code{shkr_cul_dist} calculates distance matrix
#'
#' @param ts spectrum of types as produced by \code{shkr_ts}
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' shkr_cul_dist is a wrapper of moin::dist_matr with method fixed to "euclidean"
#' @return distance matrix
#' @export
#'
#' @examples
#' dm <- shkr_cul_dist(ts)
shkr_cul_dist <- function(ts){
  require(moin)
  cul_dist <- moin::dist_matr(ts, method = "euclidean")
  return(cul_dist)
}


#' \code{shkr_net} creates igraph network from distance matrix
#'
#' @param dm distance matrix as produced by \code{shkr_cul_dist}
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return igraph object
#' @export
#'
#' @examples
#' snet <- shkr_net(dm)
shkr_net <- function(dm){
  require(igraph)
  diag(dm) <- 0
  shkr_graph <- igraph::graph_from_adjacency_matrix(dm, mode = "undirected", weighted = TRUE)
  return(shkr_graph)
}


#' \code{print_subtypes} prints subtypes of types
#'
#' The function print_subtypes prints and returns the subtypes of a type represented by the type-string containing a type code such as "B31". With the "descr" category it is also possible to submit a text-string, for which is searched in the type description.
#'
#' @param data shkr database as loaded by \code{load_shkr2010}
#' @param type-string type code such as "B31" or "A3" or "B31.1". The dot "." is used as wild-card representing one digit.
#' @param descr logical value, if TRUE the type-string is searched in the type description and not in the type code
#'
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#'
#' @return tibble of subtypes
#' @export
#'
#' @examples
#' print_subtypes(shkr, typestring = "B311.21", descr = FALSE)
#' print_subtypes(shkr, typestring = "Ompha", descr = TRUE)
print_subtypes <- function(data, typestring = "B", descr = FALSE){
    require(stringr)
    require(magrittr)
    if(descr == TRUE){
        data[[11]] %>%
            dplyr::select(d_klasse, d_text_beschr, d_name) %>%
            dplyr::filter(stringr::str_detect(d_text_beschr, typestring)) ->
            fdata
    } else {
        data[[11]] %>%
            dplyr::select(d_klasse, d_name, d_text_beschr) %>%
            dplyr::filter(stringr::str_detect(d_klasse, typestring)) ->
            fdata
    }
    #print(fdata)
    return(fdata)
}

#' \code{shkr_filter_loc06} filters certain types of sites
#'
#' The function shkr_filter_loc06 filters certain types of sites.
#'                      1 = archaeolgical object,
#'                      2 = depot,
#'                      3 = singular object,
#'                      4 = grave
#'                      5 = grvae/settlement/production
#'                      6 = infrastructure
#'                      7 = cult or religion
#'                      8 = settlement/production
#'                      9 = settlement/production/cult or religion
#'                      10 = others
#'                      12 = unknown
#'
#' @param data list of shkr tibbles as produced by \code{load_shkr2010}
#' @param type numeric vector of types of sites (see thesaurus above)
#'
#' @author Franziska Faupel <\email{ffaupel@@ufg.uni-kiel.de}>
#'
#' @return tibble/dataframe containing the filterd loc_06 information
#' @export
#'
#' @examples
#' type <- 4
#' shkr_grave <- shkr_filter_loc06(shkr, type)
#'
#' type <- c(5,8,9)
#' shkr_sett <- shkr_filter_loc06(shkr, type)
#'
shkr_filter_loc06 <- function(data, type){
    require(stringr)
    require(magrittr)
    data[[6]] %>%
        dplyr::select(loc06_id, loc06_loc05_id, loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, x, y) %>%
        dplyr::filter(loc06_typ == type) ->
        sites
    return(sites)
}

#' \code{shkr_filter_region_site} filter a shkr tibble/dataframe to a certain spatial extent
#'
#' @param data shkr tibble as produced by \code{load_shkr2010}
#' @param sites shkr tibble as produced by \code{shkr_filter_loc06}
#' @param ftype filter type: "coords", "loc01", "loc02", "loc03", "loc04", "loc05", "loc06"
#' @param a integer loc_xx_id number
#' @param x_range vector: c(minimal, maximal)
#' @param y_range vector: c(minimal, maximal)
#'
#' @author Franziska Faupel <\email{ffaupel@@ufg.uni-kiel.de}>
#'
#' @return a tibble as used as input but restricted to a certain area
#' @export
#'
#' @examples
#' type <- 4
#' shkr_grave <- shkr_filter_loc06(shkr, type)
#' shkr_grave <- shkr_filter_region_site(shkr, shkr_grave, ftype = "loc03", a=81)
shkr_filter_region_site <- function(data, sites ,ftype = "loc01", a = 11, x_range = c(0,3597713), y_range = c(0,5664752)){
    require(magrittr)
    loc_01 <- data[[1]]
    loc_02 <- data[[2]]
    loc_03 <- data[[3]]
    loc_04 <- data[[4]]
    loc_05 <- data[[5]]

    sites <- sites %>%
        rename(x_ort = x)  %>%
        rename(y_ort = y)  %>%
        dplyr::inner_join(loc_05, by = c("loc06_loc05_id" = "loc05_id")) %>%
        dplyr::select(loc06_id, loc06_loc05_id, loc05_loc04_id,loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x_ort, y_ort) %>%
        rename(loc05_id = loc06_loc05_id) %>%
        dplyr::inner_join(loc_04, by = c("loc05_loc04_id" = "loc04_id")) %>%
        dplyr::select(loc06_id, loc05_id, loc05_loc04_id, loc04_loc03_id, loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x_ort, y_ort) %>%
        rename(loc04_id = loc05_loc04_id) %>%
        dplyr::inner_join(loc_03, by = c("loc04_loc03_id" = "loc03_id")) %>%
        dplyr::select(loc06_id, loc05_id, loc04_id, loc04_loc03_id, loc03_loc02_id,loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x_ort, y_ort) %>%
        rename(loc03_id = loc04_loc03_id) %>%
        dplyr::inner_join(loc_02, by = c("loc03_loc02_id" = "loc02_id")) %>%
        dplyr::select(loc06_id, loc05_id, loc04_id, loc03_id, loc03_loc02_id, loc02_loc01_id, loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x_ort, y_ort) %>%
        rename(loc02_id = loc03_loc02_id) %>%
        dplyr::inner_join(loc_01, by = c("loc02_loc01_id" = "loc01_id")) %>%
        dplyr::select(loc06_id, loc05_id, loc04_id, loc03_id, loc02_id, loc02_loc01_id,loc06_nam, loc06_typ, loc06_dat_allg, loc06_huegelz, loc06_text_beschr, loc06_text_bem, loc06_text_lit, loc05_nam, x_ort, y_ort) %>%
        rename(loc01_id = loc02_loc01_id)

    if(ftype == "coords"){
        sites %>%
            dplyr::filter(x_fs > x_range[1], x_fs < x_range[2],y_fs > y_range[1], y_fs < y_range[2]) ->
            fdata
    }

    if(ftype == "loc01"){
        sites %>%
            dplyr::filter(loc01_id == a) ->
            fdata
    }

    if(ftype == "loc02"){
        sites %>%
            dplyr::filter(loc02_id == a) ->
            fdata
    }

    if(ftype == "loc03"){
        sites %>%
            dplyr::filter(loc03_id == a) ->
            fdata
    }

    if(ftype == "loc04"){
        sites %>%
            dplyr::filter(loc04_id == a) ->
            fdata
    }

    if(ftype == "loc05"){
        sites %>%
            dplyr::filter(loc05_id == a) ->
            fdata
    }
    return(fdata)
}

