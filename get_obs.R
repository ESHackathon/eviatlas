#' Wrangle data into observations
#
#' This function converts a nested variable, separated by a specified character,
#' to a long-form dataset, where each row is an observation.
#'
#' @param df A data frame.
#' @param var The variable to unnest as a character string.
#' @param new_var The name of the new variable as a character string.
#' @param sep The character that separates the values.
#'
#' @example
#' tibble(all_species =
#' c("birch; poplar",
#' "oak; birch; pine",
#' "",
#' "poplar",
#' "oak"),
#' all_habitats = c(
#'  "wetlands",
#'  "forest",
#'  "forest; wetlands",
#'  "grasslands",
#'  "grasslands; forest"
#' ),
#' id = letters[1:5]
#' ) %>%
#' get_obs("species", "species", ";") %>%
#' get_obs("all_habitats", "habitats", ";")


get_obs <- function(df, var, new_var, sep) {
    df %>%
      mutate(!!new_var := str_split(!!sym(var),
                                    paste0(sep, "[[:space:]]*"))) %>%
    unnest()
  }
