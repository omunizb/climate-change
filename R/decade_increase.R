
#' Inter-decade Change in Emissions Plot
#'
#' \code{decade_increase} draws a plot of decade-to-decade change in total
#' greenhouse gas emissions (carbon dioxide, methane and nitrous oxide) for the
#' regions and decades provided as arguments. It also prints a table containing
#' average emissions for each country throughout the years that span the
#' decades provided in \code{decades} argument.
#'
#' @inheritParams plot_emissions
#' @param decades A character vector containing the decades that should be
#'   included in the plot, in the format \code{"1990-1999"}.
#'
#' @export
#'
#' @examples
#'
#' Greenhouse_Gas_Emissions <-
#' read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
#'
#' decade_increase(Greenhouse_Gas_Emissions,
#' c("1990-1999", "1980-1989"),
#' c("USA", "GBR"))
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
decade_increase <- function(df, decades, regions) {

  aggdec <- df %>%
    select(c(iso_code, year, co2, methane, nitrous_oxide)) %>%
    filter(iso_code %in% regions) %>%
    mutate(decade = paste0(year - year %% 10, "-", (year - year %% 10) + 9)) %>%
    rowwise %>%
    mutate(total_emissions = sum(co2, methane, nitrous_oxide, na.rm = T)) %>%
    # filter(decade %in% decades) %>%
    group_by(iso_code, decade) %>%
    summarise(total_emissions = sum(total_emissions)) %>%
    mutate(increase_prct = if_else(decade %in% decades,
                                   (lead(total_emissions)-total_emissions)*100/total_emissions,
                                   0)) %>%
    filter(decade %in% decades)

  # mutate(upper_year = max(year), lower_year = min(year)) %>%
  # filter(year %in% c(upper_year, lower_year)) %>%
  # group_by(iso_code, decade) %>%
  # mutate(increase_abs = if_else(year %in% lower_year,
  #                              -total_emissions,
  #                              total_emissions)) %>%
  # summarise(increase_abs = sum(increase_abs), year = lower_year)

  average <- df %>%
    select(c(iso_code, year, co2, methane, nitrous_oxide)) %>%
    filter(iso_code %in% regions) %>%
    mutate(decade = paste0(year - year %% 10, "-", (year - year %% 10) + 9)) %>%
    rowwise %>%
    mutate(total_emissions = sum(co2, methane, nitrous_oxide, na.rm = T)) %>%
    ungroup() %>%
    filter(decade %in% decades) %>% group_by(iso_code) %>%
    summarise(average = mean(total_emissions))

  print(average)

  # aggdec$increase_prct <-
  #   aggdec$increase_abs / df$co2[which(df$year %in% aggdec$year & df$iso_code %in% aggdec$iso_code)]

  ggplot2::ggplot(aggdec, ggplot2::aes(fill=decade, y=increase_prct, x=iso_code)) +
    ggplot2::geom_bar(position="dodge", stat="identity") +
    ggplot2::labs(x = "Region code", y="Change in greenhouse gas emissions (%)",
         title="Inter-decade change in total greenhouse gas emissions")
}
