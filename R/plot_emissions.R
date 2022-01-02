#' Plot of annual greenhouse gas emissions
#'
#' \code{plot_emissions} draws a plot of annual greenhouse gas emissions for the
#' countries provided to the \code{regions} argument.
#'
#' The x-axis range of the plot goes from the earliest to the latest year with
#' available data for any of the regions provided to the \code{regions}
#' argument.
#'
#' If the number of regions requested is between two and ten, a legend is added
#' to the plot.
#'
#' If not provided or set to \code{NULL}, all of the regions in \code{df} are
#' included in the plot.
#'
#' @param df A dataframe containing annual carbon dioxide, methane and nitrous
#'   oxide emissions records by country or region. It must include variables
#'   \code{iso_code}, \code{year}, \code{co2}, \code{methane} and
#'   \code{nitrous_oxide}, where \code{iso_code} is a code uniquely identifying
#'   each region. The greenhouse gases emissions units should be in million
#'   tonnes of carbon dioxide-equivalents. One such dataframe can obtained from
#'   \emph{Our World in Data}: \url{https://github.com/owid/co2-data}.
#' @param regions A character vector containing the codes of countries and/or
#'   regions that should be included in the plot. The codes must match those of
#'   the \code{df} variable \code{iso_code}.
#' @param title The title of the plot. It is NULL by default.
#'
#' @export
#'
#' @examples
#' Greenhouse_Gas_Emissions <-
#' read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
#' plot_emissions(Greenhouse_Gas_Emissions,
#' c("USA", "CHN", "GBR", "RUS"),
#' "Emissions of historical global powers")
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate

plot_emissions <- function(df, regions = NULL, title = NULL) {

  total_emissions <- df %>% select(c(iso_code, country, year, co2, methane,
                                     nitrous_oxide))

  if(!is.null(regions)) {
    total_emissions <- total_emissions %>% filter(iso_code %in% regions)
  }
  else {
    total_emissions <- total_emissions %>% filter(iso_code != "OWID_WRL")
  }

  total_emissions <- total_emissions %>% rowwise() %>%
    mutate(emissions = sum(co2, methane, nitrous_oxide, na.rm = TRUE))

  plot(x = total_emissions[total_emissions$iso_code == unique(total_emissions$iso_code)[1],]$year,
       y = total_emissions[total_emissions$iso_code == unique(total_emissions$iso_code)[1],]$emissions, main = title, type = "l", col=1,
       xlim = c(min(total_emissions$year), max(total_emissions$year)),
       ylim = c(0, max(unlist(total_emissions$emissions))),
       ylab = "Total emissions (million tonnes)", xlab = "Year")

  if (length(unique(total_emissions$iso_code)) > 1) {
    for (c in 2:length(unique(total_emissions$iso_code))) {
      lines(x = total_emissions[total_emissions$iso_code == unique(total_emissions$iso_code)[c],]$year,
            y = total_emissions[total_emissions$iso_code == unique(total_emissions$iso_code)[c],]$emissions, type = "l", col=c)
    }
    if (length(unique(total_emissions$iso_code)) < 10) {
      legend(x = "topleft", legend = unique(unlist(total_emissions$iso_code)),
             col = c(seq(from = 1,
                         to = length(unique(total_emissions$iso_code)),
                         by = 1)),
             cex=4/length(unique(total_emissions$iso_code)), lwd = 2)
    }
  }
}
