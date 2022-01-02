#' Linear Prediction of Past Emissions Plots
#'
#' \code{lproj} draws plots with actual and predicted annual emissions since
#' 1990 to the latest year in the dataframe provided in \code{df} argument.
#' Three plots are provided in total, one for each of these greenhouse gases:
#' carbon dioxide, methane and nitrous oxide. The prediction is based on the
#' annual GDP, population and decade of the country or region of interest.
#'
#' @param df A dataframe containing annual carbon dioxide, methane and nitrous
#'   oxide emissions records by country or region. It must include variables
#'   \code{iso_code}, \code{year}, \code{co2}, \code{methane},
#'   \code{nitrous_oxide}, \code{population} and \code{gdp} where
#'   \code{iso_code} is a code uniquely identifying each region. The greenhouse
#'   gases emissions units should be in million tonnes of carbon
#'   dioxide-equivalents. One such dataframe can obtained from \emph{Our World
#'   in Data}: \url{https://github.com/owid/co2-data}.
#' @param region A country or region code that matches one of the
#'   \code{iso_code}s of \code{df}.
#'
#' @export
#'
#' @examples
#'
#' Greenhouse_Gas_Emissions <-
#' read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
#' lproj(df = Greenhouse_Gas_Emissions, region = "USA")
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
lproj <- function(df, region) {

  proj <- df %>% filter(iso_code == region) %>% rowwise() %>%
    mutate(decade = year - year %% 10) %>%
    mutate(decade = 1+(decade-1990)/10) %>%
    filter(decade %in% c(1:4))

  for(i in 1:ncol(proj)) {
    proj[ , i][[1]][is.na(proj[ , i][[1]])] <- mean(proj[ , i][[1]], na.rm = TRUE)
  }

  lmco2 <- lm(co2 ~ gdp + population + decade, proj)
  predco2 <- predict(lmco2)
  plot(x = proj$year, y = proj$co2, type="l", lwd = 2, xlab = "Year",
       ylab = "Carbon dioxide emissions (million tonnes)")
  lines(x = proj$year, y = predco2, type = "l", lty = 2, col = 2, lwd = 2)
  legend("topleft", col = c(1,2), lty = c(1,2), legend = c("data", "predicted"))

  lmmethane <- lm(methane ~ gdp + population + decade, proj)
  predmethane <- predict(lmmethane)
  plot(x = proj$year, y = proj$methane, type="l", lwd = 2, xlab = "Year",
       ylab = "Methane emissions (million tonnes)", col = 3)
  lines(x = proj$year, y = predmethane, type = "l", lty = 2, col = 2, lwd = 2)
  legend("topright", col = c(3,2), lty = c(1,2), legend = c("data", "predicted"))

  lmnitrous <- lm(nitrous_oxide ~ gdp + population + decade, proj)
  prednitrous <- predict(lmnitrous)
  plot(x = proj$year, y = proj$nitrous_oxide, type="l", lwd = 2, col = 4,
       xlab = "Year", ylab = "Nitrous oxide emissions (million tonnes)")
  lines(x = proj$year, y = prednitrous, type = "l", lty = 2, col = 2, lwd = 2)
  legend("top", col = c(4,2), lty = c(1,2), legend = c("data", "predicted"))
}
