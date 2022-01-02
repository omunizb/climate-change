#' Prediction of Future Emissions Plots
#'
#' \code{lproj} draws plots with actual past and projected future annual
#' emissions from 1990. Three plots are provided in total, one for each of these
#' greenhouse gases: carbon dioxide, methane and nitrous oxide. The prediction
#' is based on the annual GDP, population and decade of the country or region of
#' interest.
#'
#' The range of the actual past emissions curve goes from 1990 until the last
#' year of the dataframe provided to \code{df} argument. The projected emissions
#' curve ranges from the earliest to the latest year in the \code{future}
#' dataframe.
#'
#' @inheritParams lproj
#' @param future A dataframe containing projected GDP and population data for
#'   the region provided as argument to \code{region}. It must contain the
#'   variables \code{year}, \code{population} and \code{gdp}. One such dataframe
#'   for US, \code{futdata}, is included in the package.
#'
#' @export
#'
#' @examples
#'
#' data(futdata)
#' Greenhouse_Gas_Emissions <-
#' read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
#' emissions_pred(Greenhouse_Gas_Emissions, "USA", futdata)
#'
emissions_pred <- function(df, region, future) {
  proj <- df %>% filter(iso_code == region) %>% rowwise() %>%
    mutate(decade = year - year %% 10) %>%
    mutate(decade = 1+(decade-1990)/10) %>%
    filter(decade %in% c(1:4))

  futdec <- future %>% rowwise() %>%
    mutate(decade = year - year %% 10) %>%
    mutate(decade = 1+(decade-1990)/10)

  for(i in 1:ncol(proj)) {
    proj[ , i][[1]][is.na(proj[ , i][[1]])] <- mean(proj[ , i][[1]], na.rm = TRUE)
  }

  lmco2 <- lm(co2 ~ gdp + population + decade, proj)
  predco2 <- predict(lmco2)
  futpredco2 <- c(tail(predco2, n=1),predict(lmco2, newdata = futdec))

  plot(x = proj$year, y = predco2,
       type="l", lwd = 2, xlab = "Year",
       xlim = c(min(proj$year), max(futdec$year)),
       ylab = "Carbon dioxide emissions (million tonnes)")
  lines(x = c(max(proj$year),futdec$year), y = futpredco2, type = "l", lty = 2, col = 2, lwd = 2)

  lmmethane <- lm(methane ~ gdp + population + decade, proj)
  predmethane <- predict(lmmethane)
  futpredmethane <- c(tail(predmethane, n=1),predict(lmmethane, newdata = futdec))

  plot(x = proj$year, y = predmethane,
       type="l", lwd = 2, xlab = "Year",
       ylim = c(min(c(predmethane,futpredmethane)), max(c(predmethane,futpredmethane))),
       xlim = c(min(proj$year), max(futdec$year)),
       ylab = "Methane emissions (million tonnes)")
  lines(x = c(max(proj$year),futdec$year), y = futpredmethane, type = "l", lty = 2, col = 2, lwd = 2)

  lmnitrous <- lm(nitrous_oxide ~ gdp + population + decade, proj)
  prednitrous <- predict(lmnitrous)
  futprednitrous <- c(tail(prednitrous, n=1), predict(lmnitrous, newdata = futdec))

  plot(x = proj$year, y = prednitrous,
       ylim =c(min(c(prednitrous,futprednitrous)),
               max(c(prednitrous,futprednitrous))),
       type="l", lwd = 2, xlab = "Year",
       xlim = c(min(proj$year), max(futdec$year)),
       ylab = "Nitrous oxide emissions (million tonnes)")
  lines(x = c(max(proj$year),futdec$year), y = futprednitrous, type = "l", lty = 2, col = 2, lwd = 2)
}
