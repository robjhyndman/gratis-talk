library(forecast)
library(tsfeatures)
library(gratis)

# Time series features
sunspot.year %>% ggtsdisplay(plot.type = "spectrum")
fpp2::goog %>% ggtsdisplay(plot.type = "spectrum")
AirPassengers %>% ggtsdisplay(plot.type = "spectrum")
taylor %>% ggtsdisplay(plot.type = "spectrum")

mylist <- list(sunspot.year, fpp2::goog, AirPassengers, taylor)
tsfeatures(mylist)

# MAR models with random parameters
generate_ts(n.ts = 1, freq = 12, nComp = 2, n = 120)$N1$x %>%
  ggtsdisplay(plot.type = "spectrum")

generate_msts(seasonal.periods = c(7, 365), n = 800, nComp = 2) %>%
  ggtsdisplay(plot.type = "spectrum")

# Generate from 3 component MAR model with target features
x <- generate_ts_with_target(
  n = 1,
  ts.length = 120,
  freq = 12,
  seasonal = 1,
  features = "tsfeatures",
  selected.features = c("entropy", "x_acf1", "seas_acf1", "seasonal_strength", "peak"),
  target = c(0.2, 0.6, 0.6, 0.7, 5),
  parallel = FALSE
)
tsfeatures(x)[, c("entropy", "x_acf1", "seas_acf1", "seasonal_strength", "peak")]
x %>% ggtsdisplay(plot.type = "spectrum")
x %>% ggseasonplot()

# Generate from 3 component MAR model to mimic the AirPassengers series
my_features <- function(x) {
  output <- c(tsfeatures(x))
  output["lambda"] <- BoxCox.lambda(x)
  output["entropy"] <- entropy(x)
  unlist(output)
}
air_features <- my_features(AirPassengers)
set.seed(1)
x <- generate_ts_with_target(
  n = 1,
  ts.length = length(AirPassengers),
  freq = 12,
  seasonal = 1,
  features = "my_features",
  selected.features = names(air_features),
  target = air_features
)
my_features(AirPassengers)
my_features(x)
AirPassengers %>% ggtsdisplay(plot.type = "spectrum")
x %>% ggtsdisplay(plot.type = "spectrum")

# Shiny app
system("xdg-open https://ebsmonash.shinyapps.io/tsgeneration/")
