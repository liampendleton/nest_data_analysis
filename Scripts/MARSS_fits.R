library(MARSS)

## get chla data
source(here::here("Scripts", "chla.R"))

## pivot chla from wide to long, select chla & log-transform
ln_chla_vec <- chla |>
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "chla") |>
  select("chla") |>
  log() |>
  t()


#### seasonal model ####

## using discrete Fourier series in lieu of fixed effects because all Dec are missing
## create sin and cos vectors
sin_t <- sin(2 * pi * seq(ncol(ln_chla_vec)) / 12)
cos_t <- cos(2 * pi * seq(ncol(ln_chla_vec)) / 12)
## combine into rowwise matrix
seas <- cbind(sin_t, cos_t) |>
  t()

## effects of month to be estimated
CC <- matrix(c("sin", "cos"), 1, 2)

## create model list for MARSS
mod_list <- list(Z = matrix(1),
                 A = matrix(0),
                 D = matrix(0),
                 d = matrix(0),
                 R = matrix("R"),
                 B = matrix(1),
                 U = matrix(0),
                 Q = matrix("Q"),
                 C = CC,
                 c = seas)

## fit SSM
mod_fit <- MARSS(y = ln_chla_vec, model = mod_list)

## get model fits
ln_chla_fit <- mod_fit$states |>
  t()

## time series of data and fits
plot.ts(as.vector(ln_chla_fit))
points(as.vector(ln_chla_vec))

## bi-plot
plot(ln_chla_fit, ln_chla_vec, asp = 1)

## these fits DO NOT LOOK GOOD, so let's try a non-seasonal model


#### non-seasonal model ####

mod_list$C <- matrix(0)
mod_list$c <- matrix(0)

## fit SSM
mod_fit_2 <- MARSS(y = ln_chla_vec, model = mod_list)

## get model fits
ln_chla_fit_2 <- mod_fit_2$states |>
  t()

## time series of data and fits
plot.ts(as.vector(ln_chla_fit_2))
points(as.vector(ln_chla_vec))

## bi-plot
plot(ln_chla_fit_2, ln_chla_vec, asp = 1)

## these fits also DO NOT LOOK GOOD, so let's try a model with low obs error

mod_list$R <- matrix(0.01)

## fit SSM
mod_fit_3 <- MARSS(y = ln_chla_vec, model = mod_list)

## get model fits
ln_chla_fit_3 <- mod_fit_3$states |>
  as.vector()

## time series of data and fits
plot.ts(ln_chla_fit_3)
points(as.vector(ln_chla_vec))

## bi-plot
plot(ln_chla_fit_3, ln_chla_vec, asp = 1)

## this looks much better

## convert chla from log to normal space, create tibble & write to file
chla |>
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "chla") |>
  select(c("Year", "month")) |>
  mutate(chla = round(exp(ln_chla_fit_3), 2)) |>
  write_csv(file = here::here("Data", "clean", "chla_imputed.csv"))

