#' Aramdillo forecaster
#'
#' Armadillo forecaster only gives point forecaster, not quantile forecaster.
#'
#' @param before_pan if TRUE, the parameter out in Mean.fun is 0, and the first estimated response must be 0.
#' @param mob_shift the number of epiweek that mobility variable is shifted backwards.
#' @param mob_fun the function applied to mobility variables, min, mean, or max.
#' @param DC death delay curve, numerical vector. If NULL, a gamma distribution with scale 3.64, shape 6.28 is used.
#' @param initial_fun function to initialize the normalized parameter At. It is a function of the sum of death incidence number over week.
#' @param initial_val initialized values for other model parameters, beta, alpha, mu, sigma.
#' @param lower lower bound for parameters (At, beta, alpha, mu, sigma).
#' @param upper upper bound for parameters (At, beta, alpha, mu, sigma).
#' @param ... control arguments in optim_sa, such as initial temperature, temperature reduction in outer loop, ...
#'
#' @return a forecaster that works with evalcast.
#'
#' @examples
#' signals <- tibble::tibble(data_source = c("jhu-csse","safegraph"),
#' signal = c("deaths_incidence_num","completely_home_prop"),
#' start_day = c("2020-03-08", "2020-03-01"))
#'
#' arma_forecaster <- Armadillo_forecaster(
#'   before_pan = T,
#'   mob_shift = 1,
#'   mob_fun = "min",
#'   DC = NULL,
#'   initial_fun = function(x) {
#'     max(x) * 0.6
#'   },
#'   initial_val = c(0, 0, 0, 1),
#'   lower = c(0, -40, -4, -2, 0.00001),
#'   upper = c(Inf, 40, 4, 2, 2),
#'   t0 = 30000, r = 0.95, nlimit = 2000
#' )
#'
#' res_armadillo <- get_predictions(arma_forecaster,
#'   name_of_forecaster = "armadillo",
#'   signals,
#'   forecast_dates = "2020-07-20",
#'   incidence_period = "epiweek",
#'   ahead = 1:3,
#'   geo_type = "state",
#'   geo_values = "*"
#' )
#'
#' eva <- evaluate_predictions(res_armadillo)
#'
#'
#'
#' @export
Armadillo_forecaster <- function(before_pan = TRUE,
                                 mob_shift = 0,
                                 mob_fun = "min",
                                 DC = NULL,
                                 initial_fun = function(x) {
                                   max(x) * 0.75
                                 },
                                 initial_val = c(0, 0, 0, 1),
                                 lower,
                                 upper,
                                 ...) {
  function(df,
           forecast_date,
           signals,
           incidence_period,
           ahead,
           geo_type,
           ...) {
    Armadillo_forecaster_raw(df,
      forecast_date,
      signals,
      incidence_period,
      ahead,
      geo_type,
      before_pan = before_pan,
      mob_shift = mob_shift,
      DC = DC,
      initial_fun = initial_fun,
      initial_val = initial_val,
      lower = lower,
      upper = upper,
      ...
    )
  }
}

#' Raw forecaster
#'
#' @param df dataframe returned by covidcast.
#' @param forecast_date
#' @param signals signal specifying response variable and safegraph mobility variables.
#' @param incidence_period this forecaster only works with epiweek now.
#' @param ahead
#' @param geo_type this forecaster only works with state geo level.
#' @param before_pan
#' @param mob_shift
#' @param mob_fun
#' @param DC
#' @param initial_fun
#' @param initial_val
#' @param lower
#' @param upper
#' @param ...
#'
#' @return
#'
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom stats dgamma
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
#'
#' @examples
Armadillo_forecaster_raw <- function(df,
                                     forecast_date,
                                     signals,
                                     incidence_period = c("epiweek"),
                                     ahead,
                                     geo_type = c("state"),
                                     before_pan,
                                     mob_shift,
                                     mob_fun = c("mean", "min", "max"),
                                     DC,
                                     initial_fun,
                                     initial_val,
                                     lower,
                                     upper,
                                     ...) {
  forecast_date <- lubridate::ymd(forecast_date)
  incidence_period <- match.arg(incidence_period)
  geo_type <- match.arg(geo_type)
  mob_fun <- match.arg(mob_fun)

  if (geo_type != "state") {
    stop("Armadillo forecaster now only supports state level.")
  } else if (incidence_period != "epiweek") {
    stop("Armadillo forecaster now only supports epiweek.")
  } else if (is.null(forecast_date)) {
    stop("Forecast date must be specified.")
  } else if (!(mob_fun %in% c("mean", "min", "max"))) {
    stop("mob_fun should be one of these functions: mean, min, max.")
  } else if (class(initial_fun) != "function") {
    stop("initial_fun should be a function.")
  } else if (mob_shift < 0) {
    stop("mob_shift is not a positive number.")
  }

  mob_shift <- as.integer(mob_shift)
  # target_period <- get_target_period(forecast_date, incidence_period, ahead)
  incidence_length <- 7
  if ((lubridate::ymd(signals$start_day[1]) - lubridate::ymd(signals$start_day[2])) != mob_shift * incidence_length) {
    stop("If shift of the mobility variables is considered, the start date for safegraph signal should be adjusted manually.")
  }

  covidhub_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  # response
  dat_resp <- df %>%
    dplyr::filter(
      .data$data_source == signals$data_source[1],
      .data$signal == signals$signal[1]
    ) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::arrange(.data$time_value)

  # mobility
  dat_mob <- df %>%
    dplyr::filter(
      .data$data_source == signals$data_source[2],
      .data$signal == signals$signal[2]
    ) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::arrange(.data$time_value)

  dat_resp$time_value <- dat_resp$time_value - incidence_length * mob_shift

  # exclude Puerto Rico
  dat <- merge(dat_mob, dat_resp, by = c("location", "time_value"), all = FALSE)
  dat <- dat %>%
    dplyr::filter(.data$location != "72")

  deaths <- dat$value.y
  deaths[deaths < 0] <- 0
  mob <- dat$value.x
  geo <- dat$location
  Geo <- unique(geo)
  ngeo <- length(Geo)

  Y <- A <- NULL
  for (i in 1:ngeo) {
    Id <- (1:length(deaths))[geo == Geo[i]]
    Y <- rbind(Y, resp_trans(deaths[Id]))
    A <- rbind(A, mob_trans(mob[Id], mob_fun = mob_fun))
  }

  if (any(is.na(A)) || any(is.na(Y))) {
    stop("NA in either response data or mobility data.")
  }
  L <- dim(Y)[2]

  # deal with the case where shift (back) of mobility variables is required
  if (mob_shift != 0) {
    dat_mob <- dat_mob %>%
      dplyr::filter(.data$time_value > max(dat$time_value)) %>%
      dplyr::filter(.data$location != "72")

    mob_post <- dat_mob$value
    geo_post <- dat_mob$location
    if (length(unique(dat_mob$time_value)) >= incidence_length) {
      AA <- NULL
      for (i in 1:ngeo) {
        Id <- which(geo_post == Geo[i])
        AA <- rbind(AA, mob_trans_shift(mob_post[Id], mob_fun = mob_fun))
      }
      rem <- length(unique(dat_mob$time_value)) %% incidence_length
      if (rem != 0) {
        date_rem <- lubridate::ymd(min(dat_mob$time_value) + incidence_length * (dim(AA)[2]))
        dat_rem <- dat_mob %>%
          filter(.data$time_value >= date_rem)
        mob_rem <- dat_rem$value
        geo_rem <- dat_rem$location
        for (i in 1:ngeo) {
          Id <- which(geo_rem == Geo[i])
          AA <- rbind(AA, mob_trans_shift(mob_rem[Id], incidence_length = rem, mob_fun = mob_fun))
        }
      }
    } else {
      rem <- length(unique(dat_mob$time_value)) %% incidence_length
      AA <- NULL
      for (i in 1:ngeo) {
        Id <- which(geo_post == Geo[i])
        AA <- rbind(AA, mob_trans_shift(mob_post[Id], incidence_length = rem, mob_fun = mob_fun))
      }
    }
  }

  # by default, the death curve is gamma distribution with scale 3.64, shape 6.28
  if (is.null(DC)) {
    tt <- seq(0, L + max(ahead), by = 1)
    ft <- stats::dgamma(tt * incidence_length, scale = 3.64, shape = 6.28)
    ft <- (ft / sum(ft)) * .03
    DC <- ft[1:L]
    DC_ahead <- ft[1:(L + max(ahead))]
  }

  if (is.null(lower)) {
    lower <- c(0, -40, -4, -2, 0.00001)
  }
  if (is.null(upper)) {
    upper <- c(Inf, 40, 4, 2, 2)
  }

  PAR <- matrix(0, ngeo, 5)
  PRED <- matrix(0, ngeo, L + max(ahead))
  output_df <- list()

  for (a in ahead) {
    output_df[[a]] <- tibble::tibble()
  }

  for (i in 1:ngeo) {
    y <- Y[i, 1:L]
    M <- A[i, 1:L]

    if (mob_shift == 0) {
      M_ahead <- c(M, rep(A[i, L], max(ahead)))
    } else {
      M_ahead <- c(M, AA[i, ])
      if (length(M_ahead) > L + max(ahead)) {
        M_ahead <- M_ahead[1:(L + max(ahead))]
      } else if (length(M_ahead) < L + max(ahead)) {
        M_ahead <- c(M_ahead, rep(tail(AA[i, ], 1), L + max(ahead) - length(M_ahead)))
      }
    }

    PAR[i, ] <- fit_optim(c(initial_fun(y), initial_val), DC = DC, y = y, M = M, L = L, before_pan = before_pan, lower = lower, upper = upper, ...)
    PRED[i, ] <- Mean.fun(L + max(ahead), PAR[i, 1], PAR[i, 2], PAR[i, 3], PAR[i, 4], PAR[i, 5], M_ahead, DC_ahead, before_pan)

    for (a in ahead) {
      output_df[[a]] <- dplyr::bind_rows(
        output_df[[a]],
        tibble::tibble(
          location = Geo[i],
          tibble(
            probs = covidhub_probs,
            quantiles = stats::setNames(rep(PRED[i, L + a], length(covidhub_probs)), paste(covidhub_probs * 100, sep = "", "%"))
          )
        )
      )
    }
  }
  output_df <- output_df[ahead]
  names(output_df) <- as.character(ahead)
  dplyr::bind_rows(output_df, .id = "ahead") %>%
    dplyr::mutate(ahead = as.integer(ahead))
}
