# ### Functions {.tabset .tabset-pills}
# 
# There are some trivial but crucial functions to set up. The comments before each function (e.g. `@param x a vector`) is the official format for documenting R functions, it provides description of the function and its input parameters, and the return value / object. These lines can be directly used for documenting the functions in an R package, which then provides function documentation such as [this](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sum.html).
# 
# > `@description` describes what the function does  
# > `@param` list the input variables, with the variable name after "param"  
# > `@return` describes the object returned by the function, if any  


#' @description Calculates daily return of a price vector
#' @param x a vector
#' @return a vector of length length(x) - 1
getReturn <- function(x) {
  (x - lag(x)) / lag(x)
}

#' @description Calculates Beta given the price of a stock and the index,
#' @param input_df has an index column idx,
#'   and another column for the ticker, 
#'   the order of columns does not matter
#' @param idx the name of the index
#' @return a single numeric value
calcBeta <- function(input_df, idx = 'Eurostoxx600') {
  ticker <- names(input_df)[which(names(input_df) != idx)]
  
  ols <- data.frame(
    idx_ret = getReturn(input_df[[idx]]),
    x_ret = getReturn(input_df[[ticker]]),
    stringsAsFactors = F) %>%
    lm(x_ret ~ idx_ret, .)
  
  ols[['coefficients']][['idx_ret']]
}




#' @description Generates a calendar based on the investment cycle,
#' @param date_df a data frame with a single column named \code{date}
#' @param start_year,end_year integer for the start and end year of the output calendar
#' @param holding_months the number of months per cycle
#' @return a calendar data frame with \code{date}, \code{year} and \code{cycle}
getCycleCal <- function(date_df, start_year, end_year, holding_months) {
  cycle_df <- expand_grid(
    year = start_year:end_year,
    cycle = 1:(12 / holding_months)) %>%
    mutate(
      start_month = (cycle - 1) * holding_months + 1,
      end_month = start_month + holding_months - 1
    )
  
  date_df %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)) %>%
    left_join(cycle_df, by = 'year') %>%
    filter(
      start_month <= month &
        month <= end_month) %>%
    select(
      date, year, cycle)
}

#' @description Finds the start and end dates of a given cycle,
#' @param cycle_cal a data frame returned by \code{\link{getCycleCal}}
#' @param target_year,target_cycle the target year and cycle
#' @return a numeric vector of two elements, the start and end date of the cycle
getCycleDates <- function(cycle_cal, target_year, target_cycle) {
  tmp <- cycle_cal %>%
    dplyr::filter(year == target_year) %>%
    dplyr::filter(cycle == target_cycle)
  
  range(tmp[['date']])
}


#' @description Get a data frame give a ticker
#' @param input_df a data frame, has a column with name "Eurostoxx600",
#'   and has a column with name ticker
#' @param ticker a string vector for the ticker(s)
#' @return a data frame with the date, index, and the ticker(s) column(s)
getTickerDF <- function(input_df, ticker) {
  input_df %>%
    select_at(vars('date', 'Eurostoxx600', all_of(ticker))) %>%
    na.omit()
}



#' @description Validate a ticker data frame and calculate the Beta of possible
#' @param input_df a data frame with an index column and a stock column
#' @param start_date,end_date date format, the first and last day of the Beta period,
#'   at least one needs to be specified. If both are specified then \code{history} is
#'   ignored, otherwise one is derived by the other + \code{history}
#' @param history an integer, the period for Beta, this is overwritten
#'   if \code{start_date} is specified
#' @param min_history an integer, the minimal period required to calculate Beta,
#'   default is 1 unit less than \code{history}
#' @param skip_na_days logical, if \code{TRUE} then the missing days are not counted
#'   as part of the history, i.e. the history will be extended backward if
#'   there are missing days
#' @return A numerical value if there sufficient input data (given data range etc.),
#'   otherwise \code{NA_real_}
getBeta <- function(input_df, start_date, end_date, history = 6L, min_history = history - 1, history_unit = 'month', skip_na_days = F) {
  if (missing(start_date) & missing(end_date)) stop('Both start_date and end_date are missing.')
  
  if (missing(start_date)) {
    start_date <- (end_date - lubridate::duration(history, history_unit)) %>%
      as.Date() + days(1)
  } else if (missing(end_date)) {
    end_date <- (start_date + lubridate::duration(history, history_unit)) %>%
      as.Date() - days(1)
  }
  
  tmp <- input_df %>%
    filter(between(date, start_date, end_date))
  
  if (skip_na_days) {
    tmp <- tmp %>% na.omit()
  }
  
  if (nrow(tmp) > 0) {
    if (interval(
      min(tmp[['date']]), max(tmp[['date']])) %>%
      as.numeric(unit = 'month') >= min_history) {
      calcBeta(tmp %>% select(-date))
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}

#' @description Select stocks with built-in Beta calculation
#' @param input_df a data frame with date and price columns
#' @param base_idx the name of the index column based on which Beta is calculated
#' @param cycle_cal a data frame returned by \code{getCycleCal}
#' @param target_year,target_cycle the target year and cycle for which the stocks are selected
#' @param holding_months integer, the number of months per cycle (holding period of the stocks),
#'   default 3 months
#' @param portf_size integer, the number of stocks per band (high and low), default 30
#' @param beta_history integer, the number of months on which Beta is based, default 6
#' @return a data frame with stock tickers, the input parameters and the Beta for each stock
selectStocks <- function(input_df, base_idx,
                         cycle_cal, target_year, target_cycle,
                         holding_months = 3, portf_size = 30,
                         beta_history = 6, progress = F) {
  target_dates <- cycle_cal %>%
    getCycleDates(target_year, target_cycle)
  target_end_date <- target_dates[1] - days(1)
  

  tickers <- names(input_df)[
    which(!names(input_df) %in% c('date', base_idx))]
  
  if (progress) {
    pb_ticker <- progress_bar$new(
      format = '(:spin) [:bar] :percent eta: :eta',
      total = length(tickers), clear = T, width= 100)
  }
  
  betas <- tickers %>%
    map_dbl(function(x) {
      if (progress) pb_ticker$tick()
      input_df %>%
        getTickerDF(x) %>%
        getBeta(
          end_date = target_end_date,
          history = beta_history,
          history_unit = 'month')
    })
  beta_tbl <- tibble(
    ticker = tickers,
    year = target_year,
    cycle = target_cycle,
    holding_months = holding_months,
    portf_size = portf_size,
    beta_history = beta_history,
    beta = betas
  )
  
  beta_low <- beta_tbl %>% top_n(portf_size, -beta) %>% mutate(beta_type = 'low')
  beta_high <- beta_tbl %>% top_n(portf_size, beta) %>% mutate(beta_type = 'high')
  
  bind_rows(beta_low, beta_high)
}


#' @description Calculates the daily returns of a portfolio
#' @param price_df a data frame with date and price columns
#' @param tickers a character vector for selected stocks
#' @param start_date,end_date Date format, start and end date of the portfolio holding
#' @param portf_weights a numeric vector of the same length as \code{tickers},
#'   the weights for capital allocation
#' @return a tibble with date and return columns
getPortfRet <- function(price_df, tickers, start_date, end_date, portf_weights) {
  
  portf_df <- price_df %>%
    filter(between(date, start_date, end_date))
  
  portf_idx <- portf_df %>%
    select_at(vars(all_of(tickers))) %>%
    mutate_all(~ ./.[1])
  
  # fill NAs with the last non-NA value
  na_cols <- names(portf_idx)[portf_idx %>% map_lgl(~ any(is.na(.)))]
  if (length(na_cols) > 0) {
    portf_idx <- portf_idx %>%
      fill(sym(na_cols), .direction = 'down')
  }
  
  # remove columns with NA
  full_na_cols <- portf_idx %>% map_lgl(~ any(is.na(.)))
  na_cols1 <- names(portf_idx)[full_na_cols]
  if (length(na_cols) > 0) {
    for (each_col in na_cols1) {
      portf_idx[[each_col]] <- NULL
    }
    tmp_weights <- portf_weights[!full_na_cols]
    portf_weights <- tmp_weights / sum(tmp_weights)
  }
  
  portf_ret <- as.matrix(portf_idx) %*% portf_weights
  
  tibble(
    date = portf_df[['date']],
    return = c(portf_ret) / c(portf_ret)[1]
  )
}

#' @description Calculates the daily and holding cycle returns of a Beta strategy
#' @param price_df a data frame with date and price columns
#' @param beta_df a data frame with Beta and parameter columns,
#'   tickers are derived from this
#' @param start_year,end_year integers, start and end years of the strategy
#' @param h_mths integer, number of months for each holding cycle
#' @param b_hist integer, number of months of history for calculating Beta
#' @param p_size integer, portfolio size for each holding cycle
#' @param p_type character, portfolio type, one of \code{high} and
#'   \code{low} for the Beta type
#' @param p_wts portfolio weights, capital allocation weigths for each stock
#' @return a list that includes the parameters, daily return of the whole strategy,
#'   independent return of each cycle and cumulative return of each cycle
getStratRet <- function(prices_df, beta_df, start_year, end_year, h_mths, b_hist, p_size, p_type, p_wts) {
  if (p_wts == 'equal') {
    weights <- rep(1, p_size) / p_size
  } else {
    weights <- rep(1, p_size) / p_size
  }
  
  cycle_cal <-  prices_df %>%
    select(date) %>%
    getCycleCal(start_year, end_year, h_mths)
  
  cycle_grid <- expand_grid(
    t_year = start_year:end_year,
    t_cycle = 1:(12 / h_mths))
  
  tmp_return <- cycle_grid %>%
    pmap(function(t_year, t_cycle) {
      cycle_dates <- cycle_cal %>%
        getCycleDates(t_year, t_cycle)
      
      tickers <- beta_df %>%
        filter(
          year == t_year &
            cycle == t_cycle &
            holding_months == h_mths &
            portf_size == p_size &
            beta_history == b_hist &
            beta_type == p_type) %>%
        pull(ticker)
      
      prices_df %>%
        getPortfRet(
          tickers,
          cycle_dates[1], cycle_dates[2],
          weights)
      
    })
  
  cycle_returns <- tmp_return %>%
    map_dbl(~ tail(.[['return']], 1))
  cumul_returns <- cumprod(cycle_returns)
  daily_returns <- list(
    df = tmp_return,
    ret = cumul_returns
  ) %>%
    pmap(function(df, ret) {
      df[['return']] <- df[['return']] * ret
      df
    }) %>%
    bind_rows()
  
  list(
    holding_months = h_mths,
    beta_history = b_hist,
    portf_size = p_size,
    portf_type = p_type,
    portf_weights = p_wts,
    portf_daily_return = daily_returns,
    portf_cycle_return = cycle_grid %>%
      bind_cols(
        tibble(
          cycle_return = cycle_returns,
          cumul_return = cumul_returns
        )
      )
  )
}