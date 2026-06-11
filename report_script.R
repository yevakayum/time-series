pacman::p_load(
        tsibble,
        dplyr,
        feasts,
        fable,
        ggplot2,
        ggtime,
        tidyr,
        slider,
        forecast,
        tseries
)

data <- read.csv("BirthsAndFertilityRatesAnnual.csv", check.names = FALSE)

tfr <- data |> 
  mutate(across(-DataSeries, as.character)) |> 
  filter(DataSeries == "Total Fertility Rate (TFR)") |> 
  pivot_longer(
    cols = -DataSeries,
    names_to = "Year",
    values_to = "TFR"
  ) |> 
  mutate(
    Year = as.integer(Year),
    TFR = as.numeric(TFR)
  ) |> 
  filter(Year >= 1960, Year <= 2024) |> 
  arrange(Year) |> 
  select(Year, TFR)

tlb <- data |> 
  mutate(across(-DataSeries, as.character)) |> 
  filter(DataSeries == "Total Live-Births") |> 
  pivot_longer(
    cols = -DataSeries,
    names_to = "Year",
    values_to = "TLB"
  ) |> 
  mutate(
    Year = as.integer(Year),
    TLB = as.numeric(TLB)
  ) |> 
  filter(Year >= 1960, Year <= 2024) |> 
  arrange(Year) |> 
  select(Year, TLB)

tfr_tlb <- tfr |> 
  left_join(tlb, by = "Year")

skimr::skim_without_charts(tfr_tlb)

tfr_ts <- ts(tfr_tlb$TFR, start = 1960, frequency = 1)
tlb_ts <- ts(tfr_tlb$TLB, start = 1960, frequency = 1)

tfr_train <- window(tfr_ts, end = 2012)
tlb_train <- window(tlb_ts, end = 2012)

tfr_test <- window(tfr_ts, start = 2013)
tlb_test <- window(tlb_ts, start = 2013)


#tfr plot
p_tfr <- ggplot(tfr_tlb, aes(x = Year, y = TFR)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Singapore Total Fertility Rate, 1960 – 2024",
    caption = "Figure 1: Singapore Annual Total Fertility Rate from 1960 to 2024",
    x = "Year",
    y = "TFR"
  ) +
  theme_minimal()

#tlb plot
p_tlb <- ggplot(tfr_tlb, aes(x = Year, y = TLB)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Singapore Total Live-Births, 1960 – 2024",
    caption = "Figure 2: Singapore Annual Total Live-Births from 1960 to 2024",
    x = "Year",
    y = "TLB"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

p_tfr
p_tlb

#transformations

tfr_tlb_transformed <- tfr_tlb |>
  mutate(
    log_TFR = log(TFR),
    log_TLB = log(TLB),
    
    diff_TFR = c(NA, diff(TFR)),
    diff_TLB = c(NA, diff(TLB)),
    
    diff_log_TFR = c(NA, diff(log(TFR))),
    diff_log_TLB = c(NA, diff(log(TLB)))
  )
tfr_tlb_transformed



tfr_transform <- tfr_tlb_transformed |>
  select(Year, log_TFR, diff_TFR, diff_log_TFR) |>
  pivot_longer(
    cols = -Year,
    names_to = "Transformation",
    values_to = "Value"
  ) |>
  mutate(
    Transformation = recode(
      Transformation,
      "diff_log_TFR" = "(1-B)log(TFR)",
      "diff_TFR" = "(1-B)TFR",
      "log_TFR" = "log(TFR)"
    ),
    Transformation = factor( 
      Transformation,
      levels = c("log(TFR)", "(1-B)TFR", "(1-B)log(TFR)")
    )
  )


ggplot(tfr_transform, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ Transformation, scales = "free_y", ncol = 1) +
  labs(
    title = "Transformations of TFR, 1960 – 2024",
    x = "Year",
    y = NULL
  ) +
  theme_minimal()

tlb_transform <- tfr_tlb_transformed |>
  select(Year, log_TLB, diff_TLB, diff_log_TLB) |>
  pivot_longer(
    cols = -Year,
    names_to = "Transformation",
    values_to = "Value"
  ) |>
  mutate(
    Transformation = recode(
      Transformation,
      "log_TLB" = "log(TLB)",
      "diff_TLB" = "(1-B)TLB",
      "diff_log_TLB" = "(1-B)log(TLB)"
    ),
    Transformation = factor( 
      Transformation,
      levels = c("log(TLB)", "(1-B)TLB", "(1-B)log(TLB)")
    )
  )

ggplot(tlb_transform, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ Transformation, scales = "free_y", ncol = 1) +
  labs(
    title = "Transformations of TLB, 1960 - 2024",
    x = "Year",
    y = NULL
  ) +
  theme_minimal()



#train
train_transformed <- tfr_tlb_transformed |>
  filter(Year >= 1960, Year <= 2012)
train_transformed

#kpss
kpss_data <- train_transformed |>
  pivot_longer(
    cols = -Year,
    names_to = "Variable",
    values_to = "Value"
  ) |>
  filter(!is.na(Value)) |>
  mutate(
    Series = case_when(
      Variable %in% c("TFR", "log_TFR", "diff_TFR", "diff_log_TFR") ~ "TFR",
      Variable %in% c("TLB", "log_TLB", "diff_TLB", "diff_log_TLB") ~ "TLB"
    ),
    Transformation = case_when(
      Variable %in% c("TFR", "TLB") ~ "X_t",
      Variable %in% c("log_TFR", "log_TLB") ~ "log(X_t)",
      Variable %in% c("diff_TFR", "diff_TLB") ~ "(1-B)X_t",
      Variable %in% c("diff_log_TFR", "diff_log_TLB") ~ "(1-B)log(X_t)"
    )
  ) |>
  as_tsibble(
    index = Year,
    key = c(Series, Transformation)
  )
kpss_data

#kpss table
kpss_test <- kpss_data |>
  features(Value, unitroot_kpss)
kpss_test

kpss_table <- kpss_test |> 
  mutate(
    Transformation = factor(
      Transformation, 
      levels = c("X_t", "log(X_t)", "(1-B)X_t", "(1-B)log(X_t)")
    ),
    Series = factor(
      Series, 
      levels = c("TFR", "TLB")
    )
  ) |> 
  arrange(Series, Transformation)
kpss_table

knitr::kable( 
  kpss_table, 
  caption = "Table 1: KPSS test results for TFR and TLB transformations, 1960 - 2012"
  )



# Create transformed training series
tfr_diff_train <- ts(na.omit(train_transformed$diff_TFR), start = 1961, frequency = 1)
tfr_diff_log_train <- ts(na.omit(train_transformed$diff_log_TFR), start = 1961, frequency = 1)

tlb_diff_train <- ts(na.omit(train_transformed$diff_TLB), start = 1961, frequency = 1)
tlb_diff_log_train <- ts(na.omit(train_transformed$diff_log_TLB), start = 1961, frequency = 1)


# ACF and PACF of (1-B)TFR
par(mfrow = c(2, 1))
acf(tfr_diff_train, lag.max = 40, main = "ACF of (1-B)TFR")
pacf(tfr_diff_train,lag.max = 40, main = "PACF of (1-B)TFR")
par(mfrow = c(1, 1))

# ACF and PACF of (1-B)log(TFR)
par(mfrow = c(2, 1))
acf(tfr_diff_log_train, lag.max = 40, main = "ACF of (1-B)log(TFR)")
pacf(tfr_diff_log_train, lag.max = 40, main = "PACF of (1-B)log(TFR)")
par(mfrow = c(1, 1))

# ACF and PACF of (1-B)TLB
par(mfrow = c(2, 1))
acf(tlb_diff_train, lag.max = 40, main = "ACF of (1-B)TLB")
pacf(tlb_diff_train, lag.max = 40, main = "PACF of (1-B)TLB")
par(mfrow = c(1, 1))

# ACF and PACF of (1-B)log(TLB)
par(mfrow = c(2, 1))
acf(tlb_diff_log_train, lag.max = 40, main = "ACF of (1-B)log(TLB)")
pacf(tlb_diff_log_train, lag.max = 40, main = "PACF of (1-B)log(TLB)")
par(mfrow = c(1, 1))




#arima tfr

log_tfr_train <- log(tfr_train)
log_tlb_train <- log(tlb_train)

tfr_011 <- arima(tfr_train, order = c(0, 1, 1))
tfr_110 <- arima(tfr_train, order = c(1, 1, 0))
tfr_111 <- arima(tfr_train, order = c(1, 1, 1))

log_tfr_011 <- arima(log_tfr_train, order = c(0, 1, 1))
log_tfr_110 <- arima(log_tfr_train, order = c(1, 1, 0))
log_tfr_111 <- arima(log_tfr_train, order = c(1, 1, 1))

tfr_011_lag12_ar <- arima(
  tfr_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(1, 0, 0), period = 12)
)

log_tfr_011_lag12_ar <- arima(
  log_tfr_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(1, 0, 0), period = 12)
)

#aic tfr table 

tfr_model_table <- data.frame(
  Model = c(
    "TFR ARIMA(0,1,1)",
    "TFR ARIMA(1,1,0)",
    "TFR ARIMA(1,1,1)",
    "log(TFR) ARIMA(0,1,1)",
    "log(TFR) ARIMA(1,1,0)",
    "log(TFR) ARIMA(1,1,1)",
    "TFR ARIMA(0,1,1)(1,0,0)[12]",
    "log(TFR) ARIMA(0,1,1)(1,0,0)[12]"
  ),
  AIC = c(
    AIC(tfr_011),
    AIC(tfr_110),
    AIC(tfr_111),
    AIC(log_tfr_011),
    AIC(log_tfr_110),
    AIC(log_tfr_111),
    AIC(tfr_011_lag12_ar),
    AIC(log_tfr_011_lag12_ar)
  )
)

tfr_model_table$AIC <- round(tfr_model_table$AIC, 2)

tfr_model_table <- tfr_model_table[order(tfr_model_table$AIC), ]
tfr_model_table

#arima tlb 

tlb_011 <- arima(tlb_train, order = c(0, 1, 1))
tlb_110 <- arima(tlb_train, order = c(1, 1, 0))
tlb_111 <- arima(tlb_train, order = c(1, 1, 1))

log_tlb_011 <- arima(log_tlb_train, order = c(0, 1, 1))
log_tlb_110 <- arima(log_tlb_train, order = c(1, 1, 0))
log_tlb_111 <- arima(log_tlb_train, order = c(1, 1, 1))

tlb_011_lag12_ar <- arima(
  tlb_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(1, 0, 0), period = 12)
)

log_tlb_011_lag12_ar <- arima(
  log_tlb_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(1, 0, 0), period = 12)
)

#aic tlb table 

tlb_model_table <- data.frame(
  Model = c(
    "TLB ARIMA(0,1,1)",
    "TLB ARIMA(1,1,0)",
    "TLB ARIMA(1,1,1)",
    "log(TLB) ARIMA(0,1,1)",
    "log(TLB) ARIMA(1,1,0)",
    "log(TLB) ARIMA(1,1,1)",
    "TLB ARIMA(0,1,1)(1,0,0)[12]",
    "log(TLB) ARIMA(0,1,1)(1,0,0)[12]"
  ),
  AIC = c(
    AIC(tlb_011),
    AIC(tlb_110),
    AIC(tlb_111),
    AIC(log_tlb_011),
    AIC(log_tlb_110),
    AIC(log_tlb_111),
    AIC(tlb_011_lag12_ar),
    AIC(log_tlb_011_lag12_ar)
  )
)

tlb_model_table$AIC <- round(tlb_model_table$AIC, 2)

tlb_model_table <- tlb_model_table[order(tlb_model_table$AIC), ]
tlb_model_table



#TFR best original model
par(mfrow = c(2, 1))

acf(tfr_011_lag12_ar$resid, lag.max = 40,
    main = "Residual ACF: TFR ARIMA(0,1,1)(1,0,0)[12]")

pacf(tfr_011_lag12_ar$resid, lag.max = 40,
     main = "Residual PACF: TFR ARIMA(0,1,1)(1,0,0)[12]")

par(mfrow = c(1, 1))

#TFR best log model
par(mfrow = c(2, 1))

acf(log_tfr_011_lag12_ar$resid, lag.max = 40,
    main = "Residual ACF: log(TFR) ARIMA(0,1,1)(1,0,0)[12]")

pacf(log_tfr_011_lag12_ar$resid, lag.max = 40,
     main = "Residual PACF: log(TFR) ARIMA(0,1,1)(1,0,0)[12]")

par(mfrow = c(1, 1))


#TLB best original model
par(mfrow = c(2, 1))

acf(tlb_011_lag12_ar$resid, lag.max = 40,
    main = "Residual ACF: TLB ARIMA(0,1,1)(1,0,0)[12]")

pacf(tlb_011_lag12_ar$resid, lag.max = 40,
     main = "Residual PACF: TLB ARIMA(0,1,1)(1,0,0)[12]")

par(mfrow = c(1, 1))

#TLB best log model
par(mfrow = c(2, 1))

acf(log_tlb_011_lag12_ar$resid, lag.max = 40,
    main = "Residual ACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")

pacf(log_tlb_011_lag12_ar$resid, lag.max = 40,
     main = "Residual PACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")

par(mfrow = c(1, 1))


# Extra TFR log-scale models with lag-12 structure

log_tfr_111_lag12_ar <- arima(
  log_tfr_train,
  order = c(1, 1, 1),
  seasonal = list(order = c(1, 0, 0), period = 12)
)

log_tfr_011_lag12_ma <- arima(
  log_tfr_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 0, 1), period = 12)
)

log_tfr_111_lag12_ma <- arima(
  log_tfr_train,
  order = c(1, 1, 1),
  seasonal = list(order = c(0, 0, 1), period = 12)
)


#extra for appendix 

log_tfr_011_lag11_ar <- arima(log_tfr_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 11))
log_tfr_111_lag11_ar <- arima(log_tfr_train, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 11))
log_tfr_011_lag11_ma <- arima(log_tfr_train, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 11))
log_tfr_111_lag11_ma <- arima(log_tfr_train, order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 11))

log_tfr_011_lag13_ar <- arima(log_tfr_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 13))
log_tfr_111_lag13_ar <- arima(log_tfr_train, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 13))
log_tfr_011_lag13_ma <- arima(log_tfr_train, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 13))
log_tfr_111_lag13_ma <- arima(log_tfr_train, order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 13))

log_tfr_extra_table <- data.frame(
  Model = c(
    "log(TFR) ARIMA(0,1,1)(1,0,0)[11]",
    "log(TFR) ARIMA(1,1,1)(1,0,0)[11]",
    "log(TFR) ARIMA(0,1,1)(0,0,1)[11]",
    "log(TFR) ARIMA(1,1,1)(0,0,1)[11]",
    "log(TFR) ARIMA(0,1,1)(1,0,0)[12]",
    "log(TFR) ARIMA(1,1,1)(1,0,0)[12]",
    "log(TFR) ARIMA(0,1,1)(0,0,1)[12]",
    "log(TFR) ARIMA(1,1,1)(0,0,1)[12]",
    "log(TFR) ARIMA(0,1,1)(1,0,0)[13]",
    "log(TFR) ARIMA(1,1,1)(1,0,0)[13]",
    "log(TFR) ARIMA(0,1,1)(0,0,1)[13]",
    "log(TFR) ARIMA(1,1,1)(0,0,1)[13]"
  ),
  AIC = c(
    AIC(log_tfr_011_lag11_ar),
    AIC(log_tfr_111_lag11_ar),
    AIC(log_tfr_011_lag11_ma),
    AIC(log_tfr_111_lag11_ma),
    AIC(log_tfr_011_lag12_ar),
    AIC(log_tfr_111_lag12_ar),
    AIC(log_tfr_011_lag12_ma),
    AIC(log_tfr_111_lag12_ma),
    AIC(log_tfr_011_lag13_ar),
    AIC(log_tfr_111_lag13_ar),
    AIC(log_tfr_011_lag13_ma),
    AIC(log_tfr_111_lag13_ma)
  )
)

log_tfr_extra_table$AIC <- round(log_tfr_extra_table$AIC, 2)
log_tfr_extra_table <- log_tfr_extra_table[order(log_tfr_extra_table$AIC), ]
log_tfr_extra_table


#tlb arima appendix

log_tlb_011_lag11_ar <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 11))
log_tlb_111_lag11_ar <- arima(log_tlb_train, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 11))
log_tlb_011_lag11_ma <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 11))
log_tlb_111_lag11_ma <- arima(log_tlb_train, order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 11))

log_tlb_011_lag12_ar <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))
log_tlb_111_lag12_ar <- arima(log_tlb_train, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))
log_tlb_011_lag12_ma <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12))
log_tlb_111_lag12_ma <- arima(log_tlb_train, order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12))

log_tlb_011_lag13_ar <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 13))
log_tlb_011_lag13_ma <- arima(log_tlb_train, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 13))
log_tlb_111_lag13_ma <- arima(log_tlb_train, order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 13))


log_tlb_extra_table <- data.frame(
  Model = c(
    "log(TLB) ARIMA(0,1,1)(1,0,0)[11]",
    "log(TLB) ARIMA(1,1,1)(1,0,0)[11]",
    "log(TLB) ARIMA(0,1,1)(0,0,1)[11]",
    "log(TLB) ARIMA(1,1,1)(0,0,1)[11]",
    "log(TLB) ARIMA(0,1,1)(1,0,0)[12]",
    "log(TLB) ARIMA(1,1,1)(1,0,0)[12]",
    "log(TLB) ARIMA(0,1,1)(0,0,1)[12]",
    "log(TLB) ARIMA(1,1,1)(0,0,1)[12]",
    "log(TLB) ARIMA(0,1,1)(0,0,1)[13]",
    "log(TLB) ARIMA(1,1,1)(0,0,1)[13]",
    "log(TLB) ARIMA(0,1,1)(1,0,0)[13]"
  ),
  AIC = c(
    AIC(log_tlb_011_lag11_ar),
    AIC(log_tlb_111_lag11_ar),
    AIC(log_tlb_011_lag11_ma),
    AIC(log_tlb_111_lag11_ma),
    AIC(log_tlb_011_lag12_ar),
    AIC(log_tlb_111_lag12_ar),
    AIC(log_tlb_011_lag12_ma),
    AIC(log_tlb_111_lag12_ma),
    AIC(log_tlb_011_lag13_ma),
    AIC(log_tlb_111_lag13_ma),
    AIC(log_tlb_011_lag13_ar)
  )
)

log_tlb_extra_table$AIC <- round(log_tlb_extra_table$AIC, 2)
log_tlb_extra_table <- log_tlb_extra_table[order(log_tlb_extra_table$AIC), ]
log_tlb_extra_table


#в репорт 
par(mfrow = c(2, 1))
acf(log_tfr_111_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TFR) ARIMA(1,1,1)(1,0,0)[12]")
pacf(log_tfr_111_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TFR) ARIMA(1,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
acf(log_tfr_011_lag12_ma$resid, lag.max = 40, main = "Residual ACF: log(TFR) ARIMA(0,1,1)(0,0,1)[12]")
pacf(log_tfr_011_lag12_ma$resid, lag.max = 40, main = "Residual PACF: log(TFR) ARIMA(0,1,1)(0,0,1)[12]")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
acf(log_tfr_111_lag12_ma$resid, lag.max = 40, main = "Residual ACF: log(TFR) ARIMA(1,1,1)(0,0,1)[12]")
pacf(log_tfr_111_lag12_ma$resid, lag.max = 40, main = "Residual PACF: log(TFR) ARIMA(1,1,1)(0,0,1)[12]")
par(mfrow = c(1, 1))

#в репорт 
par(mfrow = c(2, 1))
acf(tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual ACF: TLB ARIMA(0,1,1)(1,0,0)[12]")
pacf(tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual PACF: TLB ARIMA(0,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))


#full tfr
par(mfrow = c(2,2))
acf(log_tfr_011_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TFR) ARIMA(0,1,1)(1,0,0)[12]")
pacf(log_tfr_011_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TFR) ARIMA(0,1,1)(1,0,0)[12]")
acf(log_tfr_111_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TFR) ARIMA(1,1,1)(1,0,0)[12]")
pacf(log_tfr_111_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TFR) ARIMA(1,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))




#в репорт
par(mfrow = c(2, 1))
acf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
pacf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))


#оба в репорт
par(mfrow = c(2, 1))
acf(log_tlb_011_lag13_ma$resid, lag.max = 40, main = "Residual ACF: log(TLB) ARIMA(0,1,1)(0,0,1)[13]")
pacf(log_tlb_011_lag13_ma$resid, lag.max = 40, main = "Residual PACF: log(TLB) ARIMA(0,1,1)(0,0,1)[13]")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
acf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
pacf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))

#full tlb 
par(mfrow = c(2,2))
acf(log_tlb_011_lag13_ma$resid, lag.max = 40, main = "Residual ACF: log(TLB) ARIMA(0,1,1)(0,0,1)[13]")
pacf(log_tlb_011_lag13_ma$resid, lag.max = 40, main = "Residual PACF: log(TLB) ARIMA(0,1,1)(0,0,1)[13]")
acf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual ACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
pacf(log_tlb_011_lag12_ar$resid, lag.max = 40, main = "Residual PACF: log(TLB) ARIMA(0,1,1)(1,0,0)[12]")
par(mfrow = c(1, 1))

#prediction 

tfr_actual <- as.numeric(tfr_test)
tlb_actual <- as.numeric(tlb_test)

tfr_pred_011 <- predict(log_tfr_011_lag12_ar, n.ahead = 12)
tfr_pred_111 <- predict(log_tfr_111_lag12_ar, n.ahead = 12)

tfr_forecast_011 <- exp(tfr_pred_011$pred)
tfr_forecast_111 <- exp(tfr_pred_111$pred)

tfr_forecast_table <- data.frame(
  Model = c(
    "log(TFR) ARIMA(0,1,1)(1,0,0)[12]",
    "log(TFR) ARIMA(1,1,1)(1,0,0)[12]"
  ),
  AIC = c(
    AIC(log_tfr_011_lag12_ar),
    AIC(log_tfr_111_lag12_ar)
  ),
  MSE = c(
    mean((tfr_actual - tfr_forecast_011)^2),
    mean((tfr_actual - tfr_forecast_111)^2)
  ),
  MAE = c(
    mean(abs(tfr_actual - tfr_forecast_011)),
    mean(abs(tfr_actual - tfr_forecast_111))
  )
)

tfr_forecast_table$AIC <- round(tfr_forecast_table$AIC, 2)
tfr_forecast_table$MSE <- round(tfr_forecast_table$MSE, 4)
tfr_forecast_table$MAE <- round(tfr_forecast_table$MAE, 4)

tfr_forecast_table


tlb_pred_013 <- predict(log_tlb_011_lag13_ma, n.ahead = 12)
tlb_pred_012 <- predict(log_tlb_011_lag12_ar, n.ahead = 12)

tlb_forecast_013 <- exp(tlb_pred_013$pred)
tlb_forecast_012 <- exp(tlb_pred_012$pred)

tlb_forecast_table <- data.frame(
  Model = c(
    "log(TLB) ARIMA(0,1,1)(0,0,1)[13]",
    "log(TLB) ARIMA(0,1,1)(1,0,0)[12]"
  ),
  AIC = c(
    AIC(log_tlb_011_lag13_ma),
    AIC(log_tlb_011_lag12_ar)
  ),
  MSE = c(
    mean((tlb_actual - tlb_forecast_013)^2),
    mean((tlb_actual - tlb_forecast_012)^2)
  ),
  MAE = c(
    mean(abs(tlb_actual - tlb_forecast_013)),
    mean(abs(tlb_actual - tlb_forecast_012))
  )
)

tlb_forecast_table$AIC <- round(tlb_forecast_table$AIC, 2)
tlb_forecast_table$MSE <- round(tlb_forecast_table$MSE, 2)
tlb_forecast_table$MAE <- round(tlb_forecast_table$MAE, 2)

tlb_forecast_table



#forecast 
forecast_years <- 2013:2024

tfr_final_pred <- predict(log_tfr_011_lag12_ar, n.ahead = 12)
tfr_final_forecast <- exp(tfr_final_pred$pred)

tfr_final_plot_data <- data.frame(
  Year = forecast_years,
  Actual = tfr_actual,
  Forecast = as.numeric(tfr_final_forecast)
) |>
  pivot_longer(
    cols = c(Actual, Forecast),
    names_to = "Series",
    values_to = "TFR"
  )

ggplot(tfr_final_plot_data, aes(x = Year, y = TFR, linetype = Series)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_x_continuous(
    breaks = 2013:2024,
    limits = c(2013, 2024)
  ) + 
  labs(
    title = "TFR Forecast Compared with Actual Values, 2013–2024",
    x = "Year",
    y = "TFR",
    linetype = NULL
  ) +
  theme_minimal()


tlb_final_pred <- predict(log_tlb_011_lag12_ar, n.ahead = 12)
tlb_final_forecast <- exp(tlb_final_pred$pred)

tlb_final_plot_data <- data.frame(
  Year = forecast_years,
  Actual = tlb_actual,
  Forecast = as.numeric(tlb_final_forecast)
) |>
  pivot_longer(
    cols = c(Actual, Forecast),
    names_to = "Series",
    values_to = "TLB"
  )

ggplot(tlb_final_plot_data, aes(x = Year, y = TLB, linetype = Series)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  scale_x_continuous(
    breaks = 2013:2024,
    limits = c(2013, 2024)
  ) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TLB Forecast Compared with Actual Values, 2013–2024",
    x = "Year",
    y = "TLB",
    linetype = NULL
  ) +
  theme_minimal()













