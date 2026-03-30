pacman::p_load(tsibble,
        dplyr,
        feasts,
        fable,
        ggplot2,
        ggtime,
        tidyr,
        slider) 

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
  filter(Year <= 2024) |> 
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
  filter(Year <= 2024) |> 
  arrange(Year) |> 
  select(Year, TLB)

tfr_tlb <- tfr |> 
  left_join(tlb, by = "Year")

skimr::skim_without_charts(tfr_tlb)

tfr_ts <- ts(trf_tlb$TFR, start = 1960, frequency = 1)
tlb_ts <- ts(trf_tlb$TLB, start = 1960, frequency = 1)

plot(tfr_ts,
     main = "Singapore Total Fertility Rate, 1960 - 2024", 
     sub = "Figure 1: Singapore Annual Total Fertility Rate from 1960 to 2024",
     xlab = "Year",
     ylab = "TFR")

plot(tlb_ts,
     main = "Singapore Total Live-Births, 1960 - 2024", 
     sub = "Figure 2: Singapore Annual Total Live-Births from 1960 to 2024",
     xlab = "Year",
     ylab = "TLB")

plot(diff(tfr_ts),
     main = "Diffenrence of TFR", 
     sub = "Figure 3: Difference of Singapore Annual Total Fertility Rate from 1960 to 2024",
     xlab = "Year",
     ylab = "TRF")

plot(diff(tlb_ts),
     main = "Difference of TLB", 
     sub = "Figure 4: Difference of Singapore Annual Total Live-Births from 1960 to 2024",
     xlab = "Year",
     ylab = "TLB")

par(mfrow = c(2,2))
acf(tfr_ts, lag.max = 20, main = "ACF of TFR")
pacf(tfr_ts, lag.max = 20, main = "PACF of TFR")
acf(diff(tfr_ts), lag.max = 20, main = "ACF of diff(TFR)")
pacf(diff(tfr_ts), lag.max = 20, main = "PACF of diff(TFR)")
par(mfrow = c(1,1))

par(mfrow = c(2,2))
acf(tlb_ts, lag.max = 20, main = "ACF of TLB")
pacf(tlb_ts, lag.max = 20, main = "PACF of TLB")
acf(diff(tlb_ts), lag.max = 20, main = "ACF of diff(TLB)")
pacf(diff(tlb_ts), lag.max = 20, main = "PACF of diff(TLB)")
par(mfrow = c(1,1))

tfr_train <- window(tfr_ts, end = 2012)
tlb_train <- window(tlb_ts, end = 2012)

tfr_111 <-  arima(tfr_train, order = c(1,1,1))
tfr_210 <- arima(tfr_train, order = c(2,1,0))
tfr_012 <- arima(tfr_train, order = c(0,1,2))

tlb_110 <- arima(tlb_train, order = c(1,1,0))
tlb_011 <-  arima(tlb_train, order = c(0,1,1))
tlb_111 <- arima(tlb_train, order = c(1,1,1))

aic_table <- data.frame(
  Model = c("TFR ARIMA(1,1,1)", "TFR ARIMA(2,1,0)", "TFR ARIMA(0,1,2)",
            "TLB ARIMA(1,1,0)", "TLB ARIMA(0,1,1)", "TLB ARIMA(1,1,1)"),
  AIC = c(tfr_111$aic, tfr_210$aic, tfr_012$aic,
          tlb_110$aic, tlb_011$aic, tlb_111$aic)
)
knitr::kable(aic_table, 
             caption = "AIC values for ARIMA models for TFR and TLB")


par(mfrow = c(2,2))

acf(tfr_111$resid, lag.max = 20, main = "TFR ARIMA(1,1,1) ACF")
pacf(tfr_111$resid, lag.max = 20, main = "TFR ARIMA(1,1,1) PACF")

acf(tfr_210$resid, lag.max = 20, main = "TFR ARIMA(2,1,0) ACF")
pacf(tfr_210$resid, lag.max = 20, main = "TFR ARIMA(2,1,0) PACF")

par(mfrow = c(1,1))


par(mfrow = c(2,2))

acf(tlb_011$resid, lag.max = 20, main = "TLB ARIMA(0,1,1) ACF")
pacf(tlb_011$resid, lag.max = 20, main = "TLB ARIMA(0,1,1) PACF")

acf(tlb_110$resid, lag.max = 20, main = "TLB ARIMA(1,1,0) ACF")
pacf(tlb_110$resid, lag.max = 20, main = "TLB ARIMA(1,1,0) PACF")

par(mfrow = c(1,1))

