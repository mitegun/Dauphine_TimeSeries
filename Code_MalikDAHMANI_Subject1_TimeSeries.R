# 0. PACKAGE AND DATA LOADING
required_packages <- c(
  "quantmod", "xts", "zoo",
  "tseries", "forecast",
  "rugarch", "rmgarch",
  "FinTS", "moments"
)

new_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cran.r-project.org")

invisible(lapply(required_packages, library, character.only = TRUE))

returns <- read.csv("returns_data.csv", stringsAsFactors = FALSE)
returns$Date <- as.Date(returns$Date)
dates <- returns$Date
returns$Date <- NULL  



# 1. DESCRIPTIVE STATISTICS


cat("\n Descriptive Statistics \n")
cat("Mean    AAPL:", round(mean(returns$AAPL), 6),
    " | SP500:", round(mean(returns$SP500), 6), "\n")
cat("Std Dev AAPL:", round(sd(returns$AAPL), 6),
    " | SP500:", round(sd(returns$SP500), 6), "\n")
cat("Skewness AAPL:", round(moments::skewness(as.numeric(returns$AAPL)), 4),
    " | SP500:", round(moments::skewness(as.numeric(returns$SP500)), 4), "\n")
cat("Kurtosis AAPL:", round(moments::kurtosis(as.numeric(returns$AAPL)), 4),
    " | SP500:", round(moments::kurtosis(as.numeric(returns$SP500)), 4), "\n")


# 2. PRELIMINARY ANALYSIS

#  2.1 Time Series Plots 
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(dates, as.numeric(returns$AAPL), type = "l", col = "steelblue",
     main = "AAPL Daily Log-Returns", xlab = "Date", ylab = "Log-return", lwd = 0.6)
abline(h = 0, col = "grey50", lty = 2)
plot(dates, as.numeric(returns$SP500), type = "l", col = "darkred",
     main = "S&P 500 Daily Log-Returns", xlab = "Date", ylab = "Log-return", lwd = 0.6)
abline(h = 0, col = "grey50", lty = 2)
par(mfrow = c(1, 1))

#  2.2 Stationarity: Augmented Dickey-Fuller Test 
cat("\n ADF Tests \n")
adf_aapl  <- adf.test(as.numeric(returns$AAPL),  alternative = "stationary")
adf_sp500 <- adf.test(as.numeric(returns$SP500), alternative = "stationary")

cat("AAPL  – ADF stat:", round(adf_aapl$statistic, 4),
    " | p-value:", round(adf_aapl$p.value, 4), "\n")
cat("SP500 – ADF stat:", round(adf_sp500$statistic, 4),
    " | p-value:", round(adf_sp500$p.value, 4), "\n")

#  2.3 Autocorrelation: ACF Plots 
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
acf(as.numeric(returns$AAPL),    main = "ACF – AAPL Returns",           lag.max = 30)
acf(as.numeric(returns$SP500),   main = "ACF – S&P 500 Returns",        lag.max = 30)
acf(as.numeric(returns$AAPL)^2,  main = "ACF – AAPL Squared Returns",   lag.max = 30)
acf(as.numeric(returns$SP500)^2, main = "ACF – S&P 500 Squared Returns", lag.max = 30)
par(mfrow = c(1, 1))

#  2.4 Ljung-Box Test for Serial Correlation 
cat("\n Ljung-Box Tests (10 lags) \n")
lb_aapl     <- Box.test(as.numeric(returns$AAPL),    lag = 10, type = "Ljung-Box")
lb_sp500    <- Box.test(as.numeric(returns$SP500),    lag = 10, type = "Ljung-Box")
lb_aapl_sq  <- Box.test(as.numeric(returns$AAPL)^2,  lag = 10, type = "Ljung-Box")
lb_sp500_sq <- Box.test(as.numeric(returns$SP500)^2, lag = 10, type = "Ljung-Box")

cat("LB on returns   – AAPL: Q =", round(lb_aapl$statistic, 2),
    "p =", round(lb_aapl$p.value, 4),
    "| SP500: Q =", round(lb_sp500$statistic, 2),
    "p =", round(lb_sp500$p.value, 4), "\n")
cat("LB on squared   – AAPL: Q =", round(lb_aapl_sq$statistic, 2),
    "p =", round(lb_aapl_sq$p.value, 4),
    "| SP500: Q =", round(lb_sp500_sq$statistic, 2),
    "p =", round(lb_sp500_sq$p.value, 4), "\n")

#  2.5 ARCH-LM Test 
cat("\n ARCH-LM Tests (10 lags) \n")
arch_aapl  <- ArchTest(as.numeric(returns$AAPL),  lags = 10)
arch_sp500 <- ArchTest(as.numeric(returns$SP500), lags = 10)

cat("AAPL  – LM stat:", round(arch_aapl$statistic, 2),
    "p =", round(arch_aapl$p.value, 4), "\n")
cat("SP500 – LM stat:", round(arch_sp500$statistic, 2),
    "p =", round(arch_sp500$p.value, 4), "\n")

#  2.6 Jarque-Bera Normality Test 
cat("\n Jarque-Bera Tests \n")
jb_aapl  <- jarque.test(as.numeric(returns$AAPL))
jb_sp500 <- jarque.test(as.numeric(returns$SP500))

cat("AAPL  – JB stat:", round(jb_aapl$statistic, 2),
    "p =", round(jb_aapl$p.value, 4), "\n")
cat("SP500 – JB stat:", round(jb_sp500$statistic, 2),
    "p =", round(jb_sp500$p.value, 4), "\n")

#  2.7 ARMA Model Selection (BIC) 
cat("\n ARMA Selection (BIC) \n")
arma_aapl  <- auto.arima(as.numeric(returns$AAPL),  max.p = 5, max.q = 5,
                         seasonal = FALSE, ic = "bic", stepwise = TRUE)
arma_sp500 <- auto.arima(as.numeric(returns$SP500), max.p = 5, max.q = 5,
                         seasonal = FALSE, ic = "bic", stepwise = TRUE)

cat("AAPL  ARMA order:", arimaorder(arma_aapl),  "\n")
cat("SP500 ARMA order:", arimaorder(arma_sp500), "\n")

# Store ARMA orders for GARCH mean equation
p_aapl  <- arimaorder(arma_aapl)[1]
q_aapl  <- arimaorder(arma_aapl)[3]
p_sp500 <- arimaorder(arma_sp500)[1]
q_sp500 <- arimaorder(arma_sp500)[3]


# 3. UNCONDITIONAL BETA AND SYSTEMATIC RISK

cat("\n Unconditional Estimation \n")

cov_im   <- cov(as.numeric(returns$AAPL), as.numeric(returns$SP500))
var_m    <- var(as.numeric(returns$SP500))
beta_unc <- cov_im / var_m

cat("Covariance (AAPL, SP500):", round(cov_im, 8), "\n")
cat("Variance (SP500):",         round(var_m, 8),  "\n")
cat("Unconditional Beta:",       round(beta_unc, 4), "\n")

# Confirm via OLS
ols_model <- lm(AAPL ~ SP500, data = as.data.frame(returns))
cat("\nOLS Summary:\n")
print(summary(ols_model))

# OLS scatter plot
plot(as.numeric(returns$SP500), as.numeric(returns$AAPL),
     pch = 16, cex = 0.4, col = rgb(0.2, 0.4, 0.8, 0.4),
     xlab = "S&P 500 Log-Return", ylab = "AAPL Log-Return",
     main = "Scatter Plot & OLS Regression Line")
abline(ols_model, col = "red", lwd = 2)
abline(h = 0, v = 0, col = "grey70", lty = 2)
legend("topleft", legend = paste0("Beta = ", round(beta_unc, 4)),
       col = "red", lwd = 2, bty = "n")

# Unconditional annualized systematic risk
A        <- 252                             # trading days per year
sigma_m  <- sd(as.numeric(returns$SP500))
SR_unc   <- abs(beta_unc) * sigma_m * sqrt(A)

cat("\nAnnualised market volatility:", round(sigma_m * sqrt(A), 4), "\n")
cat("Unconditional Systematic Risk:", round(SR_unc, 4), "\n")


# 4. UNIVARIATE GARCH(1,1)


cat("\n Univariate GARCH Estimation \n")

spec_aapl <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(p_aapl, q_aapl), include.mean = TRUE),
  distribution.model = "std"
)

spec_sp500 <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(p_sp500, q_sp500), include.mean = TRUE),
  distribution.model = "std"
)

fit_aapl  <- ugarchfit(spec_aapl,  data = as.numeric(returns$AAPL),  solver = "hybrid")
fit_sp500 <- ugarchfit(spec_sp500, data = as.numeric(returns$SP500), solver = "hybrid")

cat("\n=== AAPL GARCH(1,1) ===\n");  show(fit_aapl)
cat("\n=== SP500 GARCH(1,1) ===\n"); show(fit_sp500)

# Conditional volatility plots
sigma_aapl <- sigma(fit_aapl)
sigma_sp   <- sigma(fit_sp500)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(dates, sigma_aapl, type = "l", col = "steelblue", lwd = 0.8,
     main = "Conditional Volatility – AAPL",    xlab = "Date", ylab = expression(sigma[t]))
plot(dates, sigma_sp,   type = "l", col = "darkred",   lwd = 0.8,
     main = "Conditional Volatility – S&P 500", xlab = "Date", ylab = expression(sigma[t]))
par(mfrow = c(1, 1))

#  GARCH Diagnostics: ACF of standardised squared residuals 
resid_aapl  <- residuals(fit_aapl,  standardize = TRUE)
resid_sp500 <- residuals(fit_sp500, standardize = TRUE)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(as.numeric(resid_aapl)^2,  main = "ACF – AAPL Std. Resid²",  lag.max = 30)
acf(as.numeric(resid_sp500)^2, main = "ACF – SP500 Std. Resid²", lag.max = 30)
par(mfrow = c(1, 1))

# ARCH-LM on standardised residuals (model adequacy check)
cat("\n ARCH-LM on Standardised Residuals \n")
arch_resid_aapl  <- ArchTest(as.numeric(resid_aapl),  lags = 10)
arch_resid_sp500 <- ArchTest(as.numeric(resid_sp500), lags = 10)

cat("AAPL  std resid – LM stat:", round(arch_resid_aapl$statistic, 2),
    "p =", round(arch_resid_aapl$p.value, 4), "\n")
cat("SP500 std resid – LM stat:", round(arch_resid_sp500$statistic, 2),
    "p =", round(arch_resid_sp500$p.value, 4), "\n")


# 5. MULTIVARIATE DCC-GARCH(1,1)

cat("\n DCC-GARCH Estimation \n")

uspec    <- multispec(list(spec_aapl, spec_sp500))
dcc_spec <- dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = "mvt")

returns_mat <- as.matrix(returns)
dcc_fit     <- dccfit(dcc_spec, data = returns_mat, solver = "solnp")
show(dcc_fit)

# Extract conditional covariance matrices
H_array    <- rcov(dcc_fit)
sigma_im_t <- H_array[1, 2, ]   # Cov(AAPL, SP500)_t
sigma2_m_t <- H_array[2, 2, ]   # Var(SP500)_t
sigma2_i_t <- H_array[1, 1, ]   # Var(AAPL)_t

# DCC conditional correlation
rho_t <- rcor(dcc_fit)[1, 2, ]

plot(dates, rho_t, type = "l", col = "purple", lwd = 0.8,
     main = "DCC Conditional Correlation (AAPL, S&P 500)",
     xlab = "Date", ylab = expression(rho[t]),
     ylim = c(min(rho_t) - 0.05, max(rho_t) + 0.05))
abline(h = mean(rho_t), col = "grey50", lty = 2)
abline(h = cor(as.numeric(returns$AAPL), as.numeric(returns$SP500)),
       col = "red", lty = 3, lwd = 1.5)
legend("bottomleft",
       legend = c("Conditional correlation", "Mean conditional", "Unconditional"),
       col = c("purple", "grey50", "red"), lty = c(1, 2, 3),
       lwd = c(0.8, 1, 1.5), bty = "n", cex = 0.85)


# 6. CONDITIONAL BETA AND SYSTEMATIC RISK

cat("\n Conditional Beta \n")
beta_cond <- sigma_im_t / sigma2_m_t

cat("Summary of conditional beta:\n")
print(summary(beta_cond))
cat("Std Dev:", round(sd(beta_cond), 4), "\n")

# Conditional beta plot
plot(dates, beta_cond, type = "l", col = "steelblue", lwd = 0.8,
     main = "Conditional Beta of AAPL (DCC-GARCH)",
     xlab = "Date", ylab = expression(beta[t]^i))
abline(h = beta_unc,        col = "red",       lty = 2, lwd = 1.5)
abline(h = mean(beta_cond), col = "darkgreen", lty = 3, lwd = 1.5)
legend("topright",
       legend = c("Conditional beta", "Unconditional beta", "Mean conditional"),
       col    = c("steelblue", "red", "darkgreen"),
       lty    = c(1, 2, 3), lwd = c(0.8, 1.5, 1.5), bty = "n", cex = 0.85)

# Annualised conditional systematic risk
SR_cond <- abs(beta_cond) * sqrt(sigma2_m_t) * sqrt(A)

cat("\nSummary of conditional systematic risk:\n")
print(summary(SR_cond))

# Systematic risk plot
plot(dates, SR_cond, type = "l", col = "darkorange", lwd = 0.8,
     main = "Conditional Systematic Risk of AAPL (Annualised)",
     xlab = "Date", ylab = expression(SR[t]^i))
abline(h = SR_unc,        col = "red",       lty = 2, lwd = 1.5)
abline(h = mean(SR_cond), col = "darkgreen", lty = 3, lwd = 1.5)
legend("topright",
       legend = c("Conditional SR", "Unconditional SR", "Mean conditional"),
       col    = c("darkorange", "red", "darkgreen"),
       lty    = c(1, 2, 3), lwd = c(0.8, 1.5, 1.5), bty = "n", cex = 0.85)


# 7. COMPARISON SUMMARY

cat("\n Unconditional vs. Conditional Estimates \n")
cat(sprintf("%-28s %12s %12s\n", "Measure", "Unconditional", "Conditional"))
cat(sprintf("%-28s %12.4f %12.4f\n", "Beta (mean)",         beta_unc,         mean(beta_cond)))
cat(sprintf("%-28s %12s %12.4f\n",   "Beta (std dev)",      "—",              sd(beta_cond)))
cat(sprintf("%-28s %12.4f %12.4f\n", "Beta (min)",          beta_unc,         min(beta_cond)))
cat(sprintf("%-28s %12.4f %12.4f\n", "Beta (max)",          beta_unc,         max(beta_cond)))
cat(sprintf("%-28s %12.4f %12.4f\n", "Systematic Risk (mean)", SR_unc,        mean(SR_cond)))
cat(sprintf("%-28s %12s %12.4f\n",   "SR (std dev)",        "—",              sd(SR_cond)))

# Combined panel plot
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(dates, beta_cond, type = "l", col = "steelblue", lwd = 0.8,
     main = "Conditional Beta of AAPL", xlab = "Date", ylab = expression(beta[t]))
abline(h = beta_unc, col = "red", lty = 2, lwd = 1.5)
plot(dates, SR_cond, type = "l", col = "darkorange", lwd = 0.8,
     main = "Conditional Systematic Risk of AAPL (Annualised)",
     xlab = "Date", ylab = expression(SR[t]))
abline(h = SR_unc, col = "red", lty = 2, lwd = 1.5)
par(mfrow = c(1, 1))
