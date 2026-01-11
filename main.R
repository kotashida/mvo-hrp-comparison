# Main execution script

# Load libraries
library(jsonlite)
library(ggplot2)
library(reshape2)
library(PerformanceAnalytics)

# Source modules
source("R/data.R")
source("R/strategies.R")
source("R/backtest.R")

# 1. Load Configuration
if (!file.exists("config.json")) {
  stop("config.json not found!")
}
config <- fromJSON("config.json")

# Map frequency to endpoints args
freq_map <- list(
  "quarterly" = "quarters",
  "monthly" = "months",
  "yearly" = "years",
  "daily" = "days"
)
rebal_freq <- freq_map[[config$rebalancing_frequency]]
if(is.null(rebal_freq)) rebal_freq <- "quarters"

message("Configuration Loaded.")
message("Assets: ", paste(config$assets, collapse = ", "))
message("Period: ", config$start_date, " to ", config$end_date)

# 2. Fetch Data
message("Fetching data...")
returns <- fetch_data(config$assets, config$start_date, config$end_date)

if(nrow(returns) == 0) {
  stop("No data fetched.")
}
message("Data fetched successfully. Rows: ", nrow(returns))

# 3. Define Strategy Wrappers
# The backtester expects function(history, ...)
# We bind 'risk_free_rate' if needed

rf_rate <- config$risk_free_rate

# Wrapper for MVO Sharpe
strat_mvo_sharpe <- function(hist) {
  get_mvo_sharpe_weights(hist, risk_free_rate = rf_rate)
}

# Wrapper for MVO Min Vol
strat_mvo_minvol <- function(hist) {
  get_mvo_min_vol_weights(hist)
}

# Wrapper for HRP
strat_hrp <- function(hist) {
  get_hrp_weights(hist)
}

# 4. Run Backtests
message("Running Backtests...")

# We use an expanding window (lookback_window = NULL) or fixed?
# Python code implies expanding (using all available data).
# We'll use expanding window (NULL).

res_mvo_sharpe <- run_backtest(returns, strat_mvo_sharpe, rebalance_freq = rebal_freq, transaction_costs = config$transaction_costs)
message("MVO (Sharpe) Done.")

res_mvo_minvol <- run_backtest(returns, strat_mvo_minvol, rebalance_freq = rebal_freq, transaction_costs = config$transaction_costs)
message("MVO (Min Vol) Done.")

res_hrp <- run_backtest(returns, strat_hrp, rebalance_freq = rebal_freq, transaction_costs = config$transaction_costs)
message("HRP Done.")

# 5. Compile Results
# Merge returns
all_returns <- merge(res_mvo_sharpe$returns, res_mvo_minvol$returns, res_hrp$returns)
colnames(all_returns) <- c("MVO (Sharpe)", "MVO (Min Vol)", "HRP")
all_returns <- na.omit(all_returns)

# 6. Calculate Metrics
calc_metrics <- function(rets, weights, turnover, rf) {
  # Annualized metrics
  ann_ret <- Return.annualized(rets, scale = 252)
  ann_sd <- StdDev.annualized(rets, scale = 252)
  sharpe <- SharpeRatio.annualized(rets, Rf = rf, scale = 252)
  sortino <- SortinoRatio(rets, MAR = rf) * sqrt(252) # Approximate annualized
  max_dd <- maxDrawdown(rets)
  calmar <- ann_ret / abs(max_dd)
  
  # Herfindahl (Concentration)
  # Based on final weights? Or Average weights?
  # Python code used final weights (which is random snapshot). 
  # Better: Average Herfindahl over time.
  # But we only have final weights easily accessible unless we average weights_history.
  # Let's use final weights to match Python simplicity, or verify weights_history exists.
  herfindahl <- sum(weights^2)
  
  return(c(
    Sharpe = as.numeric(sharpe),
    Sortino = as.numeric(sortino),
    MaxDD = as.numeric(max_dd),
    Calmar = as.numeric(calmar),
    Herfindahl = herfindahl,
    Turnover = turnover
  ))
}

metrics_mvo_s <- calc_metrics(all_returns[,1], res_mvo_sharpe$final_weights, res_mvo_sharpe$turnover, rf_rate/252)
metrics_mvo_mv <- calc_metrics(all_returns[,2], res_mvo_minvol$final_weights, res_mvo_minvol$turnover, rf_rate/252)
metrics_hrp <- calc_metrics(all_returns[,3], res_hrp$final_weights, res_hrp$turnover, rf_rate/252)

results_table <- data.frame(
  Metric = c("Sharpe Ratio", "Sortino Ratio", "Max Drawdown", "Calmar Ratio", "Herfindahl Index", "Annual Turnover"),
  `MVO (Sharpe)` = c(metrics_mvo_s['Sharpe'], metrics_mvo_s['Sortino'], metrics_mvo_s['MaxDD'], metrics_mvo_s['Calmar'], metrics_mvo_s['Herfindahl'], metrics_mvo_s['Turnover']),
  `MVO (Min Vol)` = c(metrics_mvo_mv['Sharpe'], metrics_mvo_mv['Sortino'], metrics_mvo_mv['MaxDD'], metrics_mvo_mv['Calmar'], metrics_mvo_mv['Herfindahl'], metrics_mvo_mv['Turnover']),
  `HRP` = c(metrics_hrp['Sharpe'], metrics_hrp['Sortino'], metrics_hrp['MaxDD'], metrics_hrp['Calmar'], metrics_hrp['Herfindahl'], metrics_hrp['Turnover'])
)

print(results_table)

# 7. Plots
if(!dir.exists("images")) dir.create("images")

# Cumulative Returns
cum_returns <- cumprod(1 + all_returns) - 1
cum_returns_df <- data.frame(Date = index(cum_returns), coredata(cum_returns))
cum_returns_long <- melt(cum_returns_df, id.vars = "Date", variable.name = "Strategy", value.name = "CumulativeReturn")

p1 <- ggplot(cum_returns_long, aes(x = Date, y = CumulativeReturn, color = Strategy)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Portfolio Performance Comparison", y = "Cumulative Returns", x = "Date") +
  theme(legend.position = "bottom")

ggsave("images/performance_comparison.png", plot = p1, width = 10, height = 6)

# Final Weights Comparison
weights_df <- data.frame(
  Asset = names(res_mvo_sharpe$final_weights),
  `MVO (Sharpe)` = res_mvo_sharpe$final_weights,
  `MVO (Min Vol)` = res_mvo_minvol$final_weights,
  `HRP` = res_hrp$final_weights
)
weights_long <- melt(weights_df, id.vars = "Asset", variable.name = "Strategy", value.name = "Weight")

p2 <- ggplot(weights_long, aes(x = Asset, y = Weight, fill = Strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Portfolio Weight Comparison (Final)", y = "Weight", x = "Asset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/weights_comparison.png", plot = p2, width = 10, height = 6)

message("Plots saved to images/")
