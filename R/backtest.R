library(xts)
library(PerformanceAnalytics)

run_backtest <- function(returns, strategy_func, rebalance_freq = "quarters", transaction_costs = 0.001, lookback_window = NULL, ...) {
  # returns: xts object of asset returns
  # strategy_func: function(history_returns, ...) returning named weights
  # rebalance_freq: "days", "weeks", "months", "quarters", "years"
  # lookback_window: integer (number of days) or NULL (expanding window)
  
  # Identify rebalance dates
  # We need to start after we have enough data. 
  # Let's assume we need at least 6 months of data roughly (126 days) if lookback is NULL
  # or lookback_window size.
  
  min_history <- if(is.null(lookback_window)) 126 else lookback_window
  
  # Get endpoints
  ep <- endpoints(returns, on = rebalance_freq)
  
  # Ensure we have enough data for the first rebalance
  # Find the first endpoint that satisfies min_history
  first_ep_idx <- which(ep > min_history)[1]
  
  if(is.na(first_ep_idx)) {
    stop("Not enough data for backtest with specified lookback/frequency.")
  }
  
  # Initialize results
  # We will store portfolio returns.
  # The period before first rebalance is 0 or NA? Usually NA or benchmark (not handled here).
  # We start generating returns AFTER the first rebalance date.
  
  port_returns <- xts(rep(0, nrow(returns)), order.by = index(returns))
  # Mark pre-start as NA
  start_idx <- ep[first_ep_idx]
  port_returns[1:start_idx] <- 0 # No position
  
  current_weights <- rep(0, ncol(returns))
  names(current_weights) <- colnames(returns)
  
  total_turnover <- 0
  turnover_count <- 0
  
  # Loop through rebalance periods
  # We calculate weights at ep[i], apply them for (ep[i]+1) to ep[i+1]
  
  # Store weights for plotting
  weights_history <- list()
  
  for(i in first_ep_idx:(length(ep)-1)) {
    t_idx <- ep[i]
    next_t_idx <- ep[i+1]
    
    # Define history window
    history_start <- if(is.null(lookback_window)) 1 else max(1, t_idx - lookback_window + 1)
    history_data <- returns[history_start:t_idx, ]
    
    # Calculate new weights
    # We wrap this in tryCatch to ensure robustness
    new_weights <- tryCatch({
      w <- strategy_func(history_data, ...)
      # Ensure non-negative and sum to 1 just in case
      w[w < 0] <- 0
      w <- w / sum(w)
      w
    }, error = function(e) {
      warning(paste("Strategy failed at index", t_idx, ":", e$message))
      return(current_weights) # Fallback to hold
    })
    
    # Calculate Turnover
    turnover <- sum(abs(new_weights - current_weights)) / 2
    total_turnover <- total_turnover + turnover
    turnover_count <- turnover_count + 1
    
    # Apply weights to next period
    # Returns for next period
    period_indices <- (t_idx + 1):next_t_idx
    if(length(period_indices) > 0) {
      period_ret_data <- returns[period_indices, ]
      
      # Portfolio return = sum(w * r)
      # Note: This assumes daily rebalancing to keep fixed weights? 
      # Or Buy-and-Hold between rebalancing?
      # Standard backtest often assumes fixed weights (daily rebalanced) or drift.
      # The Python code did: (returns * weights).sum() which implies fixed weights (daily rebalancing to target)
      # OR it assumes the weights drift but calculates simple dot product daily. 
      # (w * r) sum is effectively daily rebalancing.
      # Drift would be: calculate value evolution.
      
      # Let's assume Daily Rebalancing to Target Weights (simplifies calc, standard for "Strategy Index")
      # This matches the Python logic (period_returns * weights).sum()
      
      # Transaction cost: applied once at rebalance date? 
      # Python code: costs = turnover * cost. subtracted from PERIOD returns? 
      # That's a bit odd. Usually applied at the trade moment.
      # We will subtract cost from the FIRST day of the new period.
      
      r_port <- Return.portfolio(period_ret_data, weights = new_weights, rebalance_on = NA, verbose = FALSE)
      
      # Subtract transaction costs from the first day return
      r_port[1] <- r_port[1] - (turnover * transaction_costs)
      
      port_returns[period_indices] <- r_port
    }
    
    current_weights <- new_weights
    weights_history[[as.character(index(returns)[t_idx])]] <- new_weights
  }
  
  # Trim to start date
  port_returns <- port_returns[(ep[first_ep_idx]+1):nrow(port_returns)]
  
  avg_turnover <- if(turnover_count > 0) total_turnover / turnover_count else 0
  
  return(list(
    returns = port_returns,
    turnover = avg_turnover,
    final_weights = current_weights,
    weights_history = do.call(rbind, weights_history)
  ))
}
