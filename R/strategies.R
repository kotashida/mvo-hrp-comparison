library(quadprog)
library(stats)

# --- MVO Strategies ---

# Minimum Volatility Portfolio using Quadratic Programming
get_mvo_min_vol_weights <- function(returns) {
  n_assets <- ncol(returns)
  cov_mat <- cov(returns)
  
  # Objective: Minimize 1/2 w^T * Sigma * w
  # quadprog solves: min(-d^T b + 1/2 b^T D b) s.t. A^T b >= b0
  
  d_vec <- rep(0, n_assets)
  D_mat <- cov_mat
  
  # Constraints:
  # 1. Sum of weights = 1
  # 2. Weights >= 0
  
  # A^T matrix (Note: quadprog expects A^T to be passed as Amat)
  # Row 1: 1, 1, ..., 1 (Sum = 1)
  # Rows 2..n+1: Identity matrix (w_i >= 0)
  
  A_mat <- cbind(rep(1, n_assets), diag(n_assets))
  b_vec <- c(1, rep(0, n_assets))
  
  # Solve (first constraint is equality)
  sol <- solve.QP(Dmat = D_mat, dvec = d_vec, Amat = A_mat, bvec = b_vec, meq = 1)
  
  weights <- sol$solution
  names(weights) <- colnames(returns)
  return(weights)
}

# Maximum Sharpe Ratio Portfolio using general optimization
get_mvo_sharpe_weights <- function(returns, risk_free_rate = 0) {
  n_assets <- ncol(returns)
  mu <- colMeans(returns) * 252 # Annualized expected returns
  cov_mat <- cov(returns) * 252 # Annualized covariance
  
  # Objective function: Negative Sharpe Ratio
  neg_sharpe <- function(w) {
    # Penalize if weights don't sum to 1 (soft constraint for optim, but we'll normalize)
    w <- w / sum(w) 
    port_ret <- sum(w * mu)
    port_vol <- sqrt(t(w) %*% cov_mat %*% w)
    return(- (port_ret - risk_free_rate) / port_vol)
  }
  
  # Constraints handling for 'optim' (L-BFGS-B allows bounds, but not equality constraints easily)
  # Alternative: Use solnp or just normalize inside objective. 
  # A better approach for Sharpe in R without heavy deps is simply:
  # Maximize (mu - rf)' * w / sqrt(w' Sigma w).
  # If rf=0, this is proportional to Tangency portfolio. 
  # Tangency portfolio w ~ Sigma^-1 * (mu - rf). Normalize to sum to 1.
  
  # Analytical solution for Tangency Portfolio (Unconstrained except sum=1, but we need w>=0)
  # Since we need w>=0, analytical solution doesn't strictly hold if negative weights required.
  # So we use numeric optimization.
  
  # Using constrOptim
  ui <- rbind(rep(1, n_assets), rep(-1, n_assets), diag(n_assets))
  ci <- c(0.999, -1.001, rep(0, n_assets)) # Relax equality slightly for inequality solver or use meq
  
  # Actually, let's use a simple SQP or iterate. 
  # Let's stick to the Python logic: Minimize negative Sharpe.
  # We'll use slsqp from nloptr or simply normalize weights in the objective function 
  # and optimize over n-1 parameters or just use constrained optimization.
  # To avoid extra dependencies like nloptr, we'll use base 'optim' with transformation 
  # or 'solve.QP' iteratively.
  
  # Simplest robust 'base' R approach for w>=0, sum(w)=1: 
  # Use quadprog to find Mean-Variance efficient portfolio for a specific return target?
  # No, Sharpe is max gradient.
  
  # Let's use the standard workaround: Optimize for Min Variance s.t. Return = Target.
  # Sweep targets? No, that's slow.
  
  # Let's use 'constrOptim' which is in 'stats'.
  # It minimizes function s.t. ui %*% theta - ci >= 0.
  # Equality sum(w)=1 is hard.
  
  # Revert to simple trick: Tangency portfolio = Sigma_inv * excess_returns.
  # Then set negative weights to 0? No, that's wrong.
  
  # Okay, we will use a dedicated function that maps to quadprog logic iteratively 
  # OR just use a simple random search / optimization if n is small? 
  # No, we want "Improved".
  
  # Let's use the 'solve.QP' trick for Sharpe:
  # Max (mu-rf)'x / sqrt(x'Dx) s.t. sum(x)=1, x>=0
  # Equivalent to: Min x'Dx s.t. (mu-rf)'x = 1, x>=0 (Unnormalized weights y)
  # Then w = y / sum(y).
  # This finds the tangency portfolio efficiently!
  
  excess_mu <- mu - risk_free_rate
  # If all excess returns are negative, this fails. Assume at least one positive.
  if(all(excess_mu < 0)) {
     # Fallback to Min Vol if no positive expected returns
     return(get_mvo_min_vol_weights(returns))
  }
  
  # Setup for solve.QP
  # Min 1/2 y'Dy - 0
  # s.t. (mu-rf)'y = 1
  #      y >= 0
  
  D_mat <- cov_mat
  d_vec <- rep(0, n_assets)
  
  # Constraints:
  # 1. Excess Return = 1 (Equality) -> excess_mu
  # 2. y >= 0 -> Identity
  
  A_mat <- cbind(excess_mu, diag(n_assets))
  b_vec <- c(1, rep(0, n_assets))
  
  tryCatch({
    sol <- solve.QP(Dmat = D_mat, dvec = d_vec, Amat = A_mat, bvec = b_vec, meq = 1)
    y <- sol$solution
    weights <- y / sum(y)
    names(weights) <- colnames(returns)
    return(weights)
  }, error = function(e) {
    # Fallback
    return(get_mvo_min_vol_weights(returns))
  })
}


# --- HRP Strategy ---

get_hrp_weights <- function(returns) {
  cov_mat <- cov(returns)
  corr_mat <- cor(returns)
  
  # 1. Hierarchical Clustering
  # Distance matrix
  dist_mat <- sqrt(2 * (1 - corr_mat))
  d <- as.dist(dist_mat)
  
  # Clustering (Single linkage per original paper, or Ward)
  # Python code used 'single'.
  clust <- hclust(d, method = "single")
  
  # 2. Quasi-Diagonalization (Matrix Seriation)
  ordered_indices <- clust$order
  
  # 3. Recursive Bisection
  weights <- get_rec_bipart(cov_mat, ordered_indices)
  names(weights) <- colnames(returns)
  
  # Sort by index to match original asset order
  weights <- weights[colnames(returns)]
  return(weights)
}

get_rec_bipart <- function(cov_mat, sort_ix) {
  # sort_ix is a vector of integer indices
  weights <- rep(1, ncol(cov_mat))
  names(weights) <- colnames(cov_mat) # Init with names but we use indices for logic
  
  # We work with lists of indices
  c_items <- list(sort_ix)
  
  while(length(c_items) > 0) {
    # Split items
    c_items_new <- list()
    
    for(items in c_items) {
      if(length(items) > 1) {
        # Split in half
        split_idx <- floor(length(items) / 2)
        idx0 <- items[1:split_idx]
        idx1 <- items[(split_idx + 1):length(items)]
        
        c_items_new[[length(c_items_new) + 1]] <- idx0
        c_items_new[[length(c_items_new) + 1]] <- idx1
        
        # Calculate variance of clusters
        var0 <- get_cluster_var(cov_mat, idx0)
        var1 <- get_cluster_var(cov_mat, idx1)
        
        # Calculate alpha
        alpha <- 1 - var0 / (var0 + var1)
        
        # Update weights (using asset names)
        names0 <- colnames(cov_mat)[idx0]
        names1 <- colnames(cov_mat)[idx1]
        
        weights[names0] <- weights[names0] * alpha
        weights[names1] <- weights[names1] * (1 - alpha)
      }
    }
    c_items <- c_items_new
  }
  
  return(weights)
}

get_cluster_var <- function(cov_mat, cluster_idx) {
  # Extract sub-covariance
  sub_cov <- cov_mat[cluster_idx, cluster_idx, drop=FALSE]
  
  # Inverse variance weights
  iv_weights <- 1 / diag(sub_cov)
  iv_weights <- iv_weights / sum(iv_weights)
  
  # Cluster variance
  var <- t(iv_weights) %*% sub_cov %*% iv_weights
  return(as.numeric(var))
}
