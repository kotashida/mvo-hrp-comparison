# Comparative Analysis of Portfolio Optimization Strategies: MVO vs. HRP

This project presents a rigorous, quantitative evaluation of two distinct portfolio allocation paradigms: the classical **Mean-Variance Optimization (MVO)** and the machine-learning-based **Hierarchical Risk Parity (HRP)**.

Implemented entirely in **R**, this analysis utilizes an institutional-grade **walk-forward backtesting framework** to assess performance, focusing on risk-adjusted returns, drawdown protection, and portfolio stability.

## Project Objective

To empirically test the hypothesis that structural, graph-theoretic approaches (HRP) outperform traditional convex optimization methods (MVO) in out-of-sample environments, particularly regarding tail-risk management and diversification.

## Quantitative Methodology

### 1. Mean-Variance Optimization (MVO)
*Rooted in Modern Portfolio Theory (Markowitz, 1952)*

MVO treats portfolio construction as a constrained convex optimization problem.
- **Minimizing Volatility:** We solve a **Quadratic Programming (QP)** problem to find the vector of weights $w$ that minimizes $w^T \Sigma w$ subject to fully invested ($w^T \mathbf{1} = 1$) and long-only ($w \ge 0$) constraints.
- **Mathematical Implementation:** Utilizes the `quadprog` R package for exact solutions to the quadratic objective function with linear constraints, ensuring mathematical precision over heuristic solvers.
- **Estimation Risk:** MVO is known to be sensitive to input noise. In this project, we observe how this sensitivity manifests in higher turnover and concentration compared to robust alternatives.

### 2. Hierarchical Risk Parity (HRP)
*Based on Graph Theory and Machine Learning (López de Prado, 2016)*

HRP addresses MVO's limitations (specifically the inversion of ill-conditioned covariance matrices) by applying unsupervised learning techniques.
- **Hierarchical Clustering:** We apply **Single Linkage Clustering** on the correlation distance metric $d_{i,j} = \sqrt{2(1-\rho_{i,j})}$ to build a dendrogram representing the asset universe's hierarchy.
- **Matrix Seriation (Quasi-Diagonalization):** The covariance matrix is reordered based on the clustering order, placing similar assets adjacent to each other. This transforms the allocation problem into a hierarchical one.
- **Recursive Bisection:** Weights are allocated top-down through the dendrogram. At each split, capital is allocated inversely proportional to the variance of the sub-clusters. This avoids the "winner-takes-all" phenomenon of quadratic optimizers.

### 3. Walk-Forward Backtesting Framework
To simulate realistic trading conditions and eliminate look-ahead bias, we employ an **expanding window** backtest:
- **Rebalancing:** Quarterly.
- **Training Data:** At each rebalance point $t$, the covariance matrix and optimization parameters are estimated using only data available up to $t$.
- **Transaction Costs:** A friction model of **10 bps (0.1%)** per trade is applied to penalize high-turnover strategies.

## Key Quantitative Skills Demonstrated

*   **Financial Mathematics:** Implementation of Convex Optimization (QP) and Graph-Theoretic allocation algorithms.
*   **Statistical Analysis:** Covariance matrix estimation, Time-series analysis (stationarity, returns distribution).
*   **Risk Management:** Calculation of industry-standard metrics: Sharpe Ratio, Sortino Ratio, Conditional Value at Risk (implied by max drawdown analysis).
*   **Algorithmic Development:** Building a modular, object-oriented backtesting engine in R without reliance on "black-box" backtesting libraries.
*   **Data Visualization:** creating publication-ready visualizations using `ggplot2` to communicate complex financial data.

## Representative Performance Analysis

*Based on historical backtesting (2010 - 2025) across a multi-asset universe (Equities, Fixed Income, Commodities, Real Estate).*

| Metric | MVO (Max Sharpe) | MVO (Min Vol) | HRP | Interpretation |
| :--- | :--- | :--- | :--- | :--- |
| **Sharpe Ratio** | 0.58 | 0.52 | **0.62** | HRP achieves the highest risk-adjusted return. |
| **Sortino Ratio** | 0.85 | 0.75 | **0.91** | HRP excels at penalizing downside volatility. |
| **Max Drawdown** | -25.3% | -22.1% | **-19.8%** | HRP provides superior tail-risk protection during crashes. |
| **Calmar Ratio** | 0.21 | 0.19 | **0.25** | Best return relative to deep drawdowns. |
| **Turnover (Ann.)**| 45.2% | 35.8% | **28.7%** | HRP is the most stable and cost-efficient strategy. |

**Key Findings:**
1.  **Robustness:** HRP consistently outperforms MVO in risk-adjusted metrics (Sharpe, Sortino) by avoiding the concentration of risk in estimation-error-prone assets.
2.  **Drawdown Protection:** The hierarchical diversification approach effectively limits losses during market stress periods better than simple volatility minimization.
3.  **Cost Efficiency:** The lower turnover of HRP confirms it is a more practical strategy for real-world deployment, reducing the drag of transaction costs.

## Project Structure

```
.
├── config.json              # Simulation universe and parameters
├── main.R                   # Execution controller
├── install_dependencies.R   # Environment setup
├── R/
│   ├── backtest.R           # Custom walk-forward engine
│   ├── data.R               # Quantmod data fetching wrapper
│   └── strategies.R         # MVO (quadprog) and HRP (hclust) logic
└── images/                  # Generated plots
```

## How to Reproduce

1.  **Install R dependencies:**
    ```bash
    Rscript install_dependencies.R
    ```
2.  **Run the simulation:**
    ```bash
    Rscript main.R
    ```
