# sGARCH -----------------------------------------------------------------------
# Set the specification for the GARCH model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)),
                   distribution.model = "norm",
                   fixed.pars = list(mu = 0.01, omega = 0.02, alpha1 = 0.05, beta1 = 0.9))

# Simulate the GARCH process
garch.sim <- ugarchpath(spec, n.sim = 1000)

# Extract the simulated returns and conditional variance
returns <- as.numeric(garch.sim@path$seriesSim)
variance <- as.numeric(garch.sim@path$sigmaSim)
sim_data <- reactiveVal(NULL)

# Store results in a reactive value
sim_data(data.frame(time = 1:1000, returns = returns, variance = variance))


# EGARCH -----------------------------------------------------------------------

spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",  # Normal innovations
  fixed.pars = list(
    omega = -0.17,   # α0
    alpha1 = 0.15,   # α1
    gamma1 = -0.13,  # γ
    beta1 = 0.92     # β
  )
)

# Simulate data
sim <- ugarchpath(spec, n.sim = 1000)  # Generate 1000 observations

# Extract simulated returns and conditional variances
returns <- as.numeric(sim@path$seriesSim)   # Simulated return series
sigma_t <- as.numeric(sim@path$sigmaSim)    # Conditional standard deviation

# Plot results
par(mfrow = c(2,1))
plot(returns, type = "l", main = "Simulated EGARCH(1,1) Returns", ylab = "Returns", xlab = "Time")
plot(sigma_t, type = "l", main = "Simulated EGARCH(1,1) Volatility", ylab = "Volatility", xlab = "Time")


# GJR - GARCH ------------------------------------------------------------------

# Define GJR-GARCH(1,1) model specification
spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",  # Normal innovations
  fixed.pars = list(
    omega = 0.02,   # α0 (constant)
    alpha1 = 0.1,   # α1 (GARCH term)
    gamma1 = 0.15,  # γ (leverage effect)
    beta1 = 0.80    # β (persistence)
  )
)

# Simulate data
sim <- ugarchpath(spec, n.sim = 1000)  # Generate 1000 observations

# Extract simulated returns and conditional variances
returns <- as.numeric(sim@path$seriesSim)   # Simulated return series
sigma_t <- as.numeric(sim@path$sigmaSim)    # Conditional standard deviation

# Plot results
par(mfrow = c(2,1))
plot(returns, type = "l", main = "Simulated GJR-GARCH(1,1) Returns", ylab = "Returns", xlab = "Time")
plot(sigma_t, type = "l", main = "Simulated GJR-GARCH(1,1) Volatility", ylab = "Volatility", xlab = "Time")


