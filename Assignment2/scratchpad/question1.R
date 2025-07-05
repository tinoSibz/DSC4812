# Question 1 [5]
# Student number: 10501541 → Use Cement from aus_production

library(fpp3)

# Extract Cement production time series
cement_production <- aus_production |> 
  select(Quarter, Cement)

# Plot original series
cement_production |>
  autoplot(Cement) +
  labs(
    title = "Cement Production in Australia (1956 Q1 - 2010 Q2)",
    subtitle = "Quarterly estimates of Portland cement production",
    x = "Year",
    y  = "Production volume (thousand tonnes)",
    caption = "Source: Australian Bureau of Statistics (Catalogue 8301.0.55.001)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal(base_size = 14)

# Seasonal and Subseries Plots
cement_production |>
  gg_season(Cement) +
  labs(
    y = "Cement produced (thousands of tonnes)",
    title = "Seasonal Plot: Quarterly Cement Production in Australia",
    subtitle = "Shows seasonal variation in Portland cement production by quarter",
    caption = "Source: Australian Bureau of Statistics (Catalogue 8301.0.55.001)"
  ) +
  theme_minimal(base_size = 14)

cement_production |>
  gg_subseries(Cement) +
  labs(
    y = "Cement produced (thousands of tonnes)",
    title = "Subseries Plot: Quarterly Cement Production in Australia",
    subtitle = "Highlights seasonal patterns across quarters over time",
    caption = "Source: Australian Bureau of Statistics (Catalogue 8301.0.55.001)"
  ) +
  theme_minimal(base_size = 14)

# Lag Plot
cement_production |>
  gg_lag(Cement, geom = "point") +
  labs(
    x = "Lagged Cement Production",
    y = "Cement Production",
    title = "Lag Plot of Cement Production",
    subtitle = "Scatterplot of Cement production vs lagged values",
    caption = "Source: Australian Bureau of Statistics (Catalogue 8301.0.55.001)"
  ) +
  theme_minimal(base_size = 14)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda_guerrero <- cement_production |> 
  features(Cement, features = guerrero) |>
  pull(lambda_guerrero)

# Print lambda
lambda_guerrero

# Apply Box-Cox transformation and plot
cement_production |>
  autoplot(box_cox(Cement, lambda_guerrero)) +
  labs(
    title = "Box-Cox Transformed Cement Production (λ ≈ -0.31)",
    subtitle = "Transformed to stabilize variance for modeling",
    x = "Year",
    y  = "Transformed Production Volume",
    caption = "Box-Cox transformation applied using Guerrero's method. Source: ABS (Catalogue 8301.0.55.001)."
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# COMMENT:
# The Box-Cox transformation parameter estimated using the Guerrero method is 
# approximately λ = –0.31, indicating that a transformation similar to an 
# inverse square root is optimal. This aligns with visual evidence of 
# increasing variance in the original series, particularly from the 1970s onward. 
# Applying the transformation produces a series with more stable variance, 
# which is essential for accurate time series modeling and forecasting.
# ------------------------------------------------------------------------------
