# Question 1 [5]
# Student number: 10501541 → Use Cement from aus_production

library(fpp3)
library(latex2exp)
library(lintr)
library(styler)

# Extract Cement production time series in millions of tonnes
cement_production <- aus_production |>
  select(Quarter, Cement_kt = Cement) |> # Cement in kilotonnes (thousands)
  mutate(Cement_mt = Cement_kt / 1000) # Convert to megatonnes (millions)

# Estimate optimal Box-Cox lambda using Guerrero's method
lambda_guerrero <- cement_production |>
  features(Cement_mt, features = guerrero) |>
  pull(lambda_guerrero)

# Add Box-Cox transformed column
cement_production <- cement_production |>
  mutate(Cement_mt_BoxCox = box_cox(Cement_mt, lambda_guerrero))

# Plot original vs transformed series using inline reshaping
cement_production |>
  pivot_longer(
    cols = c(Cement_mt, Cement_mt_BoxCox),
    names_to = "Type",
    values_to = "Value"
  ) |>
  mutate(
    Type = factor(
      Type,
      levels = c("Cement_mt", "Cement_mt_BoxCox"),
      labels = c("Original (Production Volume in megatonnes)", "Box-Cox Transformed")
    )
  ) |>
  ggplot(aes(x = Quarter, y = Value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(
    title = "Cement Production Volume in Australia",
    subtitle = TeX(paste0(
      "Variance Stabilisation via Box-Cox: $\\lambda \\approx ", round(lambda_guerrero, 3), "$"
    )),
    y = "Value",
    x = "Year",
    caption = "Quarterly cement production in Australia from 1956 to 2010."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# ------------------------------------------------------------------------------
# COMMENT:
# The Box-Cox transformation parameter estimated using the Guerrero method is
# approximately λ = –0.309, indicating that a transformation similar to an
# inverse square root is optimal. This aligns with visual evidence of
# increasing variance in the original series. Applying the transformation produces
# a series with more stable variance, which is essential for time series modeling
# and forecasting.
# ------------------------------------------------------------------------------
