library(ggplot2)

# Sample Data
set.seed(123)
# basically we just created a sample dataframe with two columns x and y with randomized values
df <- data.frame(x = 1:100, y = sin(1:100/10) + rnorm(100, 0, 0.2))


# loess smoothing basically provides a model that "learns" the pattern of the dataframe
# Plot with LOESS smoothing
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2, color = "blue") + # LOESS curve
  theme_minimal()
