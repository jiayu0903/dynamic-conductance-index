
library(readxl)

data <- read_excel("Ig data for DynG calculation.xlsx")

# Fixed gpw value
gpw <- c(0.293265448,0.204828956,0.104634834)

# Initialize a vector to store the slope of each line
slopes <- numeric(nrow(data))

# Cycle through each row to calculate the slope of the linear regression
for (i in 1:nrow(data)) {
  Ig_row <- as.numeric(data[i, 2:4])  # Extract Ig data for each row
  model <- lm(gpw ~ Ig_row -1)           # Fitting a linear model
  slopes[i] <- coef(model)[1]         # extraction slope
}

# Create a new data box to save the results
results <- data.frame(Time = data$`Time (s)`, Slope = slopes)

write.csv(results, "DynG_results.csv", row.names = FALSE)

