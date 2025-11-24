# 0. Preparatory steps ----
# 1. Download the file from Canvas
# 2. Place it in your working directory
# 3. Open the file in Excel
# 4. Remove rows 1 through 9
# 5. Save a copy of the file as CSV (Comma delimited) (.csv)
#    in your working directory, for example: Groep4_Bradford.csv


# 1. Set working directory ----
# In Windows Explorer, you can click next to the search bar to copy the full path.
# Change the backslashes to forward slashes.
setwd("C:/Users") 
getwd()


# 1.1 Load packages
library(ggplot2)
library(tidyr)
library(dplyr)


# 2. Import and clean data ----

# Inside the quotes, put the name of your file. Pay attention to capitals, spaces, etc.
data <- read.csv("Groep4_bradford.csv", sep = ";", header = TRUE) 
str(data) # to check how R interprets the data, characters or numeric


# Convert all columns except the first one to numeric
data[,-1] <- lapply(data[,-1], function(x) as.numeric(gsub(",", ".", trimws(x))))

# Adjust column names <- check your pipetting scheme to confirm these match your group
colnames(data)[1:8]  <- c("Student", "0.0", "0.1", "0.2", "0.4", "0.8", "1.5", "2.0") # BSA dilutions (seen as characters)
colnames(data)[9:13] <- c("FL2.5", "FL10", "dTPR2.5", "dTPR10", "Blanco") # names of other samples

# Check whether it worked
data

# Vector of BSA concentrations, used later
BSA_conc <- c(0.0, 0.1, 0.2, 0.4, 0.8, 1.5, 2.0)


# 3. BSA plot with x axis from 0.0 ug to 2.0 ug ----

# Select only the BSA standards
bsa_data <- data[, c(1:8)]  # columns X to 2.0, assuming BSA dilutions were pipetted in wells 1 to 7

# Convert to long format
bsa_long <- pivot_longer(bsa_data,
                         cols = "0.0":"2.0",
                         names_to = "Amount",
                         values_to = "Absorbance")
names(bsa_long)

# Convert x values to numeric
bsa_long$Amount <- as.numeric(bsa_long$Amount)
head(bsa_long)

# Filter for students A to E (or add F if you have 6 students)
bsa_long <- bsa_long %>% filter(Student %in% c("A", "B", "C", "D", "E"))
# this might require adjustment depending on your dataset

# Plot only points
ggplot(bsa_long, aes(x = Amount, y = Absorbance, colour = Student)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5),
                     limits = c(0, 2)) +
  labs(title = "BSA standard points per student (A to E)",
       x = "BSA concentration (µg/µl)",
       y = "Measured absorbance (595 nm)") +
  theme_minimal()


# 4. Outlier removal example (D2) ----

# Only rows A to E and columns X1 to X7
bsa_data <- data[1:5, 2:8]
rownames(bsa_data) <- data$Student[1:5]
bsa_data

# Set the 0.1 value of student D (row 4) to NA
bsa_data["D", "0.1"] <- NA
bsa_data


# 5. Plot per student including regression line and printed R2 value ----

# Layout for 5 plots
par(mfrow=c(2,3), mar=c(4,4,2,1))

# Vector to store R² values
r2_values <- numeric(nrow(bsa_data))
names(r2_values) <- rownames(bsa_data)

for (i in 1:nrow(bsa_data)) {
  y <- as.numeric(bsa_data[i, 1:7])  # columns 0.0 to 2.0
  x <- BSA_conc                      # true BSA concentrations
  
  # Linear regression using true x values
  fit <- lm(y ~ x)
  
  # Save R²
  r2_values[i] <- summary(fit)$r.squared
  
  # Plot
  plot(x, y, type="p", pch=16, col="blue",
       xlab="BSA amount (µg)", 
       ylab="Measured absorbance (595 nm)",
       main=paste("Student", rownames(bsa_data)[i]),
       ylim=c(min(bsa_data[,1:7], na.rm=TRUE), max(bsa_data[,1:7], na.rm=TRUE)),
       xaxt="n")
  axis(1, at=seq(0, 2, by = 0.5))
  
  # Add regression line
  abline(fit, col="red", lwd=2)
  
  # Add R² 
  x_pos <- max(x)*0.5  
  y_pos <- max(y, na.rm=TRUE)*0.95  
  text(x = x_pos, y = y_pos, labels = paste0("R² = ", round(r2_values[i], 3)))
}

# Reset layout
par(mfrow=c(1,1))

# View R² values
r2_values


# 7. Create a regression model per student ----

# Create empty matrix for output
df <- data.frame(data) 
result <- data.frame(Student = df$Student,
                     FL_2.5 = NA,
                     FL_10 = NA,
                     dTPR_2.5 = NA,
                     dTPR_10 = NA)

# Loop over students A to E = rows 1 to 5
for (i in 1:5) {
  
  # OD values of BSA standards
  OD_BSA <- as.numeric(df[i, 2:8])   # columns 0.0 to 2.0, each student has their own calibration curve
  
  # Create linear regression: OD = a + b * concentration
  model <- lm(OD_BSA ~ BSA_conc)
  # This calculates the line: OD = intercept + slope * concentration
  
  # OD values of unknown samples
  unknown_Sample_OD <- as.numeric(df[i, 9:12])  # columns FL_2.5 to dTPR_10
  
  # Determine OD range of BSA standards
  OD_min <- min(OD_BSA, na.rm = TRUE)
  OD_max <- max(OD_BSA, na.rm = TRUE)
  
  # Check if OD falls within calibration range; otherwise return NA
  predicted_conc <- sapply(unknown_Sample_OD, function(od) {
    if (od >= OD_min & od <= OD_max) {
      (od - coef(model)[1]) / coef(model)[2] 
    } else {
      NA  
    }
  })
  
  # Save results
  result[i, 2:5] <- predicted_conc
}

# View the results
result  # “There was approximately X µg of protein in the well”


# 8. Convert to concentration (ug/ul) ----

# Correct for the volume pipetted into the well
# Values outside the calibration curve remain NA
result$FL_2.5   <- result$FL_2.5   / 2.5
result$FL_10    <- result$FL_10    / 10
result$dTPR_2.5 <- result$dTPR_2.5 / 2.5
result$dTPR_10  <- result$dTPR_10  / 10

result

# If both OD values (2.5 and 10) of FL or dTPR are NA, the output remains NA
