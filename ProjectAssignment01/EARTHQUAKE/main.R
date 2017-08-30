# loading data into R
# filename is "EARTHQUAKE.xlsx"
filename <- "EARTHQUAKE.xlsx"

library(readxl)
library(dplyr)

earthquakes <- read_excel(filename, na = c("Not Available", "not available", "NOT AVAILABLE", "na", "NA", "-", "_", "null", "NULL"))
head (earthquakes)

# number of discrepancy is number of rows with na values
# this can be calculated by is.na() function
dataOfNAValues <- earthquakes[rowSums(is.na(earthquakes)) > 0,]
head (dataOfNAValues)

# Here, dataOfNAValues has 0 rows
# means, there is not any discrepancy in our cata
# If we somehow forgot to keep names of possible na strings while
# reading excel file, we have to use omit function to 
# remove all na or not suitable values
earthquakes <- na.omit(earthquakes)
head (earthquakes)

# We will rename columns to ease of writing
colNames <- c("time", "lat", "long", "depth", "mag")
colnames(earthquakes) <- colNames
head (earthquakes)

# Now, we will calculate 5 point summary for depth and mag
new_data <- subset(earthquakes, select = c(depth, mag))

# Boxplot before removing outliers
boxplot(new_data$depth, main = "Boxplot for depth in Km (Before removing outliers)")
boxplot(new_data$mag, main = "Boxplot for magnitude (Before removing outliers)")

# Calculating 5 points
quantileDepth <- quantile(new_data$depth, probs = c(0, 0.25, 0.50, 0.75, 1.0))
quantileMag <- quantile(new_data$mag, probs = c(0, 0.25, 0.50, 0.75, 1.0))
quantileDepth
quantileMag

# Calculating minimum and maximum of depth and magnitude
q1Depth <- quantileDepth[2]
q3Depth <- quantileDepth[4]
q1Mag <- quantileMag[2]
q3Mag <- quantileMag[4]

IQRDepth <- q3Depth - q1Depth
IQRMag <- q3Mag - q1Mag

minDepth <- q1Depth - (1.5 * IQRDepth)
maxDepth <- q3Depth + (1.5 * IQRDepth)
minMag <- q1Mag - (1.5 * IQRMag)
maxMag <- q3Mag + (1.5 * IQRMag)

sprintf("Range of %s should be %f - %f\n", c("Depth", "Magnitude"), c(minDepth, minMag), c(maxDepth, maxMag))

# Now, we will remove data below minimum limit
# and above maximum limit
new_data <- filter(new_data, depth >= minDepth & depth <= maxDepth & mag >= minMag & mag <= maxMag)

# Again we will plot boxplot and calculate the 5 point summary
sprintf("After removing outliers")
# Boxplot after removing outliers
boxplot(new_data$depth, main = "Boxplot for depth in Km (After removing outliers)")
boxplot(new_data$mag, main = "Boxplot for magnitude (After removing outliers)")

# Calculating 5 points summary
quantileDepth <- quantile(new_data$depth, probs = c(0, 0.25, 0.50, 0.75, 1.0))
quantileMag <- quantile(new_data$mag, probs = c(0, 0.25, 0.50, 0.75, 1.0))
quantileDepth
quantileMag

# We can see that after removing outliers, boxplot is much more symmetric
# and we don't have any data in outliers
# so that we can accept all the data in new_data

