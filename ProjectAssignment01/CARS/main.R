# reading data into R
# filename is "CARS.csv"
filename <- "CARS.csv"
cars <- read.csv(filename, row.names = 1, na.strings = c("Not Available", "NOT AVAILABLE", "NULL", "NA", "na", "not available", "-", "_", "X"))

# removing na values from data
cars <- cars[complete.cases(cars), ]

# import dplyr package
library(dplyr)

# We will group data with respect to speed of the car
cars <- group_by (cars,speed)

# We will thtan take average of speeds in each group
cars <- summarize(cars, mean_dist = mean(dist))

# Now we will calculate weighted means (AM, GM and HM)
weighted_am <- weighted.mean(cars$mean_dist, cars$speed)
weighted_gm <- exp(weighted.mean(log(cars$mean_dist), cars$speed))
weighted_hm <- 1 / (weighted.mean(1 / (cars$mean_dist), cars$speed))

# printing weighted am, gm and hm
sprintf (
    "%s  = %f",
    c("WEIGHTED AM", "WEIGHTED GM", "WEIGHTED HM"),
    c(weighted_am, weighted_gm, weighted_hm)
)

# Here range and observations both are different, so we will consider GM
# We can further visualize data using plot
plot (
    cars$speed,
    cars$mean_dist,
    main = "Car meandist v/s speed",
    xlab = "Speed of car",
    ylab = "Mean distance"
)

# We can see that relation between mean distance and speed 
# is approximately linear
