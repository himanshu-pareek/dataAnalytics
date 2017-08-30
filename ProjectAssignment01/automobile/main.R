#accessing file
filename <- "AUTOMOBILES.csv"

automobiles <- read.csv(filename, na.strings = c("NA", "na", "not available", "NOT AVAILABLE", "Not Available", "?", "-", "_", "null", "NULL"))
head (automobiles)

naData <- automobiles[rowSums(is.na(automobiles)) > 0, ]
head (naData)

automobiles <- na.omit(automobiles)
head (automobiles)

colNames <- c("symboling", "normalized_lossed", "make", "fuel_type", "aspiration",
              "doors", "body_style", "drive_wheels", "engine_loc", "wheel_base",
              "length", "width", "height", "curb_weight", "engine_type", "noc",
              "engine_size", "fuel_system", "bore", "stroke", "comp_ratio", "hp",
              "peak_rpm", "city_mpg", "high_mpg", "price")
colnames(automobiles) <- colNames
head (automobiles)

colClasses <- sapply(automobiles, class)
colClasses

make_data <- subset(automobiles, select = make)
head (make_data)

library (dplyr)
make_data <- group_by (make_data, make)
head (make_data)

make_data <- summarize(make_data, count = n())
head (make_data)

pie(make_data$count, labels = make_data$make, main = "Cars sold by companies")

barplot(make_data$count, names.arg = make_data$make, main = "Bar chart for car companies",
        xlab = "Car companies", ylab = "Number of cars", las = 2, ylim = c(0, 40))

plot(make_data$make, make_data$count,
     main = "Line chart for car companies",
     xlab = "Car companies",
     ylab = "Number of cars", las = 2, ylim = c(0, 40))


#######################################################

noc_data <- subset(automobiles, select = noc)
head (noc_data)

noc_data <- group_by(noc_data, noc)
noc_data <- summarize(noc_data, count = n())
head (noc_data)

numOfCyls <- c("one" = 1,
               "two" = 2,
               "three" = 3,
               "four" = 4,
               "five" = 5,
               "six" = 6,
               "seven" = 7,
               "eight" = 8,
               "nine" = 9,
               "ten" = 10
            )

noc_data$noc <- as.character(noc_data$noc)
noc_data <- mutate(noc_data, lab = numOfCyls[noc])
head (noc_data)

noc_data <- arrange(noc_data, lab)
noc_data

noc_data$noc <- noc_data$lab
noc_data <- subset (noc_data, select = -lab)
noc_data

plot(noc_data$noc, noc_data$count, main = "Car numbers v/s #Cylinders",
     xlab = "Number of cylinders", ylab = "Number of cars", las = 1,
     ylim = c(0, 140))

pie(noc_data$count, labels = noc_data$noc,
    main = "Car numbers v/s number of cylinders in the car")

barplot(noc_data$count, names.arg = noc_data$noc, main = "Bar chart for number of cylinders",
        xlab = "Number of cylinder in car", ylab = "Number of cars", las = 1, ylim = c(0, 140))

#########################################

length_data <- subset(automobiles, select = length)
head (length_data)

hist (length_data$length,
      col = "blue",
      main = "Histogram of length of cars",
      ylim = c(0, 40),
      xlab = "Length of car",
      las = 1)


mean_len <- mean(length_data$length)
var_len <- var(length_data$length)
sd_len <- sd(length_data$length)

sprintf("%s ===> %f", c("Mean", "Variance", "Standard deiation"), c(mean_len, var_len, sd_len))

q <- quantile(length_data$length, probs = c(0, 0.25, 0.50, 0.75, 1.0))
q

##########################################
# 4: Rational - comp_ratio

comp_data <- subset(automobiles, select = comp_ratio)
head (comp_data)

hist (comp_data$comp_ratio,
      col = "blue",
      main = "Histogram of compression ratio",
      ylim = c(0, 150),
      xlim = c(6, 12),
      xlab = "Compression ratio of car",
      las = 1)

hist (comp_data$comp_ratio,
      col = "blue",
      main = "Histogram of compression ratio",
      ylim = c(0, 60),
      xlab = "Compression ratio of car",
      breaks = seq(6, 23, 0.5),
      las = 1)

hist (comp_data$comp_ratio,
      col = "blue",
      main = "Histogram of compression ratio",
      ylim = c(0, 60),
      xlim = c(6, 11),
      xlab = "Compression ratio of car",
      breaks = seq(6, 23, 0.5),
      las = 1)

hist (comp_data$comp_ratio,
      col = "blue",
      main = "Histogram of compression ratio",
      ylim = c(0, 60),
      xlim = c(20, 24),
      xlab = "Compression ratio of car",
      breaks = seq(6, 23, 0.5),
      las = 1)


mean_comp <- mean(comp_data$comp_ratio)
var_comp <- var(comp_data$comp_ratio)
sd_comp <- sd(comp_data$comp_ratio)

sprintf("%s ===> %f", c("Mean", "Variance", "Standard deiation"), c(mean_comp, var_comp, sd_comp))

q <- quantile(comp_data$comp_ratio, probs = c(0, 0.25, 0.50, 0.75, 1.0))
q

########################################

peak_data <- subset(automobiles, select = peak_rpm)
head (peak_data)

hist (peak_data$peak_rpm,
      col = "blue",
      main = "Histogram of peak rpm",
      xlab = "Peak RPM",
      las = 2,
      breaks = seq(4000, 7000, 100))

mean_pr <- mean(peak_data$peak_rpm)
var_pr <- var(peak_data$peak_rpm)
sd_pr <- sd(peak_data$peak_rpm)

sprintf("%s ===> %f", c("Mean", "Variance", "Standard deviation"), c(mean_pr, var_pr, sd_pr))

q <- quantile(peak_data$peak_rpm, probs = c(0, 0.25, 0.50, 0.75, 1.0))
q

###########################################

city_mpg <- subset (automobiles, select = city_mpg)
head (city_mpg)

hist (city_mpg$city_mpg,
      col = "blue",
      main = "Histogram of city mpg",
      xlab = "City MPG",
      breaks = seq(15.0, 50.0, 2.5),
      las = 2)

mean_cm <- mean (city_mpg$city_mpg)
var_cm <- var (city_mpg$city_mpg)
sd_cm <- sd (city_mpg$city_mpg)

sprintf ("%s ===> %f", c("Mean", "Variance", "Standard deviation"),
         c(mean_cm, var_cm, sd_cm))

q <- quantile (city_mpg$city_mpg, probs = c(0, 0.25, 0.50, 0.75, 1.0))
q

