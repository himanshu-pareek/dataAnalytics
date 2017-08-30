#filename is "IRIS.xlsx"
filename <- "IRIS.xlsx"

library(readxl)

iris <- read_excel (
    filename,
    na = c("Not Available", "not available", "NOT AVAILABLE", "na", "NA", "null", "NULL", "-", "_", "?"))
head (iris)

# removing na values (if any)
iris <- na.omit(iris)

# Now, we have to delete Id column
iris <- subset(iris, select = -Id)
head (iris)

# Creating random indices for sample 
sample_index <- sample.int(nrow(iris), 50)

# sample_iris 
sample_iris <- iris[sample_index, ]
head (sample_iris)

library(dplyr)

# Now we will group sample_iris by Species
sample_iris <- group_by (sample_iris, Species)

# summrize data to get meand and variances
sample_iris_summary <- summarize(sample_iris, 
                                 sl_mean = mean(SepalLengthCm),
                                 sw_mean = mean(SepalWidthCm),
                                 pl_mean = mean(PetalLengthCm),
                                 pw_mean = mean(PetalWidthCm),
                                 sl_var = var(SepalLengthCm),
                                 sw_var = var(SepalWidthCm),
                                 pl_var = var(PetalLengthCm),
                                 pw_var = var(PetalWidthCm)
                            )

# Grouping of original data
iris_group <- group_by(iris, Species)

# Summarizing original data
iris_summary <- summarize(iris_group, 
                                 sl_mean = mean(SepalLengthCm),
                                 sw_mean = mean(SepalWidthCm),
                                 pl_mean = mean(PetalLengthCm),
                                 pw_mean = mean(PetalWidthCm),
                                 sl_var = var(SepalLengthCm),
                                 sw_var = var(SepalWidthCm),
                                 pl_var = var(PetalLengthCm),
                                 pw_var = var(PetalWidthCm)
)

# printing the summary
sample_iris_summary
iris_summary

# Here we can see that mean and variance of 
# sample iris data (n = 50) and original 
# iris data (n = 150) are very much similar
# But if we look closer we see that Variance
# of PetalWidth for population is lesser than that
# for sample baut all other variances are somewhat
# greater for population data




