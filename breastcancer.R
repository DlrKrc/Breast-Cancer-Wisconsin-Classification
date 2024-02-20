library(readxl)
tumor <- read_excel("Downloads/tumor.xlsx")
View(tumor)
# Calculating total NA values
sum(is.na(tumor))

# Assigning NA value
tumor[5,1] <- NA

# Recalculating total NA values
sum(is.na(tumor))

# Investigating patterns of NA values using md.pattern() function
md.pattern(tumor, rotate.names = TRUE)

# Performing statistical analysis on the original data
summary(tumor)

# Calculating column means from column 2 to 10
means <- colMeans(tumor[, 2:10])
print(means)

# Checking the structure of the dataset and calculating standard deviations of all columns separately
str(tumor)
standard_deviation <- apply(tumor[, 1:11], 2, sd)
print(standard_deviation)

# Visualizing tumors count using barplot
barplot(table(tumor$Class), col=c("lightblue", "lightcoral"), main="Types of Tumors", xlab="Types", ylab="Data", legend.text=TRUE)

# Plotting histogram of Mitoses variable
hist(tumor$Mitoses, col="skyblue", main="Mitoses Values", xlab="Mitoses")

# Defining ZeroR model
ZeroR <- function(X, targetId) {
  if (is.character(X[, targetId]) | is.factor(X[, targetId])) {
    u.x <- unique(X[, targetId])
    u.x.temp <- c()
    for (i in u.x) {
      u.x.temp <- c(u.x.temp, sum(X[, targetId] == i))
    }
    names(u.x.temp) <- u.x
    return(c(max(u.x.temp), names(u.x.temp)[which.max(u.x.temp)]))
  }
  return(NULL)
}

# Applying ZeroR model to the dataset
result <- ZeroR(tumor, "Class")
print(result)

# Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))
