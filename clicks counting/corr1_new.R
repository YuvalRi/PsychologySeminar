df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//correlationsToJASP.csv")

# C, G
y <- df[, 3]
x <- df[, 7]
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     xlab = "# G", 
     ylab = "# C", 
     pch = 19)
abline(lm(y~x), col="dodgerblue2",
       fill = c("dodgerblue2"), bty = "n"
)



df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//correlationsToJASP.csv")

# A, B
y <- df[, 1]
x <- df[, 2]
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     xlab = "# B", 
     ylab = "# A", 
     pch = 19)
abline(lm(y~x), col="dodgerblue2",
       fill = c("dodgerblue2"), bty = "n"
)


df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//correlationsToJASP.csv")
# D, E
y <- df[, 4]
x <- df[, 5]
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     xlab = "# E", 
     ylab = "# D", 
     pch = 19)
abline(lm(y~x), col="dodgerblue2",
       fill = c("dodgerblue2"), bty = "n"
)


