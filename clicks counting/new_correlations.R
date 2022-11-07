## Pearson correlation and Pvalue

# A
corrA_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//corrA.csv")
colnames(corrA_df) <- c("A", "B")

x_A <- corrA_df$A
y_A <- corrA_df$B

cor.test(corrA_df$A, y_A)


# B

corrB_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//corrB.csv")
colnames(corrB_df) <- c("A", "C")


x_B <- corrB_df$A
y_B <- corrB_df$C
cor.test(x_B, y_B)


corrC_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//analysis//clicks counting//corrC.csv")
colnames(corrC_df) <- c("B", "C")


x_C <- corrC_df$B
y_C <- corrC_df$C
cor.test(x_C, y_C)


## Plots

par(mfrow = c(1,3))

plot(x_A, y_A, 
     xlab = "# clicks each participant felt", 
     ylab = "# clicks with each participant ", 
     pch = 19)
abline(lm(y_A ~ x_A), col="dodgerblue2",
       fill = c("dodgerblue2"), bty = "n"
)


plot(x_B, y_B, 
     xlab = "# clicks each participant felt", 
     ylab = "# clicks with each participant ", 
     pch = 19)
abline(lm(y_B ~ x_B), col="dodgerblue2",
       bty = "n"
)

plot(x_C, y_C, 
     xlab = "# clicks each participant felt", 
     ylab = "# clicks with each participant ", 
     pch = 19)
abline(lm(y_C ~ x_C), col="dodgerblue2",
       bty = "n"
)



