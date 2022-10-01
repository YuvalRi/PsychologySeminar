
df <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\analysis\\clicks counting\\r1.csv")
y <- df$partner.s.clicks.with.subject
x <- df$ï..subject.s.clicks
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     xlab = "# clicks each participant felt", 
     ylab = "# clicks with each participant ", 
     pch = 19)
abline(lm(y~x), col="dodgerblue2",
       fill = c("dodgerblue2"), bty = "n"
)
