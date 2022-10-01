library(ggplot2)

df <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\analysis\\clicks counting\\r1.csv")
y <- df$partner.s.clicks.with.subject
x <- df$ï..subject.s.clicks
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     xlab = "Number of clicks a participant felt with others", 
     ylab = "Number of clicks others felt with a participant", 
     pch = 19)
abline(lm(y~x), col="dodgerblue2")
legend(4.5, 9.2, legend=c(" R = 0.3, P = 0.04"), 
       fill = c("dodgerblue2"), bty = "n"
)
