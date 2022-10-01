library(ggplot2)

df <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\analysis\\clicks counting\\r2.csv")
y <- df$number.of.all.mutual.clicks.person.i.has.with.others...number.of.clicks.a.person.i.has.with.others
x <- df$ï..number.of.clicks.a.person.i.has.with.others...number.of.all.potential.clicks
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     ylab = "Number of all mutual clicks for each participant divided by all clicks a participant reported", 
     xlab = "All clicks a participant reported divided by all potential clicks",
     pch = 19)
abline(lm(y~x), col="dodgerblue2")
legend(0, 0.95, legend=c(" R = -0.163, P = 0.256"), 
       fill = c("dodgerblue2"), bty = "n"
)
