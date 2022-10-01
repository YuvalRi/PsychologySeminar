
df <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\analysis\\clicks counting\\r2.csv")
y <- df$number.of.all.mutual.clicks.person.i.has.with.others...number.of.clicks.a.person.i.has.with.others
x <- df$ï..number.of.clicks.a.person.i.has.with.others...number.of.all.potential.clicks
cor(x, y, method = c("pearson"))
cor.test(x, y)


plot(x,y, 
     ylab = "Normalized # mutual clicks for each participant", 
     xlab = "Normalized # clicks each participant felt",
     pch = 19)
abline(lm(y~x), col="dodgerblue2", 
       fill = c("dodgerblue2"), 
       bty = "n"
)
