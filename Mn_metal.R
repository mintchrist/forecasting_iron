metal = read.csv("Manganese.csv")
tsmetal = ts(metal$Mn44,start=c(2006,6), freq = 12)
tsmetal
plot(tsmetal, xlab="Time", ylab="Price of Mn", main="Monthly Mn Price")
autoplot(tsmetal) +
  ggtitle("Monthly Mn Price") +
  xlab("Time") +
  ylab("Price of Mn")

ggseasonplot(tsmetal, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

ggseasonplot(tsmetal, polar=TRUE) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

ggsubseriesplot(tsmetal) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

add.decomp = decompose(tsmetal, "additive")
autoplot(add.decomp)
