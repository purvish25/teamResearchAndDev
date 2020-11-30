rainfall2 = read.csv("C:/Users/ranjandi/Desktop/Practical2/rainfall.csv")
rainfall2$tempDate = paste("01",rainfall2$Month,rainfall2$Year,sep="-")
rainfall2$trueDate = as.Date(rainfall2$tempDate,format("%d-%b-%Y"))
recent = head(rainfall2, n=150)
plot(rainfall2$Year,recent$Rainfallmm, main = "Visualization Rainfall", xlab="Year", ylab="Rainfall in MM",type="h", col  = rainbow(25))

recent$trueDate

