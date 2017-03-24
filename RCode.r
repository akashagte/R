SP100 <- read.csv("ElectricityRates.csv",header=T);
attach(SP100);

#Histogram plots of ElectricityPrices by Regions
hist(SP100$Residential_2004,main="Residential Electricity Prices in 2004",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="blue",xlim=c(6,20));
		
hist(SP100$Residential_2005,main="Residential Electricity Prices in 2005",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="blue",xlim=c(6,22));

hist(SP100$Commercial_2004,main="Commercial Electricity Prices in 2004",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="green",xlim=c(5,20));

hist(SP100$Commercial_2005,main="Commercial Electricity Prices in 2005",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="green",xlim=c(5,20));

hist(SP100$Industrial_2004,main="Industrial Electricity Prices in 2004",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="tomato",xlim=c(3,16));

hist(SP100$Industrial_2005,main="Industrial Electricity Prices in 2005",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="tomato",xlim=c(3,16));

hist(SP100$AllOtherSectors_2004,main="All Other Sectors Electricity Prices in 2004",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="grey",xlim=c(4,20));

hist(SP100$AllOtherSectors_2005,main="All Other Sectors Electricity Prices in 2005",
		xlab="Prices in Cents per kilowatthour",ylab="Number of States",border="black",col="grey",xlim=c(4,20));


#Box Plots of Electricity Prices by Sector
boxplot(SP100$Residential_2004,SP100$Commercial_2004,SP100$Industrial_2004,SP100$AllOtherSectors_2004,
		names=c("Residential","Commercial","Industrial","AllOtherSectors"),col=c("blue","green","tomato","grey"),
		ylab="Electricity Prices",main="Boxplots for All 4 types Sectors in 2004");
		
boxplot(SP100$Residential_2005,SP100$Commercial_2005,SP100$Industrial_2005,SP100$AllOtherSectors_2005,
		names=c("Residential","Commercial","Industrial","AllOtherSectors"),col=c("blue","green","tomato","grey"),
		ylab="Electricity Prices",main="Boxplots for All 4 types Sectors in 2005");
		

#Sector-wise Summary 
summary(SP100$Residential_2004);
summary(SP100$Commercial_2004);
summary(SP100$Industrial_2004);
summary(SP100$AllOtherSectors_2004);
summary(SP100$Residential_2005);
summary(SP100$Commercial_2005);
summary(SP100$Industrial_2005);
summary(SP100$AllOtherSectors_2005);


#Box Plot of Electricity Prices by Region
boxplot(SP200$NewEngland,SP200$MiddleAtlantic,SP200$EastNorthCentral,SP200$WestNorthCentral,
		SP200$SouthAtlantic,SP200$EastSouthCentral,SP200$WestSouthCentral,
		SP200$Mountain,SP200$PacificContiguous,SP200$PacificNoncontiguous,
		names=c("NewEngland","M_Atlantic","EN_Central","WN_Central","S_Atlantic","ES_Central","WS_Central","Mountain","Pacific_C","Pacific_NC"),
		col=colorRampPalette(c("blue", "red"))(10),ylab="ElectricityPrices",
		main="Boxplots for Electricity Prices in US Census Divisions (Years 2004 & 2005)");
	

#Region-wise Summary	
summary(SP200$NewEngland);
summary(SP200$MiddleAtlantic);
summary(SP200$EastNorthCentral);
summary(SP200$WestNorthCentral);
summary(SP200$SouthAtlantic);
summary(SP200$EastSouthCentral);
summary(SP200$WestSouthCentral);
summary(SP200$Mountain);
summary(SP200$PacificContiguous);
summary(SP200$PacificNoncontiguous);


#Correlation between Sectors
line1 <- lm(SP100$Residential_2004~SP100$Commercial_2004);
plot(SP100$Commercial_2004,SP100$Residential_2004, xlab="Commercial Sector prices",ylab="Residential sector prices",main="Residential - Commercial,2004" );
abline(line1);
cor(SP100$Commercial_2004,SP100$Residential_2004);

line2<- lm(SP100$Residential_2005~SP100$Commercial_2005);
plot(SP100$Commercial_2005,SP100$Residential_2005, xlab="Commercial Sector prices",ylab="Residential sector prices",main="Residential - Commercial,2005" );
abline(line2);
cor(SP100$Commercial_2005,SP100$Residential_2005);

line3<- lm(SP100$Residential_2004~SP100$Industrial_2004);
plot(SP100$Industrial_2004,SP100$Residential_2004, xlab="Industrial Sector prices",ylab="Residential sector prices",main="Residential - Industrial,2004" );
abline(line3);
cor(SP100$Industrial_2004,SP100$Residential_2004);

line4<- lm(SP100$Residential_2005~SP100$Industrial_2005);
plot(SP100$Industrial_2005,SP100$Residential_2005, xlab="Industrial Sector prices",ylab="Residential sector prices",main="Residential -Industrial,2005" );
abline(line4);
cor(SP100$Industrial_2005,SP100$Residential_2005);

line5<- lm(SP100$Commercial_2004~SP100$Industrial_2004);
plot(SP100$Industrial_2004,SP100$Commercial_2004, xlab="Industrial Sector prices",ylab="Commercial sector prices",main="Commercial - Industrial,2004" );
abline(line5);
cor(SP100$Industrial_2004,SP100$Commercial_2004);

line6<- lm(SP100$Commercial_2005~SP100$Industrial_2005);
plot(SP100$Industrial_2005,SP100$SP100$Commercial_2005, xlab="IndustrialSector prices",ylab="Commercial sector prices",main="Commercial - Industrial, 2005" );
abline(line6);
cor(SP100$Industrial_2005,SP100$Commercial_2005);


#Quantile Plots for Region-wise Residential sector prices
qqnorm(SP101$Residential_2005, main="Normal Quantile Plot of Region1");qqline(SP101$Residential_2005);
qqnorm(SP104$Residential_2005, main="Normal Quantile Plot of Region4");qqline(SP104$Residential_2005);
qqnorm(SP105$Residential_2005, main="Normal Quantile Plot of Region5");qqline(SP105$Residential_2005);
qqnorm(SP108$Residential_2005, main="Normal Quantile Plot of Region8");qqline(SP108$Residential_2005);


#One-way ANOVA Tests
fit1 <- aov(SP100$Residential_2005~SP100$Region); summary(fit1);
fit2 <- aov(SP100$Commercial_2005~SP100$Region); summary(fit2);
fit3 <- aov(SP100$Industrial_2005~SP100$Region); summary(fit3);
fit4 <- aov(SP100$AllOtherSectors_2005~SP100$Region); summary(fit4);


detach(SP100);