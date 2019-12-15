house = read.csv("tabella.csv");
house$date <- NULL; house$lat <- NULL; house$long <- NULL; house$zipcode <- NULL; house$sqft_basement <- NULL; 
round(cor(house), 2);


for(i in 1:length(house[,1])){
    if(house$price[i] > 4000000){
        house$price[i] = house$price[i] / 1000;
    }
}

a = c(); 
for(i in 1:length(house[,1])){
    if((house$sqft_living[i] > house$sqft_lot[i]) | house$bedrooms > 30){
        a = c(a, i);
    }
}
house = house[-a,];


house.lm = lm(price~.,data=house);
summary(house.lm);



house.lm=lm(price~bedrooms+sqft_living+view+grade+yr_built,data=house);
summary(house.lm);



house.lm.r = residuals(house.lm);
plot(house.lm.r, pch=".");
    


qqnorm(house.lm.r, pch="*");
qqline(house.lm.r, pch=".", col="red");



hist(house.lm.r,40,freq=F, ylim=c(0, 3e-06), xlim=c(-2e+06, +2e+06));
lines(density(house.lm.r),col="red");
lines(sort(house.lm.r),dnorm(sort(house.lm.r),mean(house.lm.r),sd(house.lm.r)));



house.lm_log = lm(log(price)~bedrooms+sqft_living+view+grade+yr_built,data=house);
summary(house.lm_log);



house.lm_log.r = residuals(house.lm_log);
plot(house.lm_log.r, pch=".", ylim=c(-3, 3));
    


qqnorm(house.lm_log.r, pch="*", ylim=c(-7,2));
qqline(house.lm_log.r, pch=".", col="red");



hist(house.lm_log.r,40,freq=F, xlim=c(-2,2));
lines(density(house.lm_log.r),col="red");
lines(sort(house.lm_log.r),dnorm(sort(house.lm_log.r),mean(house.lm_log.r),sd(house.lm_log.r)));


lhouse = log(house);
is.na(lhouse) <- sapply(lhouse, is.infinite)
lhouse[is.na(lhouse)] <- 0

set.seed(25)

u = sample( length(house[,1]), round(length(house[,1]) * 0.2));
houseTraining = house[-u,];
houseTest = house[u,];
house.lm = lm(price~bedrooms + sqft_living + view+grade + yr_built, data = houseTraining);
house.lm.p = predict(house.lm, houseTest);

lhouseTraining = lhouse[-u,];
lhouseTest = lhouse[u,]
lhouse.lm_log = lm(price~bedrooms + sqft_living + view+grade + yr_built, data = lhouseTraining);   
lhouse.lm_log.p = predict(lhouse.lm_log, lhouseTest);

sqrt(mean((house.lm.p-houseTest$price)^2)/mean(houseTest$price)^2)
sqrt(mean((lhouse.lm_log.p-lhouseTest$price)^2)/mean(lhouseTest$price)^2)
    

    
