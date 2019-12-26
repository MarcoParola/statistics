uber = read.csv("tabella.csv");
uber.ts = ts(uber$num, frequency = 168);
plot(uber.ts);
plot(acf(uber$num, lag=200));


# modello additivo
uber.ts = ts(uber$num, frequency = 168);
uber.ts.da = decompose(uber.ts, type="additive");
plot(uber.ts.da);


# modello additivo
uber.ts = ts(uber$num, frequency = 168);
uber.ts.da = decompose(uber.ts, type="additive");
plot(uber.ts.da);

# QQPLOT
layout(t(1:2))
qqnorm(uber.ts.da$random);
qqline(uber.ts.da$random, col="red");

uber.ts.resid = uber.ts.da$random[85:2123];
 hist(uber.ts.da$random, 20, freq=F)
lines(density(uber.ts.resid), col="red")
lines(sort(uber.ts.resid), dnorm(sort(uber.ts.resid), mean(uber.ts.resid), sd(uber.ts.resid)))



#PREDICT
uber.hw = HoltWinters(uber.ts)
plot(uber.hw, predict(uber.hw, 168))
plot(uber.hw, predict(uber.hw, 336))


uber.hw.r = residuals(uber.hw);
layout(t(1:2))
qqnorm(uber.hw.r);
qqline(uber.hw.r, col="red");

 hist(uber.hw.r, 35, freq=F)
lines(density(uber.hw.r), col="red")
lines(sort(uber.hw.r), dnorm(sort(uber.hw.r), mean(uber.hw.r), sd(uber.hw.r)))


#VARIANZA SPIEGATA
1 - var(uber.hw.r)/var(window(uber.ts, start= c(13,1), end=c(13,168)))

wind = window(uber.ts, end=c(12,168))
train = window(uber.ts, end=c(12,168))
test = window(uber.ts, start=c(13,1), end=c(13,168))
train.hw = HoltWinters(train)
test.pred = predict(train.hw, 168)
ts.plot(test, test.pred, col=c("black", "red"))
mean((test - test.pred)^2)/var(test)

