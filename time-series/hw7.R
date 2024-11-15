library(astsa)
plot(chicken)
acf2(diff(chicken))
sarima(chicken, 2, 1, 0)
sarima(chicken, 2, 1, 0, 1, 0, 0, 12, no.constant=TRUE)

sarima.for(chicken, 12, 2,1, 0, 1, 0, 0, 12)