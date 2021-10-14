#r script for plot digitizer studies


Bauch_2016_data_from_plotdigitizer <- read.csv("Bauch_2016_data_from_plotdigitzer.csv")

Bauch_2016_lm <- lm(residual.telomere.length ~ residual.corticosterone, data = Bauch_2016_data_from_plotdigitizer)

summary(Bauch_2016_lm)$r.squared

#r.squared = 4.65 e -06


