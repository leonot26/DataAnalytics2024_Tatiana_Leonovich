library(ggplot2)
library(tidyverse)

# EXAMPLE 1
# importing data set
epi_2024 <- read.csv("C:/Users/Tat/Desktop/Data Analytics 2024/Lab 2/epi2024results06022024.csv")

# quantile-quantile plots
help("qqnorm")

attach(epi_2024)
# going to use MKP.new and MKP.old and MHP.old




# removing the Na in MKP.new
# VARIABLE 1
MKP_na <- is.na(MKP.new)

# add comma after the removed NA to call all columns in the data set
epi_2024<- epi_2024[!MKP_na , ]
epi_2024


summary(MKP.new)
# q-q plot with best fit line MKP.new
help(qqline)
qqnorm(MKP.new) ; qqline(MKP.new)
# setting the q-q plot bounds
MKP_seq <- seq(1.,101.,1.)
help(seq)

qqplot(qnorm(ppoints(200)), MKP_seq) ; qqline(MKP_seq)

qqplot(qnorm(ppoints(1000)), MKP_seq) ; qqline(MKP_seq)

qqplot(qnorm(ppoints(200)),MKP.new) ; qqline(MKP.new)
qqplot(qnorm(ppoints(1000)),MKP.new) ; qqline(MKP.new)

#cumulative density MKP.new
plot(ecdf(MKP.new), do.points=FALSE)
# in rnorm(points, mean, sd)
help(sd)
# calculating sd of MKP.new
sd(MKP.new, na.rm= TRUE)
# sd is 33.616
plot(ecdf(rnorm(1000, 38.9, 33.6)), do.points=FALSE) 
lines(ecdf(MKP.new))





# MKP_old
# VARIABLE 2
MKP_old_na <- is.na(MKP.old)

# add comma after the removed NA to call all columns in the data set
epi_2024<- epi_2024[!MKP_old_na , ]
epi_2024

summary(MKP.old)
qqnorm(MKP.old) ; qqline(MKP.old)
MKP_old_seq<- seq(1,101,1)

# qq plots
qqplot(qnorm(ppoints(200)), MKP_old_seq) ; qqline(MKP_old_seq)

qqplot(qnorm(ppoints(1000)), MKP_old_seq) ; qqline(MKP_old_seq)

qqplot(qnorm(ppoints(200)),MKP.old) ; qqline(MKP.old)
qqplot(qnorm(ppoints(1000)),MKP.old) ; qqline(MKP.old)

#cumulative density MKP.old
plot(ecdf(MKP.old), do.points=FALSE)
# in rnorm(points, mean, sd)
# calculating sd of MKP.old
sd(MKP.old, na.rm= TRUE)
# sd is 32.1
plot(ecdf(rnorm(1000, 35.43, 32.1)), do.points=FALSE) 
lines(ecdf(MKP.new))

# qq plots
qqplot(qnorm(ppoints(200)), MKP_old_seq) ; qqline(MKP_old_seq)

qqplot(qnorm(ppoints(1000)), MKP_old_seq) ; qqline(MKP_old_seq)

qqplot(qnorm(ppoints(200)),MKP.old) ; qqline(MKP.old)
qqplot(qnorm(ppoints(1000)),MKP.old) ; qqline(MKP.old)

#cumulative density MKP.old
plot(ecdf(MKP.old), do.points=FALSE)
# in rnorm(points, mean, sd)
# calculating sd of MKP.old
sd(MKP.old, na.rm= TRUE)
# sd is 32.1
plot(ecdf(rnorm(1000, 35.43, 32.1)), do.points=FALSE) 
lines(ecdf(MKP.new))






#MHP.old
#VARIABLE 3

MHP_old_na <- is.na(MHP.old)
epi_2024 <- epi_2024[!MHP_old_na, ]
summary(MHP.old)
qqnorm(MHP.old) ; qqline(MHP.old)
MHP_old_seq<- seq(1,101,1)


# qq plots
qqplot(qnorm(ppoints(200)), MHP_old_seq) ; qqline(MHP_old_seq)

qqplot(qnorm(ppoints(1000)), MHP_old_seq) ; qqline(MHP_old_seq)

qqplot(qnorm(ppoints(200)),MHP.old) ; qqline(MHP.old)
qqplot(qnorm(ppoints(1000)),MHP.old) ; qqline(MHP.old)

#cumulative density MKP.old
plot(ecdf(MHP.old), do.points=FALSE)
# in rnorm(points, mean, sd)
# calculating sd of MKP.old
sd(MHP.old, na.rm= TRUE)
# sd is 21.78 mean = 22.74
plot(ecdf(rnorm(1000, 21.78, 22.74)), do.points=FALSE) 
lines(ecdf(MHP.new))

# qq plots
qqplot(qnorm(ppoints(200)), MHP_old_seq) ; qqline(MHP_old_seq)

qqplot(qnorm(ppoints(1000)), MHP_old_seq) ; qqline(MHP_old_seq)

qqplot(qnorm(ppoints(200)),MHP.old) ; qqline(MHP.old)
qqplot(qnorm(ppoints(1000)),MHP.old) ; qqline(MHP.old)


# comparing the distributions
boxplot(MKP.old, MKP.new, MHP.old, names=c("MKP.old","MKP.new", "MHP.old"))
plot(ecdf(MKP.old), do.points=FALSE, main="MKP.old vs. MKP.new vs. MHP.old ECDF")
lines(ecdf(MKP.new))
lines(ecdf(MHP.old))












# EXERCISE 2
#linear models, comparing population to MKP.new and MKP.old and MHP.old
population <- read.csv("C:/Users/Tat/Desktop/Data Analytics 2024/Lab 2/countries_populations_2023.csv")
population
# drop countries not in epi_2024
population <- population[-which(!population$Country %in% epi_2024$country),]
population

# sort population by country
population <- population[order(population$Country),]
# drop countries not in populations
mkp.results.sub <- epi_2024[-which(!epi_2024$country %in% population$Country),]
# sort epi results by country
mkp.results.sub <- mkp.results.sub[order(epi_2024$country),]
# only keep necessary columns
mkp.results.sub <- mkp.results.sub[,c("country","MKP.old","MKP.new", "MHP.old")]

# convert population to numeric # the example was missing removing the na
mkp.results.sub <- na.omit(mkp.results.sub)
mkp.results.sub$population <- as.numeric(population$Population)

# compute population log base 10
mkp.results.sub$population_log <- log10(mkp.results.sub$population)





# LINEAR MODELS
#MKP.new
lin.mod.mkpnew <- lm(MKP.new~population_log,mkp.results.sub)
population_log <- mkp.results.sub$population_log
# example was wrong you need to specifically call items from their own data set
plot(mkp.results.sub$MKP.new~population_log)

abline(lin.mod.mkpnew)
summary(lin.mod.mkpnew)
plot(lin.mod.mkpnew)

# ggplot for mkp.new

ggplot(mkp.results.sub, aes(x = population_log, y = MKP.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.mkpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



#MKP.old
lin.mod.mkpold <- lm(MKP.old~population_log,mkp.results.sub)
population_log <- mkp.results.sub$population_log
# example was wrong you need to specifically call items from their own data set
plot(mkp.results.sub$MKP.old~population_log)

abline(lin.mod.mkpold)
summary(lin.mod.mkpold)
plot(lin.mod.mkpold)

# ggplot for MKP.old
ggplot(mkp.results.sub, aes(x = population_log, y = MKP.old)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.mkpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


#MHP.old
lin.mod.mhpold <- lm(MKP.old~population_log,mkp.results.sub)
population_log <- mkp.results.sub$population_log
# example was wrong you need to specifically call items from their own data set
plot(mkp.results.sub$MHP.old~population_log)

abline(lin.mod.mhpold)
summary(lin.mod.mhpold)
plot(lin.mod.mhpold)

# ggplot for MHP.old

ggplot(mkp.results.sub, aes(x = population_log, y = MHP.old)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.mhpold, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')












