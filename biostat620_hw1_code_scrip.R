# R setup and Load the data
library(lubridate)
library(scales)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(GGally)
library(circular)
st = read.csv("./Biostat620ScreenTimeZihaoHan.csv")
head(st)
# Problem 1 c
# First Convert the Screen Time and Social Screen Time to the minutes (numeric)
hm_to_min = function(text){
  split = strsplit(text,"h|m")
  hours = as.numeric(split[[1]][1])
  minutes = as.numeric(split[[1]][2])
  convert_minutes = hours * 60 + minutes
  return(convert_minutes)
}
st$Total.ST.min = sapply(st$Total.ST,hm_to_min)
st$Social.ST.min = sapply(st$Social.ST,hm_to_min)
head(st)

# Compute these two adding variables
st$proportionOfSocial = st$Social.ST.min / st$Total.ST.min
st$duration = st$Total.ST.min / st$Pickups
head(st[c("Date","proportionOfSocial", "duration")])

#Problem 2A
# modify the date format fo dataset
st$Date <- as.Date(st$Date, format = "%m/%d/%y")
# Define a demographic variable indicate whether today I have course
st$CouseorNot <- ifelse(st$CourseToday >= 1, 1, 0)
st$CouseorNot <- as.factor(st$CouseorNot)
# Plot the Time Series of Total Screen Time
st.plot = ggplot(st, aes(x = Date, y = Total.ST.min, color = CouseorNot, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Total Screen Time in Minutes") +
  ylim((min(st$Total.ST.min) %/% 100 - 1) * 100, (max(st$Total.ST.min) %/% 100 + 1) * 100) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())

social.plot = ggplot(st, aes(x = Date, y = Social.ST.min, color = CouseorNot, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Social APP Screen Time in Minutes") +
  ylim(max((min(st$Social.ST.min) %/% 100 - 1) * 100,0), (max(st$Social.ST.min) %/% 100 + 1) * 100) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())
grid.arrange(
  st.plot,
  social.plot,
  nrow = 2
)

pickups.plot = ggplot(st, aes(x = Date, y = Pickups, color = CouseorNot, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Numbers of Pickups") +
  ylim(min(st$Pickups), max(st$Pickups)) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())
proportion.plot = ggplot(st, aes(x = Date, y = proportionOfSocial, 
                                 color = CouseorNot, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Proportion of Social Screen Time") +
  ylim(0,max(st$proportionOfSocial)+0.1) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())

grid.arrange(
  pickups.plot,
  proportion.plot,
  nrow = 2
)
duration.plot = ggplot(st, aes(x = Date, y = duration, 
                               color = CouseorNot, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point() +
  xlab("Date") +
  ylab("Screen Time Duration per used") +
  ylim(max(min(st$duration)-5,0),max(st$duration)+5) +
  scale_color_manual(labels = c("No Course Today","Have Course Today"), 
                     values = c("black", "red")) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank())
duration.plot

# Problem 2B
# options(tcltk.qtk=FALSE)
corr.plot = ggpairs(st, columns = c("Total.ST.min", "Social.ST.min", "Pickups",
                                    "proportionOfSocial", "duration"),
                    columnLabels = c("Total ST", "Social ST", "Pickups",
                                     "Proportion of Social ST", "Duration")) +
  theme_bw()
corr.plot

# Problem 2C
OCT = function(x,title){
  probabilities = ecdf(x)
  range = seq(0, max(x)*1.1, length.out = 100)
  cdf_plot <- probabilities(range)
  plot = ggplot() +
    geom_step(aes(x = range, y = 1- cdf_plot), color = "blue") +
    labs(title = title,
         x = "Magnitude c",
         y = "P(X>c)")
  return(plot)
}

grid.arrange(
  OCT(st$Total.ST.min, "Total Screen Time"),
  OCT(st$Social.ST.min, "Social Screen Time"),
  ncol = 2
)
grid.arrange(
  OCT(st$Pickups, "Pickups"),
  OCT(st$proportionOfSocial, "Proportion of Social ST"),
  ncol = 2
)
OCT(st$duration, "Duration")

# Problem 2D
par(mfrow = c(1, 2))
# Apply auto-correlation function (ACF)
acf(st$Total.ST.min)
acf(st$Social.ST.min)
acf(st$Pickups)
acf(st$proportionOfSocial)
acf(st$duration)
# Using plot=FALSE to output the value
acf.st = acf(st$Total.ST.min,plot=FALSE)
acf.social = acf(st$Social.ST.min,plot=FALSE)
acf.pickups = acf(st$Pickups,plot=FALSE)
acf.prop = acf(st$proportionOfSocial,plot=FALSE)
acf.duration = acf(st$duration,plot=FALSE)
acf.st
acf.social
acf.pickups
acf.prop
acf.duration

#Problem 3A
Pickup1st_to_angle = function(text){
  split = strsplit(text,":")
  hours = as.numeric(split[[1]][1])
  minutes = as.numeric(split[[1]][2])
  convert_minutes = hours * 60 + minutes
  return(convert_minutes/1440*360)
}
st$Pickup.1st.angle = sapply(st$Pickup.1st,Pickup1st_to_angle)
head(st[c("Date","Pickup.1st.angle")])

# Problem 3B
pickups1st.cir = circular(st$Pickup.1st.angle,units = "degrees", template = "clock24")
plot(pickups1st.cir,col="blue")

# Problem 3C
par(mfrow = c(1, 2))

# using bin = 10, corresponds to the interval of 40 mins
plot(pickups1st.cir, stack= TRUE, bins = 10, col = "blue")
title(main = "bin = 10")

# using bin = 30, corresponds to the interval of 120 mins
plot(pickups1st.cir, stack= TRUE, bins = 30, col = "blue")
title(main = "bin = 30")

# using bin = 60, corresponds to the interval of 240 mins
plot(pickups1st.cir, stack= TRUE, bins = 60, col = "blue")
title(main = "bin = 60")

# using bin = 90, corresponds to the interval of 360 mins
plot(pickups1st.cir, stack= TRUE, bins = 90, col = "blue")
title(main = "bin = 90")

par(mfrow = c(1, 1))

# Problem 4 B
est.lambda = glm(Pickups ~ offset(log(Total.ST.min/60)),
                 family = poisson,data = st)
summary(est.lambda)
cat("The estimate is",exp(coef(est.lambda)["(Intercept)"]),"\n")

# Problem 4 C
# define the dummy variable X_t
st$wday = wday(st$Date)
st$Xt = ifelse(st$wday >= 2 & st$wday <= 6, 1, 0)
# define the dummy variable Z_t
st$Zt = ifelse(st$Date>=as.Date("2024-01-10"),1,0)
# fit the model
est.lambda.dummy = glm(Pickups ~ Xt+Zt+
                         offset(log(Total.ST.min/60)),
                       family = poisson,
                       data=st)
summary(est.lambda.dummy)

# Problem 5A

# apply the function mle.vonmises, and input the angle degree of first pickup times

est.mu.lambda = mle.vonmises(st$Pickup.1st.angle)
est.mu.lambda

# Problem 5 B
# convert the 8:30 AM to circular data
treshold = circular(2*pi*(8 + 30/60)/24)
probability = pvonmises(treshold,mu = est.mu.lambda$mu,
                        kappa = est.mu.lambda$kappa,
                        tol = 1e-4)
cat("The probability of first pickup time later than 8:30 AM with 1e-4 tolerence, and the parameters are estimated by (a), is",1- probability,"\n")
