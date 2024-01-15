library(amt)
library(sf)
library(ggplot2)
library(dplyr)
library(moveHMM)
library(cowplot)

rawdat <- read.csv("tortoise.csv")
head(rawdat)

rawdat$id <- "tortoise1"
rawdat$time <- seq(1:nrow(rawdat)) ##introduce time in the data
rawdat$day <- ceiling(rawdat$time/(24*60*60))  ### assuming this is a 1Hz regular data

day_trck <-ggplot(rawdat, aes(x=X, y=Y, col = as.factor(day)))+
  geom_point()+
  theme_bw()+ labs(col = "Day")

#### Create track from the data
track <- rawdat %>% 
  mk_track( .x=X, .y=Y, .t=time, id= id) 

### Calculate the step-lengths and turn angles to understadn the movement
track$sl<-  step_lengths(track)
track$ta<-  direction_rel(track)
head(track)

trck_sl <-ggplot(track, aes(x=x_, y=y_, col = sl))+
  geom_point()+
  theme_bw() +labs(col = "Step\nLength")

plot_grid(day_trck, trck_sl)

##########################################################################################
### the track is too fine scale, I'll resample to see if the patttern holds or not

rawdat_samp <- rawdat[seq(1, nrow(rawdat), by=20),]

#### Create track from the data
track_samp <- rawdat_samp %>% 
  mk_track( .x=X, .y=Y, .t=time, id= id) 

### Calculate the step-lengths and turn angles to understadn the movement
track_samp$sl<-  step_lengths(track_samp)
track_samp$ta<-  direction_rel(track_samp)
head(track_samp)

trck_sl_samp<- ggplot(track_samp, aes(x=x_, y=y_, col = sl))+
  geom_point()+
  theme_bw() +labs(col = "Step\nLength")

track_turn <- track_samp %>% filter(abs(ta)>1) ### Points of interest, recorded sharp turns 
track_fast <- track_samp %>% filter(sl > 0.05) ### Points of interest, recorded fast movements

turn_samp <- ggplot()+
  geom_path(data= rawdat, aes(x=X, y=Y, col = as.factor(day)), lwd=1.5)+
  geom_point(data= track_turn, aes(x=x_, y=y_), col = "brown", alpha =0.5)+
  geom_point(data= track_fast, aes(x=x_, y=y_), col = "gold", alpha =0.5, size=0.25)+
  theme_bw() +labs(col = "Day")

turn_all <- ggplot()+
     geom_path(data= rawdat, aes(x=X, y=Y, col = as.factor(day)), lwd=1.5)+
     geom_point(data= track_turn, aes(x=x_, y=y_), col = "black", alpha =0.5,size=0.25)+
     theme_bw() +labs(col = "Day")

plot_grid(turn_all, turn_samp)
ggsave("track_plot.jpeg", width = 9, height = 6, units = "in", dpi = 300)

##########################################################################################
######### Behaviour exploration usign hidden markov model
hmmdat<- rawdat_samp
colnames(hmmdat)[1:4] <- c("x", "y", "ID", "time")

hmm_data <- moveHMM::prepData(hmmdat, type =  "UTM",
                              coordNames = c("x", "y"))
head(hmm_data)
summary(hmm_data$step)

hist(hmm_data$step, breaks = 40)
hist(hmm_data$angle, breaks = seq(-pi, pi, length=40))

step0 <- c(0.01, 0.12,0.03,0.05, 0.01, 0.01) ### Initial step-length values for the two behaviours 
angle0 <- c(0,0,0.1,1.5).   ### Initial turn-angle values for the two behaviours 

hmm_mod <- moveHMM::fitHMM(data = hmm_data, nbStates = 2, stepPar0 = step0, anglePar0 = angle0,
                           formula = ~ 1, stepDist = "gamma", angleDist = "vm")
hmm_mod

plot(hmm_mod, plotCI = TRUE, breaks = 40, col = c("brown", "green"))
res <- pseudoRes(hmm_mod)

states <- viterbi(hmm_mod)
xtabs(~states)
