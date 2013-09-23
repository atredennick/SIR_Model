##Simple SIR model of disease transmission##
##this example is a 'zombie apocalypse' for an undergrad example##

sir <- function(So, Io, Ro,
                beta, r, time){
  S <- numeric(time)
  I <- numeric(time)
  R <- numeric(time)
  S[1] <- So
  I[1] <- Io
  R[1] <- Ro
  for(t in 2:time){
    dSdt <- -beta*S[t-1]*I[t-1]
    dIdt <- beta*S[t-1]*I[t-1] - r*I[t-1]
    dRdt <- r*I[t-1]
    
    S[t] <- S[t-1] + dSdt
    I[t] <- I[t-1] + dIdt
    R[t] <- R[t-1] + dRdt
  }
  outs <- cbind(S,I,R)
  return(outs)
}


So <- 499
Io <- 1
Ro <- 0
transmission <- 0.001
infectiousness <- 1 #zombies have total infectiousness, if S contacts I p=100%
beta <- transmission*infectiousness
r <- 0 #no recovery from zombie state

time <- 100

model <- sir(So, Io, Ro,
             beta, r, time)

mod.df <- as.data.frame(model)
mod.df$Time <- seq(1,time,1)
colnames(mod.df) <- c("Live", "Zombie", "R", "Time")
mod.df <- subset(mod.df, select=-c(R))
require(reshape2)
df.m <- melt (mod.df, id.vars="Time")

require(ggplot2)
require(ggthemes)

ggplot(data=df.m, aes(x=Time, y=(value/500*100), linetype=variable)) +
  geom_line(size=1.5) +
  xlab("Time Since Initial Zombie (years)") +
  ylab("Percent of Population (%)") +
  theme_few() +
  theme(legend.position = c(0.75,0.75))







