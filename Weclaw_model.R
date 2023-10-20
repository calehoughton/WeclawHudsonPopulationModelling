rm(list = ls())
graphics.off()
quartz()

library(ggplot2)


#############
##Original model without wolves
#############

##set up parameter values
sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0
mW <- 0
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.0 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.0 <- within(pop.df.0,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0
})


for(t in 2:nsteps){
  pop.df.0 <- within(pop.df.0,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05))
    
    W[t] <- max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW*runif(n=1,min=0.95,max=1.05))
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.0)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)




#############
##Original model with predation
#############
# sP <- 0.5
# Kp <- 240
# lP <- 2682.75/1000
# hp <- 300
# sL <- 0.07
# Kl <- 870
# lL <- 2682.75/1000
# hl <- 400
# sH <- 0.5
# Kh <- 970
# lH <- 15841/5000
# hh <- 1000#533
# fC <- 1
# eC <- 37/10
# mC <- 0
# fM <- 1.5#2
# eM <- 12/10
# mM <- 0
# mW <- 0
# b <- 1.255
# g <- 0.38
# dC <- 4.6*100
# dM <- 0.46*100


sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0
mW <- 0
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.1 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.1 <- within(pop.df.1,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0.08*100
})


for(t in 2:nsteps){
  pop.df.1 <- within(pop.df.1,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05))
    
    W[t] <- max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW*runif(n=1,min=0.95,max=1.05))
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.1)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)






#############
##Control wolf populations model
#############

sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0
mW <- 0
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.2 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.2 <- within(pop.df.2,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0.08*100
})


for(t in 2:nsteps){
  pop.df.2 <- within(pop.df.2,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05))
    
    W[t] <- min(max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW*runif(n=1,min=0.95,max=1.05)),10)
  })
}

colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.2)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)







#############
##Control moose populations
#############

sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0
mW <- 0
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.3 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.3 <- within(pop.df.3,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0.08*100
})


for(t in 2:nsteps){
  pop.df.3 <- within(pop.df.3,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- min(max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05)),30)
    
    W[t] <- max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW*runif(n=1,min=0.95,max=1.05))
  })
}

colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.3)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)






#############
##Wolf hunting
#############

sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0
mW <- 0.1
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.4 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.4 <- within(pop.df.4,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0.08*100
})


for(t in 2:nsteps){
  pop.df.4 <- within(pop.df.4,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05))
    
    W[t] <- max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW)#*runif(n=1,min=0.95,max=1.05))
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.4)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)





#############
##Moose hunting
#############

sP <- 0.5
Kp <- 240
lP <- 2682.75/1000
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2682.75/1000
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 15841/5000
hh <- 1000#533
fC <- 1
eC <- 37/20
mC <- 0
fM <- 1.5#2
eM <- 12/20
mM <- 0.1
mW <- 0
b <- 0.8
g <- 0.38
dC <- 4.6*100
dM <- 0.46*100



nsteps <- 200
pop.df.5 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.5 <- within(pop.df.5,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 0.07*100
  M[1] <- 0.25*100
  W[1] <- 0.08*100
})


for(t in 2:nsteps){
  pop.df.5 <- within(pop.df.5,{
    P[t] <- max(0,P[t-1] + sP*P[t-1]*(1-P[t-1]/Kp) - lP*P[t-1]*C[t-1]/(hp+P[t-1])) ##plants consumed by Caribou
    L[t] <- max(0,L[t-1] + sL*L[t-1]*(1-L[t-1]/Kl) - lL*L[t-1]*C[t-1]/(hl+L[t-1])) ##lichen consumed by Caribou
    H[t] <- max(0,H[t-1] + sH*H[t-1]*(1-H[t-1]/Kh) - lH*H[t-1]*M[t-1]/(hh+H[t-1])) ##plants consumed by Moose
    
    C[t] <- max(0,C[t-1] + C[t-1]*fC*(P[t-1]/(P[t-1]+hp))*(L[t-1]/(L[t-1]+hl)) - C[t-1]*(1-P[t-1]/(P[t-1]+hp))*(1-L[t-1]/(L[t-1]+hl)) - W[t-1]*eC*C[t-1]/(C[t-1]+dC) - C[t-1]*mC*runif(n=1,min=0.95,max=1.05))
    
    M[t] <- max(0,M[t-1] + M[t-1]*fM*H[t-1]/(H[t-1]+hh) - M[t-1]*(1-H[t-1]/(H[t-1]+hh)) - W[t-1]*eM*M[t-1]/(M[t-1]+dM) - M[t-1]*mM*runif(n=1,min=0.95,max=1.05))
    
    W[t] <- max(0,W[t-1] + (b*(C[t-1]/(dC+C)[t-1]) + b*(M[t-1]/(dM+M[t-1])))*W[t-1] - g*W[t-1] - W[t-1]*mW*runif(n=1,min=0.95,max=1.05))
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.5)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)






range(pop.df.0$C)
range(pop.df.1$C)
range(pop.df.2$C)
range(pop.df.3$C)
range(pop.df.4$C)
range(pop.df.5$C)

range(pop.df.0$M)
range(pop.df.1$M)
range(pop.df.2$M)
range(pop.df.3$M)
range(pop.df.4$M)
range(pop.df.5$M)

range(pop.df.0$W)
range(pop.df.1$W)
range(pop.df.2$W)
range(pop.df.3$W)
range(pop.df.4$W)
range(pop.df.5$W)


















