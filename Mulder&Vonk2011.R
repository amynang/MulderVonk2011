library(tidyverse)
library(ggplot2)


# get the data
fauna = read.table("https://figshare.com/ndownloader/files/5620305", 
                   header = T,
                   sep = "\t")[1:29552,]

dutchagroecosystems = read.table("https://figshare.com/ndownloader/files/5620302", 
                                 header = T,
                                 sep = "\t")[1:200,1:20]

# muldervonk = read.table("H:/Literature/Mulder&Vonk2011_Traitsoilnematofauna.txt",
#                         sep = "\t", header = TRUE)
# muldervonk = muldervonk[1:29552,]

p <- ggplot(fauna[fauna$Lifestage == "juveniles",], aes(x=Mass, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p

p <- ggplot(fauna[fauna$Lifestage == "female",], aes(x=Mass, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p

p <- ggplot(fauna[fauna$Lifestage == "male",], aes(x=Mass, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p

p <- ggplot(fauna, aes(x=Mass, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p


masses = fauna %>% group_by(TAX.MORPHON) %>% 
  summarise(AvgMass = mean(Mass),
            StDevMass = sd(Mass),
            N = n())
masses = as.data.frame(masses)


m <- 26.4783
s <- 3.821168e+01
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
hist(rlnormTrunc(10000, 
                 meanlog = location, 
                 sdlog = shape, 
                 min = qlnorm(.05, 
                              meanlog = location, 
                              sdlog = shape), 
                 max = qlnorm(.95, 
                              meanlog = location, 
                              sdlog = shape)),
     breaks = 1000)


rlnormtrunc.intuitive = function(n, m, s, p=.9) {
  trnc <- EnvStats::rlnormTrunc(n, 
                     meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                     sdlog = sqrt(log(1 + (s^2 / m^2))), 
                     min = qlnorm((1-p)/2, 
                                  meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                  sdlog = sqrt(log(1 + (s^2 / m^2)))), 
                     max = qlnorm(1-(1-p)/2, 
                                  meanlog = log(m^2 / sqrt(s^2 + m^2)), 
                                  sdlog = sqrt(log(1 + (s^2 / m^2)))))
 return(trnc)
}


hist(rlnormtrunc.intuitive(100000, 26.4783, 3.821168e+01),
     breaks = 1000)
plot(density(rlnormtrunc.intuitive(100000, 26.4783, 3.821168e+01)))


par(mfrow=c(46,46))
for (i in 1:91) {
  try(hist(rlnormtrunc.intuitive(100000, 
                                 masses[i,2], 
                                 masses[i,3],
                                 .98),
           main = paste("Histogram of" , masses[i,1]),
           xlab = NULL,
           ylab = NULL,
           breaks = 1000))
}


