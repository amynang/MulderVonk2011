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

# # group by taxon and get average, standard dev and number of obs
# masses = fauna %>% group_by(TAX.MORPHON) %>% 
#   summarise(AvgMass = mean(Mass),
#             StDevMass = sd(Mass),
#             N = n())
# # tibbles suck
# masses = as.data.frame(masses)



# http://nemaplex.ucdavis.edu/Ecology/nematode_weights.htm
# Body Mass of nematodes may be calculated using the Andrássy (1956) formula 
# W = (L*D2)/(1.6*106) where W is the mass (as fresh weight μg) per individual, 
# L is the nematode length (μm) and D is the greatest body diameter (μm).

fauna <- fauna %>% mutate(Andrassy = (Length*Width^2)/(1.6*10^6),
                          .keep = "all",
                          .after = Width)
# So Mulder and Vonk assume nematodes are 80% water
plot(fauna$Mass,fauna$Andrassy)

masses = fauna %>% group_by(TAX.MORPHON) %>% 
  summarise(N = n(),
            feeding.type = first(Type.of.feeding.interaction),
            AvgMass = mean(Andrassy),
            StDevMass = sd(Andrassy))
# tibbles suck
masses = as.data.frame(masses)
write.csv2(masses, file = "Mulder&Vonk2011_bodymass&feeding.csv",
           quote = F,
           row.names = F,
           sep = ";")

# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/
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

# function to get a sample of random data that follows a log-normal distribution 
# and has arithmetic mean of m and a standard deviation of s
# p determines truncation at desired percentile interval (.9 gives the 90th)
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


hist(rlnormtrunc.intuitive(10000, 26.4783, 3.821168e+01),
     breaks = 1000)
plot(density(rlnormtrunc.intuitive(100000, 26.4783, 3.821168e+01)))


#par(mfrow=c(46,46))
for (i in 1:91) {
  try(hist(rlnormtrunc.intuitive(10000, 
                                 masses[i,2], 
                                 masses[i,3],
                                 .98),
           main = paste("Histogram of" , masses[i,1]),
           xlab = NULL,
           ylab = NULL,
           breaks = 1000))
}


