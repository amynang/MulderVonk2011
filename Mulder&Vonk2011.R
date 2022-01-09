library(ggplot2)

# get the data
fauna = read.table("https://figshare.com/ndownloader/files/5620305", 
                   header = T,
                   sep = "\t")[1:29552,]

dutchagroecosystems = read.table("https://figshare.com/ndownloader/files/5620302", 
                                 header = T,
                                 sep = "\t")[1:200,1:20]

muldervonk = read.table("H:/Literature/Mulder&Vonk2011_Traitsoilnematofauna.txt",
                        sep = "\t", header = TRUE)
muldervonk = muldervonk[1:29552,]

p <- ggplot(muldervonk[muldervonk$Lifestage == "juveniles",], aes(x=Length, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p

p <- ggplot(muldervonk[muldervonk$Lifestage == "female",], aes(x=Length, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p

p <- ggplot(muldervonk[muldervonk$Lifestage == "male",], aes(x=Length, color = TAX.MORPHON)) + 
  geom_density() + 
  facet_wrap(vars(TAX.MORPHON), scales = "free") +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank())
p
