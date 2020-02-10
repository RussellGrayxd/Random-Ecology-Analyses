lunardat<-data.frame(table(dat$lunarphase, dat$`Snake moved`))

Lunarplot <- ggplot(data=lunardat, aes(x=Var1, y=Freq, fill=Var2))+
  ggtitle("Lunar Acticity BoCy059") +
  ylab("Animal Activity") + xlab("") +
  theme_minimal(base_size = 14) + 
  geom_bar(position = position_dodge(width = 0.8), width=0.8, stat='identity', color = "black") +
  scale_fill_brewer() +
  theme(legend.title = element_blank()) 
Lunarplot

