install.packages("ggtext")
library(ggtext)
#Plot the Finger length vs Relative size
df %>%
  ggplot(aes(y= Finger_Length, x = relative_size, color =  Species))+
  geom_point()+
  geom_smooth(method = "lm")+ theme_bw()+
  theme(
    legend.text=element_text(face="italic"),
    legend.position=c(0.85,0.2),
    plot.title=element_markdown(hjust=0.5)
  )+
  scale_color_manual(values=c("darkturquoise","palevioletred"))+
  labs(
    x="Relative size of head (Head length / Body length)",
    y="Finger length",
    title="Finger Length over Relative Head Size of 
    <br>*Dipodomys heermanni* and *Dipodomys venustus*"
  )

#Running bernouilli model to predict species based on measurements
m.bern.finger <- brm(Species ~ 1 + Finger_Length + relative_size, 
                     data = df, 
                     family = bernoulli(), 
                     chains = 4, iter = 2000,
                     file = "output/m.bern.finger") 
#summary model
summary(m.bern.finger)
plot(m.bern.finger)

#Predict the response based on the model and plot it
preds.m.finger <- predict_response(m.bern.finger)
plot(preds.m.finger, show_data = TRUE)

#Check the probability of each species
pp_check(m.bern.finger, type="scatter_avg")

#Use function to check if these were right
fitted(m.bern.finger)