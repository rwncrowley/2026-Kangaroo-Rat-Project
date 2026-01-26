library(tidyverse)

#install.packages("read.xl")
library(readxl)

clean_kangaroo <- cleanKratdata1217

#finding unique data collection days
unique(as.Date(clean_kangaroo$CreationDate))

clean_species <- clean_kangaroo |> mutate(correct_species=case_when(CreationDate > "2025-11-7" ~ "ve", 
                                                   CreationDate < "2025-10-20"~"he", 
                                                   CreationDate == "2025-7-22"~"ve"))

measurements <- clean_species %>% 
  select(CreationDate, Hind_Limb_Length, Foot_Width, Wrist_Width, Forelimb_Length_Nail_to_Elbow,
         Bolea_Head_Width, Maxillary_Head_Width, Head_Length, Finger_Length,
         Hand_Width, Ear_Length_Notch, Toe_Length, Foot_Length, Tail_Length,
         Body_Length, Weight_Animal_Only, Sex, correct_species)

#tail length over body length
measurements %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  ggplot(mapping = aes(x = Body_Length, y = Tail_Length)) +
  geom_point(aes(color = Sex))+
  facet_wrap(~correct_species)

#head width
measurements %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  ggplot(mapping = aes(x = Bolea_Head_Width, y = Maxillary_Head_Width)) +
  geom_point(aes(color = Sex))+
  facet_wrap(~correct_species)

#foot width and length (shows some pattern...)
measurements %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  ggplot(mapping = aes(x = Foot_Length, y = Foot_Width)) +
  geom_point(aes(color = Sex))+
  facet_wrap(~correct_species)

#Body Length and Animal Weight
measurements %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  ggplot(mapping = aes(x = Body_Length, y = Weight_Animal_Only)) +
  geom_point(aes(color = Sex))+
  facet_wrap(~correct_species)

#hand width and foot width
measurements %>% 
  filter(Sex %in% c("Female", "Male")) %>% 
  ggplot(mapping = aes(x = Foot_Width, y = Hand_Width)) +
  geom_point(aes(color = Sex))+
  facet_wrap(~correct_species)

