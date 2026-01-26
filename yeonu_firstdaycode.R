install.packages("readxl")
library(readxl)
my_data <- read_excel("cleanKratdata1217.xlsx")
unique(as.Date(my_data$EditDate))
length(unique(as.Date(my_data$EditDate))
)
library(ggplot2)

ggplot(my_data, aes(x = Species, y = Hind_Limb_Length, fill = Species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Hind_Limb_Length")
library(tidyverse)

my_data <- my_data |> 
  mutate(Tail_Body_Ratio = Tail_Length / Body_Length)
str(my_data[c("Tail_Length", "Body_Length")])
library(dplyr)

my_data <- my_data |> 
  mutate(
    Tail_Length = as.numeric(Tail_Length),
    Body_Length = as.numeric(Body_Length),
    Tail_Body_Ratio = Tail_Length / Body_Length
  )

ggplot(my_data, aes(x = Species, y = Tail_Body_Ratio, fill = Species)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2) + 
  theme_minimal() +
  labs(title = "Tail Length / Body Length",
       y = "Tail Length / Body Length")

ratio_test <- t.test(Tail_Body_Ratio ~ Species, data = my_data)

print(ratio_test)