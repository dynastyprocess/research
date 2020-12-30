library(tidyverse)
library(magrittr)
random_bet <- function(...,times){
  x <- runif(times) * 100
  ten <- as.integer(x<=10) %>% 
    multiply_by(206) %>% 
    sum()
  twenty <- as.integer(x<=20) %>% 
    multiply_by(80) %>% 
    sum()
  tibble(ten = ten, twenty = twenty) %>% 
    mutate(
      better = case_when(ten > twenty ~ "ten",
                         twenty > ten ~ "twenty",
                         TRUE ~ "even")
    )
}
map_dfr(1:10,random_bet,times = 40) %>% 
  summarise(sum(ten), sum(twenty))

library(tidyverse)
library(magrittr)
random_bet <- function(...,times){
  x <- runif(times) * 100
  ten <- as.integer(x<=50) %>% 
    multiply_by(1) %>% 
    sum()
  twenty <- as.integer(x<=25) %>% 
    multiply_by(2) %>% 
    sum()
  tibble(ten = ten, twenty = twenty) %>% 
    mutate(
      better = case_when(ten > twenty ~ "oneflip",
                         twenty > ten ~ "twoflip",
                         TRUE ~ "even")
    )
}
map_dfr(1:10,random_bet,times = 100) %>% 
  summarise(sum(ten), sum(twenty))

library(tidyverse)
library(glue)
random_bet <- function(...){
  x <- runif(1000) * 100
  z <- x %>% summarise(sum(num<=50))*1 %>% round(2)
  y <- x %>% summarise(sum(num<=25))*2 %>% round(2)
  tibble(oneflip = z, twoflip = y) %>% 
    mutate(
      better = case_when(oneflip > twoflip ~ "oneflip",
                         twoflip > oneflip ~ "twoflip",
                         TRUE ~ "even")
    )
}
map_dfr(1:10,random_bet)