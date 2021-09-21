fits <- list(
  fit1 <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + as.factor(receiver_gsis_pos) + ti(air_yards,yardline_100)
              + s(passer_age) + as.factor(pass_location),
              data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE),
  
  fit2 <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + as.factor(receiver_gsis_pos) + ti(air_yards,yardline_100)
              + as.factor(pass_location),
              data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE),
  
  fit3 <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + as.factor(receiver_gsis_pos) + ti(air_yards,yardline_100)
             + s(passer_age) + as.factor(pass_location) + s(ydstogo),
             data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE),
  
  fit4 <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + as.factor(receiver_gsis_pos),
              data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE)
)
gof <- map_df(fits, glance, .id= "model")
summary(fit4)

rand_for <- function(a1, a2, a3, a4) {
  model <- ranger(
    formula         = complete_pass ~ air_yards + yardline_100  + receiver_gsis_pos + passer_age + pass_location,
    data            = passdf, 
    num.trees       = a1,
    mtry            = a2,
    min.node.size   = a3,
    sample.fraction = a4,
    seed            = 123,
    importance      = 'impurity'
  )
  
  model$prediction.error
  model
}

hyper_grid <- expand.grid(
  trees = seq(200,600,200),
  mtry       = seq(1, 5, by = 2),
  node_size  = seq(4, 12, by = 4),
  sample = c(.5,.75))%>%
  mutate(mod = purrr::pmap(list(trees, mtry, node_size, sample), rand_for))#,

temp <- rand_for(600,1,4,.75)
temp2 <- rand_for(400,1,11,0.5)

temp$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")