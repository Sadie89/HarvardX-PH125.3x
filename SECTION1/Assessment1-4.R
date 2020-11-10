library(gtools)
library(tidyverse)
# Question 1d
# Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  simulated_winners <- sample(runners, 3, replace = FALSE)
  all(simulated_winners == "Jamaica")
})

mean(results)


# Question 2d: Restaurant management
meals <- seq(1, 12, 1)
sides <- nrow(combinations(6, 2))
drinks <- nrow(combinations(2, 1))
# lunch <- expand.grid(meal = meals, side = sides, drink = drinks)

lunch <- function(meals){
  result <- meals*sides*drinks
  if (result >= 365){
    print(meals)
    print(result)
  }
}

sapply(meals, lunch)


# Question 2e: Restaurant management
meals <- nrow(combinations(6, 1))
sides <- c(2:12)
drinks <- nrow(combinations(3, 1))
# lunch <- expand.grid(meal = meals, side = sides, drink = drinks)

lunch <- function(sides){
  result <- meals*nrow(combinations(sides, 2))*drinks
    print(sides)
    print(result)
}

sapply(sides, lunch)

# Questions 3 and 6: Esophageal cancer and alcohol/tobacco use
head(esoph, 20)
library(tidyverse)

nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases),
            tot=sum(ncontrols) + sum(ncases),
            probability=sum_cases/tot)

# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases),
            tot=sum(ncontrols) + sum(ncases),
            probability=sum_cases/tot)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
esoph %>% filter(ncases >= 1 & tobgp != "0-9g/day") %>%
  summarize(sum_cases=sum(ncases),
            probability=sum_cases/all_cases)

# Given that a person is a control, what is the probability that they smoke 10g or more a day?
esoph %>% filter(ncontrols >= 1 & tobgp != "0-9g/day") %>%
  summarize(sum_ncontrols=sum(ncontrols),
            probability=sum_ncontrols/all_controls)

# For cases, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases),
            probability=sum_cases/all_cases)

# For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases),
            probability=sum_cases/all_cases)

# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases),
            probability=sum_cases/all_cases)

# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases),
            probability=sum_cases/all_cases)

# For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_controls=sum(ncontrols),
            probability=sum_controls/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_controls = sum(ncontrols), sum_cases = sum(ncases),
            prob_controls = sum_controls/all_controls, prob_cases = sum_cases/all_cases,
            ratio = prob_cases/prob_controls)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_ncontrols=sum(ncontrols),
            probability=sum_ncontrols/all_controls)

# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(sum_ncontrols=sum(ncontrols),
            probability=sum_ncontrols/all_controls)

# For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum_ncontrols= sum(ncontrols),
            probability= sum_ncontrols/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum_controls = sum(ncontrols), sum_cases = sum(ncases),
            prob_controls = sum_controls/all_controls, prob_cases = sum_cases/all_cases,
            ratio = prob_cases/prob_controls)
