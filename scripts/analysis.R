library(car)
library(readr)
library(lme4)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(ggeffects)
library(nlme)
library(robustlmm)
library(apaTables)

rm(list = ls())

### Data organisation ###

data <- read_rds("./data/out/2023-01-06_harmonised_data_by_region_by_year_2017-2021.rds")

data <- data %>%
  filter(ethn_dv == "african" | 
           ethn_dv == "caribbean" | 
           ethn_dv == "any other black background" | 
           ethn_dv == "white and black caribbean" | 
           ethn_dv == "white and black african" | 
           ethn_dv == "british/english/scottish/welsh/northern irish" | 
           ethn_dv == "any other white background") %>%
  rename(
    total_pop_density = population_density_All.usual.residents,
    
  ) %>%
  mutate(
    ethn_collapsed = fct_collapse(ethn_dv, 
                                  "white" = c("british/english/scottish/welsh/northern irish",
                                  "any other white background"),
                                  "black" = c("african",
                                  "caribbean",
                                  "any other black background",
                                  "white and black caribbean",
                                  "white and black african")),
    health = factor(health, levels = c("No","Yes")),
    net_income_z = (fimnnet_dv - mean(fimnnet_dv)) / sd(fimnnet_dv),
    age_z = (age_dv - mean(age_dv, na.rm = T)) / sd(age_dv, na.rm = T),
    black_pop_density = (population_density_Mixed.multiple.ethnic.groups..White.and.Black.Caribbean +
                           population_density_Mixed.multiple.ethnic.groups..White.and.Black.African +
                           population_density_Black.African.Caribbean.Black.British),
    white_pop_density = population_density_White - population_density_White..Gypsy.or.Irish.Traveller,
    density_ratio_bw = black_pop_density / white_pop_density,
    total_pop_density_z = (total_pop_density - mean(total_pop_density, na.rm = T)) / sd(total_pop_density, na.rm = T)
  ) %>%
  relocate(
    ethn_collapsed, .after = ethn_dv
  )

# drop Wales - too few Black respondents
data <- data %>%
  filter(gor_dv != "Wales")

### End ###

### Models ###

# Check whether random effects make a difference

# just random intercept for participant
rand_int <- lmer(scghq1_dv ~ (1|pidp), data = data, REML = F)

# add random intercept for region - note this is crossed because some respondents moved
rand_int_reg <- lmer(scghq1_dv ~ (1|pidp) + (1|gor_dv), data = data, REML = F)
summary(rand_int_reg)

# improves fit, but explains tiny amount of variance - causes convergence issues later
anova(rand_int, rand_int_reg)

# add random intercept for year - note this is crossed because year is not nested
# i.e. there is no structure whereby year and other variables are nested to create
# unique observations; all years can occur for all participants and regions and 
# vice versa.
rand_int_reg_year <- lmer(scghq1_dv ~ (1|pidp)  + (1|gor_dv) + (1|year), data = data, REML = F)

# improves fit
anova(rand_int_reg,rand_int_reg_year)

# 1. Likert

# mod_1 <- lmerTest::lmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health +
#                        net_income_z + employ + total_pop_density_z + imd_extent + rr + 
#                          (1|pidp) + (1|gor_dv) + (1|year), data = data,
#                        REML = F, # REML should not be used if intention for model comparison
#                      lmerControl(optimizer = "bobyqa",
#                                   optCtrl = list(maxfun = 20000)))

# remove region ranef becuase causes singularity (small variance)
mod_1 <- lmerTest::lmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health +
                          net_income_z + employ + total_pop_density_z + imd_extent + rr +
                          (1|pidp) +(1|year), data = data,
                        REML = F, # REML should not be used if intention for model comparison
                        lmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 20000)))
summary(mod_1)

# ethnicity x rr interaction
mod_2 <- update(mod_1, ~.+ ethn_collapsed:rr)
summary(mod_2)

# no improvement in fit
anova(mod_1, mod_2)

# Findings so far: Neither RR nor IMD are found to relate to GHQ caseness
# Interaction by ethnicity unimportant

### Stop rates ###
sr_mod_1 <- lmerTest::lmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health + 
                          net_income_z + employ + total_pop_density_z + imd_extent + Black_stop_rate + 
                          White_stop_rate + (1|pidp) + (1|gor_dv) + (1|year), data = data,
                          REML = F,
                        lmerControl(optimizer = "bobyqa", 
                                     optCtrl = list(maxfun = 20000)))
summary(sr_mod_1)

# remove region as we have singularity and region accounts for very little variance compared to other ranefs
sr_mod_1 <- lmerTest::lmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health + 
                             net_income_z + employ + 
                             total_pop_density_z + 
                             imd_extent + Black_stop_rate + 
                             White_stop_rate + (1|pidp) + + (1|year), data = data,
                           REML = F,
                           lmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 20000)))
summary(sr_mod_1)

# add ethnicity x stop-rate interaction
sr_mod_2 <- update(sr_mod_1, ~.+ ethn_collapsed:Black_stop_rate + 
                     ethn_collapsed:White_stop_rate)

summary(sr_mod_2)

# adding interaction improves the fit
anova(sr_mod_1, sr_mod_2)

# visualise predicted values

ems_1 <- ggemmeans(sr_mod_2, terms = c("Black_stop_rate", "ethn_collapsed"))

ggplot(ems_1, aes(x, predicted, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) + 
  ylab("Predicted GHQ-12 score") + xlab("Black stop rate") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_bw()


ems_2 <- ggemmeans(sr_mod_2, terms = c("White_stop_rate", "ethn_collapsed"))

ggplot(ems_2, aes(x, predicted, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) +
  ylab("Predicted GHQ-12 score") + xlab("White stop rate") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme_bw()


emtrends(sr_mod_2, specs = "ethn_collapsed", var = "Black_stop_rate")
emtrends(sr_mod_2, specs = "ethn_collapsed", var = "White_stop_rate")

# I wonder if the overall stop rate in an area would show something interesting?

### Diagnostics ###

#Redefine model with lme4
sr_mod_2 <- lme4::lmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health + 
                             net_income_z + employ + imd_extent + Black_stop_rate + 
                             White_stop_rate + ethn_collapsed:Black_stop_rate + 
                         ethn_collapsed:White_stop_rate + (1|pidp) + 
                         (1|gor_dv) + (1|year), data = data,
                           REML = F,
                       lmerControl(optimizer = "bobyqa", 
                                   optCtrl = list(maxfun = 20000)))
# Residuals

# data$standardised_resids <- rstandard(sr_mod_2)
# data$studentised_resids <- rstudent(sr_mod_2)
# dfbeta <- dfbeta(sr_mod_2)
# dffit <- dffits(sr_mod_2)
# covariance_ratios <- covratio(sr_mod_2)

cooks <- cooks.distance(sr_mod_2)
leverage <- hatvalues(sr_mod_2)

plot(sr_mod_2)

# indication of heteroscedasticity. Try robustlmm

robust_sr_mod_2 <- robustlmm::rlmer(scghq1_dv ~ age_z + sex_dv + ethn_collapsed + health + 
                         net_income_z + employ + imd_extent + Black_stop_rate + 
                         White_stop_rate + ethn_collapsed:Black_stop_rate + 
                         ethn_collapsed:White_stop_rate + (1|pidp) + 
                         (1|gor_dv) + (1|year), data = data)

# Linearity

ggplot(data.frame(x = sr_mod_2@frame[["age_z"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_point() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["sex_dv"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_boxplot() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["ethn_collapsed"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_boxplot() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["health"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_boxplot() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["net_income_z"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_point() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["employ"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_boxplot() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["imd_extent"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_point() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["Black_stop_rate"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_point() +
  theme_bw()

ggplot(data.frame(x = sr_mod_2@frame[["White_stop_rate"]], pearson = residuals(sr_mod_2, type = "pearson")), 
       aes(x, pearson)) + 
  geom_point() +
  theme_bw()

# Normality

