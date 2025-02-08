library(tidyverse)
library(modelsummary)
library(kableExtra)
library(fixest)
library(texreg)


nsw = haven::read_dta("ps1/data/nsw.dta")
nsw_dw = haven::read_dta("ps1/data/nsw_dw.dta")
cps1 = haven::read_dta("ps1/data/cps1.dta")
psid1 = haven::read_dta("ps1/data/psid1.dta") 


# add up by data_id

data = nsw |>
  bind_rows(nsw_dw) |>
  bind_rows(cps1) |>
  bind_rows(psid1)

# simplify names
data <- data %>% 
  mutate(data_id = if_else(data_id == "Lalonde Sample", "LS", if_else(data_id == "Dehejia-Wahba Sample", "DW", data_id)))

data_all <- data %>% 
  mutate(treat = if_else(treat==1, "T", "C")) %>% 
  mutate(data_id2 = paste0(data_id, "-", treat))


se <- function(x) {
  sd(x) / sqrt(length(x))
}

datasummary(age + education + black + hispanic + married + nodegree + re74 + re75 + re78 ~ 
              data_id2 * (mean + se), 
            data = data_all, 
            title = "Descriptive statistics for all data files",
            notes = "LS: LaLonde Sample, C: control, T: treatment") 


# table 5, col 1
# take out not needed sample
data_5 <- data %>% filter(data_id != "DW")

data_5_col1 <- data_5 %>% filter(treat==0)

data_5_col1 <- data_5_col1 %>% mutate(growth = re78 - re75)

data_5_col1 <- data_5_col1 %>% 
  group_by(data_id) %>% 
  summarise(growth_mean = mean(growth),
            std_error = sd(growth), n=n())


# table 5, col2
data_5_col2 <- data_5 %>% mutate(id = paste0(data_id, "-", treat))

filter_conditions <- list(
  data_5_col2 %>% filter(data_id == "LS"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "PSID"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "CPS1")
) 

col2_result <- map(filter_conditions, ~ feols(re75 ~ treat, data = .x, vcov = "hetero"))
screenreg(col2_result, digits = 0)

# col3

col3_result <- map(filter_conditions, ~ feols(re75 ~ treat + age + age^2 + education + nodegree + black + hispanic, data = .x, vcov = "hetero"))
screenreg(col3_result, digits = 0)

# col4

col4_result <- map(filter_conditions, ~ feols(re78 ~ treat, data = .x, vcov = "hetero"))
screenreg(col4_result, digits = 0)

# col5

col5_result <- map(filter_conditions, ~ feols(re78 ~ treat + age + age^2 + education + nodegree + black + hispanic, data = .x, vcov = "hetero"))
screenreg(col5_result, digits = 0)

# col6
data_5_col2 <- data_5_col2 %>% mutate(growth = re78 - re75)

filter_conditions <- list(
  data_5_col2 %>% filter(data_id == "LS"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "PSID"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "CPS1")
) 

col6_result <- map(filter_conditions, ~ feols(growth ~ treat, data = .x, vcov = "hetero"))
screenreg(col6_result, digits = 0)

# col7
col7_result <-  map(filter_conditions, ~ feols(growth ~ treat + age +age^2, data = .x, vcov = "hetero"))
screenreg(col7_result, digits = 0)

# col8
col8_result <- map(filter_conditions, ~feols(growth ~ treat + re75, data = .x, vcov = "hetero")) 
screenreg(col8_result digits = 0)


# col9
col9_result <- map(filter_conditions, ~feols(growth ~ treat + re75 +age + age^2 + education + black + hispanic + nodegree, data = .x, vcov = "hetero")) 
screenreg(col9_result, digits = 0)



