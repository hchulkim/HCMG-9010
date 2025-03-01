library(tidyverse)
library(modelsummary)
library(kableExtra)
library(fixest)
library(broom)
library(texreg)
library(MatchIt)

## Basic and matching theory is solved in ps1-solution.pdf

## Matching empirics

# read in the data
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

# 1
se <- function(x) {
  sd(x) / sqrt(length(x))
}

datasummary(age + education + black + hispanic + married + nodegree + re74 + re75 + re78 ~ 
              data_id2 * (mean + se), 
            data = data_all, 
            title = "Descriptive statistics for all data files",
            notes = "LS: LaLonde Sample, C: control, T: treatment", output = "ps1/output/tab1.tex") 

# 2

# table 5, col 1
# take out not needed sample
data_5 <- data %>% filter(data_id != "DW")

data_5_col1 <- data_5 %>% filter(treat==0)

data_5_col1 <- data_5_col1 %>% mutate(growth = re78 - re75)

data_5_col1 <- data_5_col1 %>% 
  group_by(data_id) %>% 
  summarise(growth_mean = mean(growth),
            std = sd(growth), n=n()) %>% 
  mutate(std_error = std / sqrt(n))


# table 5, col2
data_5_col2 <- data_5 %>% mutate(id = paste0(data_id, "-", treat))

filter_conditions <- list(
  data_5_col2 %>% filter(data_id == "LS"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "PSID"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "CPS1")
) 

col2_result <- map(filter_conditions, ~ feols(re75 ~ treat, data = .x, vcov = "hetero"))
screenreg(col2_result, digits = 0)

col2_result <- imap(col2_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)

# col3

col3_result <- map(filter_conditions, ~ feols(re75 ~ treat + age + age^2 + education + nodegree + black + hispanic, data = .x, vcov = "hetero"))
screenreg(col3_result, digits = 0)

col3_result <- imap(col3_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)


# col4

col4_result <- map(filter_conditions, ~ feols(re78 ~ treat, data = .x, vcov = "hetero"))
screenreg(col4_result, digits = 0)

col4_result <- imap(col4_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)

# col5

col5_result <- map(filter_conditions, ~ feols(re78 ~ treat + age + age^2 + education + nodegree + black + hispanic, data = .x, vcov = "hetero"))
screenreg(col5_result, digits = 0)

col5_result <- imap(col5_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)

# col6
data_5_col2 <- data_5_col2 %>% mutate(growth = re78 - re75)

filter_conditions <- list(
  data_5_col2 %>% filter(data_id == "LS"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "PSID"),
  data_5_col2 %>% filter(id == "LS-1" | data_id == "CPS1")
) 

col6_result <- map(filter_conditions, ~ feols(growth ~ treat, data = .x, vcov = "hetero"))
screenreg(col6_result, digits = 0)

col6_result <- imap(col6_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)

# col7
col7_result <-  map(filter_conditions, ~ feols(growth ~ treat + age +age^2, data = .x, vcov = "hetero"))
screenreg(col7_result, digits = 0)

col7_result <- imap(col7_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)

# col8
col8_result <- map(filter_conditions, ~feols(growth ~ treat + re75, data = .x, vcov = "hetero")) 
screenreg(col8_result, digits = 0)

col8_result <- imap(col8_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)


# col9
col9_result <- map(filter_conditions, ~feols(growth ~ treat + re75 +age + age^2 + education + black + hispanic + nodegree, data = .x, vcov = "hetero")) 
screenreg(col9_result, digits = 0)

col9_result <- imap(col9_result, ~ tidy(.x) %>% mutate(id = .y)) %>% 
  bind_rows() %>% 
  filter(term == "treat") %>% 
  select(-term)


# now add them all up to get a table 5
data_list <- mget(paste0("col", 2:9, "_result"))

data_list <- map(data_list, ~ select(.x, -c("statistic", "p.value")))


suffixes <- 2:9  

table_5 <- map2(
  data_list, suffixes, 
  ~ rename(.x, 
           !!paste0("estimate", .y) := estimate, 
           !!paste0("std.error", .y) := std.error)
) %>% 
  bind_cols()

table_5 <- table_5 %>% 
  select(-starts_with("id")) %>% 
  bind_cols(id = c("Controls", "PSID-1", "CPS-SSA-1"))

data_5_col1 <- data_5_col1 %>% rename(estimate1 = growth_mean, "std.error1"=std_error) %>% 
  select(data_id, estimate1, std.error1)

table_5 <- data_5_col1 %>%
  mutate(data_id = if_else(data_id =="LS", "Controls", if_else(data_id == "CPS1", "CPS-SSA-1", "PSID-1"))) %>% 
  left_join(table_5, by=c("data_id"="id"))


table_5 <- table_5 %>% mutate(across(matches("\\d$"), ~ round(.x, 0)))

# Generate LaTeX table as a string
latex_table <- table_5 %>%
  kable(format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position", table.envir = NULL)

# Save the output to a .tex file
writeLines(latex_table, "ps1/output/table-5.tex")


# 4. psm for dw2002
data_all5 <- data_all %>% 
  filter(data_id != "LS") %>% 
  mutate(treat = if_else(treat =="T", 1, 0))

data_all5 <- data_all5 %>% mutate(u74 = if_else(re74==0, 1, 0), u75= if_else(re75==0, 1, 0))

# Estimate propensity score using logistic regression
filter_conditions <- list(data_cps = data_all5 %>% filter(data_id != "PSID" & data_id2 != "DW-C"),
                          data_psid = data_all5 %>% filter(data_id != "CPS1" & data_id2 != "DW-C"))

propensity_model <- map(filter_conditions, ~ glm(treat ~ age + age^2 + age^3 + education + education^2 + married + nodegree + black + hispanic + re74 + re75 + u74 + u75 + education:re74, family = binomial(), data = .x))

data_all5_ps <- imap(filter_conditions, ~ .x %>% mutate(propensity_score = predict(propensity_model[[.y]], type = "response")))

# get mean values of X
# Estimate propensity scores and match using nearest neighbor method
match_model <- map(filter_conditions, ~ matchit(treat ~ age + age^2 + age^3 + education + education^2 + married + nodegree + black + hispanic + re74 + re75 + u74 + u75 + education:re74, data = .x, method = "nearest", distance = "logit", ratio = 1))

matched_data <- map(match_model, ~ match.data(.x)) %>% bind_rows() 


datasummary(age + education + black + hispanic + married + re74 + re75 + re78 ~ data_id * mean, data = matched_data, title = "Group means of X variables", output = "ps1/output/tab3.tex")

# 5 figure 1
data_all5_1 <- data_all5_ps[[1]]

# Find the minimum propensity score in the treatment group
min_treat_ps <- data_all5_1 %>%
  filter(treat == 1) %>%
  summarize(min_ps = min(propensity_score)) %>%
  pull(min_ps)



# Filter out rows with propensity scores smaller than min_treat_ps
# Create histogram with grouping by treat
data_all5_1 %>% filter(treat==1 | propensity_score >= min_treat_ps) %>%  
  ggplot(aes(x = propensity_score, fill = factor(treat), color = factor(treat))) +
  geom_histogram(binwidth = 0.05, alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  labs(title = "Histogram of Propensity Scores",
       x = "Propensity Score",
       y = "Count",
       fill = "Group",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05)) +
  coord_cartesian(ylim = c(0, 200))
ggsave("ps1/output/fig1.png")

# 5 figure 2

data_all5_2 <- data_all5_ps[[2]]

# Find the minimum propensity score in the treatment group
min_treat_ps <- data_all5_2 %>%
  filter(treat == 1) %>%
  summarize(min_ps = min(propensity_score)) %>%
  pull(min_ps)

# Filter out rows with propensity scores smaller than min_treat_ps
# Create histogram with grouping by treat
data_all5_2 %>% filter(treat==1 | propensity_score >= min_treat_ps) %>%  
  ggplot(aes(x = propensity_score, fill = factor(treat), color = factor(treat))) +
  geom_histogram(binwidth = 0.05, alpha = 0.4, position = "identity") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  labs(title = "Histogram of Propensity Scores",
       x = "Propensity Score",
       y = "Count",
       fill = "Group",
       color = "Group") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(0,1,by=0.05))+
  coord_cartesian(ylim = c(0, 200))

ggsave("ps1/output/fig2.png")





###########################################################################333


filter_conditions <- list(data_cps = data_all5 %>% filter(data_id != "PSID" & data_id2 != "DW-C"),
                          data_psid = data_all5 %>% filter(data_id != "CPS1" & data_id2 != "DW-C"))



# 6 table 2 dw
# load necessary packages
library(MatchIt)
data_cps <- data_all5 %>% filter(data_id != "PSID" & data_id2 != "DW-C")
data_psid <- data_all5 %>% filter(data_id != "CPS1" & data_id2 != "DW-C")

# cps first
match_model_cps1 <- matchit(treat ~ age + age^2 + age^3 + education + education^2 + married + nodegree + black + hispanic + re74 +re75 + u75 + u75 +education:re74, data = data_cps, method = "nearest", distance = "logit", ratio = 1, replace = FALSE, m.oder = "random")




match_model <- map(filter_conditions, ~ matchit(treat ~ age + age^2 + age^3 + education + education^2 + married + nodegree + black + hispanic + re74 + re75 + u74 + u75 + education:re74, data = .x, method = "nearest", distance = "logit", ratio = 1))

matched_data <- map(match_model, ~ match.data(.x)) %>% bind_rows() 












library(boot)

# row 1: nsw
nsw <- data_all5_ps[[1]] %>% 
  filter(data_id2 == "DW-T")

nsw_nmean <- nsw %>% 
  summarise(obs = n(), ps_mean = mean(propensity_score))

# Define a function for bootstrapping
boot_no_ctrl <- function(data, indices) {
  d <- data[indices, ]  # Resample data
  fit <- lm(re78 ~ treat, data = d)  # Run regression
  return(coef(fit))  # Extract coefficients
}

boot_ctrl <- function(data, indices) {
  d <- data[incides, ]
  fit <- lm(re78 ~ treat + age + education + married + nodegree + black + hispanic + re74 + re75, data = d)
  return(coef(fit))
}

# Run bootstrap with 100 repetitions
boot_results <- function(data) {
  boot_int_no <- boot(data, statistic = boot_no_ctrl, R = 100)
  boot_int <- boot(data, statistic = boot_ctrl, R = 100)
  boot_ef <-boot_int_no 
}


boot_results <- boot(data = data_all5_ps[[1]], statistic = boot_no_ctrl, R = 100)

# Compute standard errors from bootstrap
boot_se <- apply(boot_results$t, 2, sd)



# 6 table 3 dw



