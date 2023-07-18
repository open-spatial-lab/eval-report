library(tidyverse)
library(DescTools)
library(DT)
library(stargazer)
library(vtable)

setwd("~/Documents/GitHub/eval-report")

data <- read.csv("data/Partner DMA Scores + Responses.csv")
head(data)
dim(data)

data_scores <- data[63:76]

# Exploring overall mean and median
mean <- round(colMeans(data[63:76]), 2)
median <- apply(data[63:76], 2, median)
summary_stats <- summary(data_scores)

# Summary stats table
st(data_scores, add.median = TRUE)

st(data_scores)

# What field does your organization primarily work in? 

sum(grepl("Education", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Health", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Social Services", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Urban Development", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Energy & Environment", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Humanitarian Relief", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("International Development", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Public Safety", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Economic Development", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Gender Equality", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Racial and Social Justice", data$What.field.does.your.organization.primarily.work.in.))
sum(grepl("Other", data$What.field.does.your.organization.primarily.work.in.))


#### Budget ####

budget <- data %>% 
  select(1,9,63:76) %>%
  group_by(What.is.your.organization.s.annual.operating.budget..in.U.S..dollars..) %>%
  summarise_at(.vars = names(.)[3:16],.funs = c(mean="mean"))
budget

#interactive
datatable(budget)

budget_transpose = t(budget)
write.csv(budget_transpose, "outputs/budget.csv")

#### Budget <1M ####
less10M <- data %>% 
  select(1,9,63:76) %>%
  group_by(What.is.your.organization.s.annual.operating.budget..in.U.S..dollars..) %>%
  filter(!What.is.your.organization.s.annual.operating.budget..in.U.S..dollars.. %in% 
           c("$5 million to $9.9 million", "$10 million to $24.9 million")) %>%
  summarise_at(.vars = names(.)[3:16],.funs = c(mean="mean"))

less10M_t = t(less10M)
write.csv(less10M_t, "outputs/budgetLess10M.csv")

#### Number of Employees ####

staff <- data %>%
  select(1,10,63:76) %>%
  group_by(How.many.employees.work.at.your.organization.) %>%
  summarise_at(.vars = names(.)[3:16],.funs = c(mean="mean"))

staff_t <- t(staff)
write.csv(staff_t, "outputs/staff.csv")  


