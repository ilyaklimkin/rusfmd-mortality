options(scipen = 999)

# load R packages
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(broom)
library(ggplot2)

# clear the R environment
rm(list = ls())

# load regions codebook
reg_codebook <- read_xlsx("data/reg_codebook.xlsx") %>%
  mutate(reg_sk = as.integer(reg_sk))

reg_sk_list <- unique(reg_codebook$reg_sk)

rm(reg_codebook)

# load RusFMD files
dr1989_2014 <- read.table("data/DR5a1989-2014.txt",
                          sep = ",", head = T, na = ".")
dr2015_2022 <- read.table("data/DR5a2015-2022.txt",
                          sep = ",", head = T, na = ".")

# unite tables
dr <- bind_rows(dr1989_2014, dr2015_2022) %>%
  filter(Group == "T")

rm(dr1989_2014, dr2015_2022)

# transform data into longer format (tidy data), clean data, filter data
dr <- dr %>%
  pivot_longer(cols = starts_with("DrAa"), names_to = "age", values_to = "dr") %>%
  transmute(
    year = Year,
    reg_sk = Reg,
    sex_sk = as.integer(case_when(
      Sex == "M" ~ 1,
      Sex == "F" ~ 2,
      Sex == "B" ~ 3,
    )),
    age = as.integer(str_remove(age, "DrAa")),
    dr = dr / 1000000
  ) %>%
  filter(reg_sk %in% reg_sk_list)

# smooth the data with Gompertz model
start_age <- 40

model_params <- dr %>%
  filter(age >= start_age) %>%
  mutate(dr = na_if(dr, 0)) %>%
  nest(data = c(age, dr)) %>%
  mutate(
    model = map(data, ~ lm(log(dr) ~ age, data = .x)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied) %>%
  select(year, reg_sk, sex_sk, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(intercept = `(Intercept)`, age_c = age)

# fill 1993-2002 years for region 1196 with NAs
y <- c(1993:2002)
r <- 1196
s <- c(1:3)
a <- c(0,1,seq(5,85,5))

dr_1196 <- expand.grid(
  year = y,
  reg_sk = r,
  sex_sk = s,
  age = a,
  dr = NA,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

dr_1196 <- tibble(dr_1196) %>%
  mutate(
    reg_sk = as.integer(reg_sk),
    age = as.integer(age),
    dr = as.numeric(dr)
  )

dr <- bind_rows(dr, dr_1196) %>%
  arrange(year, reg_sk, sex_sk, age)

rm(dr_1196, a, r, s, y, reg_sk_list)

# model dr
dr <- dr %>%
  left_join(model_params, by = c("year", "reg_sk", "sex_sk")) %>%
  transmute(
    year,
    reg_sk,
    sex_sk,
    age,
    dr = if_else(
      age >= start_age & dr == 0,
      exp(intercept + age_c * age),
      dr
    )
  )

rm(model_params, start_age)

# export DR dataset
write_csv(dr, "data/dr.csv")

# set the age standard (European Population Standard 1976&2013)
age <- c(0,1,seq(5,85,5))

# 1976
weight <- c(0.016,0.064,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,
            0.07,0.06,0.05,0.04,0.03,0.02,0.01,0.01)
esp1976 <- tibble(age, weight) %>%
  mutate(esp_year = 1976)

# 2013
weight <- c(0.01,0.04,0.055,0.055,0.055,0.06,0.06,0.065,0.07,0.07,0.07,
            0.07,0.065,0.06,0.055,0.05,0.04,0.025,0.025)
esp2013 <- tibble(age, weight) %>%
  mutate(esp_year = 2013)

esp <- esp1976 %>%
  bind_rows(esp2013) %>%
  mutate(
    age = as.integer(age),
    esp_year = as.integer(esp_year)
  )

rm(esp1976, esp2013)

# join population weights with mortality data
# calculate SDRs by broad age groups (0-14, 15-49, 50-64, 65+)
sdr <- dr %>%
  left_join(esp, by = "age") %>%
  mutate(
    age = as.integer(case_when(
      age %in% c(0:14) ~ 0,
      age %in% c(15:49) ~ 15,
      age %in% c(50:64) ~ 50,
      age >= 65 ~ 65
    ))
  ) %>%
  group_by(year, reg_sk, sex_sk, esp_year, age) %>%
  mutate(
    weight = weight / sum(weight),
    weighted_dr = dr * weight
  ) %>%
  summarise(sdr = sum(weighted_dr)) %>%
  ungroup()

# calculate aggregated weights
esp_aggr <- esp %>%
  mutate(
    age = as.integer(case_when(
      age %in% c(0:14) ~ 0,
      age %in% c(15:49) ~ 15,
      age %in% c(50:64) ~ 50,
      age >= 65 ~ 65
    ))
  ) %>%
  group_by(age, esp_year) %>%
  summarise(weight = sum(weight)) %>%
  ungroup()

# calculate SDRs for all ages combined
sdr_all <- sdr %>%
  left_join(esp_aggr, by=c("age","esp_year")) %>%
  mutate(weighted_sdr = sdr * weight) %>%
  group_by(year, reg_sk, sex_sk, esp_year) %>%
  summarise(sdr = sum(weighted_sdr)) %>%
  ungroup() %>%
  transmute(year, reg_sk, sex_sk, esp_year, age = as.integer(999), sdr)

# unite data & prepare data for export
sdr <- bind_rows(sdr, sdr_all) %>%
  rename(age_sk = age) %>%
  arrange(year, reg_sk, sex_sk, esp_year, age_sk) %>%
  mutate(sdr = round(sdr * 1000, 2))

write_csv(sdr, "data/sdr.csv")

rm(esp, esp_aggr, sdr_all, age, weight)

# build life tables in order to receive the values of LE
# calculate Life Table
le <- dr %>%
  rename(x = age, mx = dr) %>%
  group_by(year, reg_sk, sex_sk) %>%
  mutate(
    n = case_when(
      x == 0 ~ 1,
      x == 1 ~ 4,
      x %in% c(5:80) ~ 5
    ),
    ax = case_when(
      x == 0 ~ 0.07 + 1.7 * mx,
      x %in% c(1:80) ~ n / 2,
      x == 85 ~ 1 / mx
    ),
    qx = if_else(
      x < 85,
      n * mx / (1 + (n - ax) * mx), # Chiang
      1
    ),
    px = 1 - qx,
    lx = cumprod(lag(px, default = 1)),
    dx = lx * qx,
    Lx = if_else(
      x < 85,
      ax * lx + (n - ax) * lead(lx),
      ax * lx
    ),
    ex = rev(cumsum(rev(Lx))) / lx
  ) %>%
  ungroup() %>%
  filter(x %in% c(0,15,50,65)) %>%
  transmute(
    year, reg_sk, sex_sk, age_sk = x,
    le = round(ex, 2)
  )

# export ex
write_csv(le, "data/le.csv")

#####

le %>%
  mutate(reg_sk = as.character(reg_sk)) %>%
  filter(reg_sk %in% c(1100, 1145, 1181, 1177, 1160, 1196)) %>%
  ggplot(aes(year, le, color = reg_sk)) +
  geom_line() +
  geom_point(size = 0.3) +
  facet_grid(age_sk ~ sex_sk, scales = "free") +
  theme_bw()

sdr %>%
  mutate(reg_sk = as.character(reg_sk)) %>%
  filter(reg_sk %in% c(1100, 1145, 1181, 1177, 1160, 1196), esp_year == 2013) %>%
  ggplot(aes(year, sdr, color = reg_sk)) +
  geom_line() +
  geom_point(size = 0.3) +
  facet_grid(age_sk ~ sex_sk, scales = "free") +
  theme_bw()
