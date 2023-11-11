## Loading packages -----------

library(tidyverse)
library(lubridate)
library(stringr)
library(stargazer)
library(rsq)
library(reshape2)
library(zoo)
library(broom)
library(nlme)
modelname = "dicfullmc10thr10defnob40noa1_4t"

## Loading all functions from the auxfun folder -----------
file_list <- list.files("auxfun", full.names = TRUE)
lapply(file_list, source)

## Loading cequity_mapper --------

cequity_mapper <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/cequity_mapper.csv")

## Loading patent_ik --------
print("Cleaning patent database...")
patent_ik <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/KPSS_2020_public.csv") %>%
  mutate(year = as.numeric(substr(filing_date,7,10)) ) %>%
  mutate(xi_real = coalesce(xi_real, 0)) %>%
  group_by(permno, year) %>%
  summarize(xi_total = sum(xi_real, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(permno) %>%
  mutate(xi_cumsum = cumsum(xi_total)) %>%
  ungroup() %>%
  rename(LPERMNO = permno)  

## Creating figfolder out of dicname --------
dir.create(file.path("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/", modelname), showWarnings = FALSE)
figfolder = paste0("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/", modelname, "/")
filecounter(figfolder)

## Cleaning Fama-French factors -------------
print("Loading Fama-French factors...")
ff3fw <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff3fw.csv") %>%
  cleanffw()

ff5fw <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff5fw.csv") %>%
  cleanffw() %>%
  group_by(yw) %>%
  summarize(across(.cols = everything(), sum)) %>%
  ungroup()

## Cleaning linkt --------
print("Cleaning linkt database...")
linkt <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/CRSP-Compustat Merged Database - Linking Table.csv")
linkt = linkt %>%
  group_by(gvkey) %>%
  fill(LPERMNO, .direction = "downup") %>%
  ungroup() %>%
  mutate(CUSIP8 = substr(cusip, 1, 8)) %>%
  select(CUSIP8, LPERMNO, sic, cik, gvkey, conm, naics) %>%
  mutate(naics4 = substr(as.character(naics), 1, 4)) %>%
  select(-naics) %>%
  group_modify(~create_ind12(.x)) %>%
  group_modify(~add_hi_tech_column(.x)) %>%
  distinct()

## Cleaning skilldata --------
print("Loading skilldata...")
skilldata <- read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/belo_labor_skill_data.csv")

expandgrid = expand.grid(YEAR = 2014:2022, naics4 = unique(skilldata %>% filter(YEAR == 2013) %>% pull(ind))) %>%
  rename(year = YEAR) 

skilldata <- skilldata %>%
  rename(naics4 = ind) %>%
  rename(year = YEAR) %>%
  full_join(expandgrid, by = c("naics4", "year")) %>%
  arrange(year, naics4) %>%
  group_by(naics4) %>%
  fill("Skill", .direction = "down") %>%
  ungroup()

## Creating topic_map --------
load("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/comp_funda2.Rdata")
peterstaylor <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/peterstaylor.csv")

compustat_thin = comp_funda2%>%
  mutate(gvkey = as.integer(GVKEY)) %>%
  left_join(peterstaylor, by = c("fyear", "gvkey"))  %>%
  rename(year = fyear) %>%
  select(K_int_Know, K_int, at, LPERMNO, year)

print("Creating topic_map...")
topic_map <- read.csv(paste0("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/", modelname, "/topic_map_2006_2022.csv")) %>%
  left_join(linkt, by = c("CIK" = "cik")) %>%
  mutate(naics4 = as.numeric(naics4)) %>%
  left_join(skilldata, by = c("naics4", "year")) %>%
  left_join(patent_ik, by = c("LPERMNO", "year")) %>%
  arrange(year, LPERMNO) %>%
  group_by(LPERMNO) %>%
  fill(xi_cumsum, .direction = "down") %>%
  ungroup() %>%
  mutate(xi_cumsum = ifelse(is.na(xi_cumsum),NA, xi_cumsum)) %>%
  mutate(xi_total = ifelse(is.na(xi_total), NA, xi_total)) %>%
  left_join(compustat_thin, by = c("LPERMNO", "year")) %>%
  mutate(xir_cumsum = ifelse(is.na(xi_cumsum/at), NA, xi_cumsum/at)) %>%
  mutate(xir_total = ifelse(is.na(xi_total/at), NA, xi_total/at)) %>%
  sample_me(figfolder) %>%
  understand_topics(figfolder) %>%
  select(-K_int_Know, -K_int) 

set.seed(1)
sample_topic_map = topic_map %>%
  select(conm, year, CIK, starts_with("topic")) %>%
  sample_n(10)


## Cleaning stocks data --------
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxwe_post2005short.Rdata") 
stox = stoxwe_post2005short %>%
  mutate(y = yw%/%100) %>%
  inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "year")) %>%
  filter(crit_ALL == 1) %>%
  inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) %>%
  filter(y >= min(topic_map$year)) %>%
  left_join(ff5fw, by = "yw") %>%
  mutate(eretw = retw - RF) %>%
  drop_na(eretw) %>%
  nest_by(PERMNO) %>%
  mutate(mod = list(lm(formula3ff <- eretw ~ Mkt.RF, data = data))) %>% #mutate(mod = list(lm(formula3ff <- eretm ~ Mkt.RF+ SMB + HML + RMW + CMA, data = data))) %>%
  mutate(augmented = list(broom::augment(mod, data = data))) %>%
  select(-mod, -data) %>%
  unnest(augmented)

stox_aw = stox %>%
  filter(!is.na(.resid)) %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(aw_residuals = sum(.resid * at, na.rm = TRUE)/sum(at, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ntile_topic_kk) %>%
  mutate(cumsum_awr = cumsum(aw_residuals)) %>%
  ungroup() %>%
  yw2day()
  
ggplot(stox_aw, aes(x = yw, y = cumsum_awr, color = as.factor(ntile_topic_kk))) +
  geom_line() +
  labs(x = "Year-Week", y = "Value", color = "Ntile Topic KK") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ndim_mb = 5
ndim_me = 5
ndim_ik = 3
ndim_kkc = 2

print("Creating compustat_sel...")
compustat_sel = comp_funda2%>%
  mutate(gvkey = as.integer(GVKEY)) %>%
  left_join(peterstaylor, by = c("fyear", "gvkey"))  %>%
  mutate(K_int_Know = coalesce(K_int_Know, 0)) %>% #filter(ceq > 0) %>%
  select(prcc_f, prcc_c, ppegt, csho, ceq, cusip, fyear, exchg, K_int, K_int_Know) %>%
  mutate(mb = (csho*prcc_f)/ceq, 
         me = csho*prcc_f,
         CUSIP8 = str_sub(cusip, 1, -2)) %>%
  select(-cusip) %>%
  rename(y = fyear) %>%
  mutate(kk_share = K_int_Know/ppegt) %>%
  inner_join(stox, by = c("y", "CUSIP8")) %>%
  group_by(y) %>%
  drop_na(me, mb) %>%
  mutate(med_kk_share = median(kk_share[kk_share != 0], na.rm = TRUE),
         kk_level = case_when(kk_share == 0 ~ 0,
                              kk_share < med_kk_share ~ 1,
                              TRUE ~ 2)) %>%
  mutate(med_NYSE_me = median(me[exchg == 11], na.rm = TRUE)) %>%
  mutate(med_NYSE_mb70p = quantile(mb[exchg == 11], prob = 0.7, na.rm = TRUE)) %>%
  mutate(med_NYSE_mb30p = quantile(mb[exchg == 11], prob = 0.3, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(y) %>%
  mutate(mb_5tile = ntile(mb, 5), me_5tile = ntile(me, 5)) %>%
  ungroup() %>%
  mutate(pf_number = 10*me_5tile + mb_5tile) 

stock_evol(compustat_sel, "ntile_topic_kk", "")
stock_evol(compustat_sel, "ntile_topic_kk_ind12", "_aggind")
stock_evol(compustat_sel, "max_topic", "_byg")

dog = comp_funda2 %>%
  mutate(book = ceq,
         market = csho*prcc_f) %>%
  arrange(fyear) %>%
  group_by(PERMNO) %>%
  mutate(book_lag = dplyr::lag(book)) %>%
  mutate(market_lag = dplyr::lag(market))
  
compustat_sel2 = 
  mutate(gvkey = as.integer(GVKEY)) %>%
  select(prcc_f, prcc_c, ppegt, csho, ceq, cusip, fyear, exchg, K_int, K_int_Know) %>%
  mutate(book = ceq,
         market = csho*prcc_f,
         CUSIP8 = str_sub(cusip, 1, -2)) %>%
  select(-cusip) %>%
  rename(y = fyear) %>%
  inner_join(stox, by = c("y", "CUSIP8")) %>%
  arrange(y) %>%
  group_by(PERMNO) %>%
  mutate(book_lag = dplyr::lag())
  group_by(y) %>%
  mutate(mb_5tile = ntile(mb, 5), me_5tile = ntile(me, 5)) %>%
  ungroup() %>%
  mutate(pf_number = 10*me_5tile + mb_5tile)

stock_evol(compustat_sel, "ntile_topic_kk", "")
stock_evol(compustat_sel, "ntile_topic_kk_ind12", "_aggind")
stock_evol(compustat_sel, "max_topic", "_byg")


pf_ret = compustat_sel %>%
  group_by(yw, pf_number) %>%
  summarize(ret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RF = mean(RF))

kkhml_ret = compustat_sel %>%
  drop_na(topic_kk) %>%
  ungroup() %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(ret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
  pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = ret) %>%
  transmute(yw, kkhml = kk4-kk1)

#' 
#' ## Cross-Sectional Regression\
eret_we = pf_ret %>%
  inner_join(kkhml_ret, by = c("yw")) %>%
  rename(eretw = ret)  %>%
  drop_na() %>%
  ungroup()

formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF

first_stage1 = eret_we %>%
  ungroup() %>%
  nest_by(pf_number) %>%
  mutate(mod = list(lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = data))) %>%
  summarize(eretw = mean(data$eretw), t = length(data$eretw), tidy(mod)) 

get_sigmae = first_stage1 %>%
  mutate(sigmae = t*std.error^2) %>%
  filter(term == "(Intercept)") %>%
  select(pf_number, sigmae) %>%
  drop_na()

first_stage2 = first_stage1 %>%
  select(-std.error, -statistic, -p.value) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  full_join(get_sigmae, by = c("pf_number")) %>%
  drop_na()

#corr = mean((first_stage$`(Intercept)`)^2)*mean(first_stage$t)

second_stage_ols = lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = first_stage2)


second_stage_wls = lm(formula3ff, data = first_stage2, weights = first_stage2$sigmae)
summary(second_stage_ols)
summary(second_stage_wls)