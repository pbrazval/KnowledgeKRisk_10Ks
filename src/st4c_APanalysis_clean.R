## Loading packages -----------

library(tidyverse)
library(lubridate)
library(stringr)
library(stargazer)
library(rsq)
library(reshape2)
library(zoo)
library(lubridate)
library(broom)
library(nlme)
library(moments)
source("utilities_R.R")

## Basic configurations -----------
splitvar = "ntile_topic_kk"
modelname = "dicfullmc10thr10defnob40noa1_4t"
dir.create(file.path("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/", modelname), showWarnings = FALSE)
figfolder = paste0("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/", modelname, "/")

## Loading all csvs -----------
patent_ik_orig <- read.csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/KPSS_2020_public.csv")
cequity_mapper <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/cequity_mapper.csv")
ff3fw_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff3fw.csv")
ff5fw_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff5fw.csv") 
ff3fm_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF3F.csv")
ff5fm_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF5F.csv") 
linkt_orig <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/CRSP-Compustat Merged Database - Linking Table.csv")
load("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/comp_funda2.Rdata")
peterstaylor <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/peterstaylor.csv")
skilldata_orig <- read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/belo_labor_skill_data.csv")
topic_map_orig <- read.csv(paste0("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/", modelname, "/topic_map_2006_2022.csv"))
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxwe_post2005short.Rdata") 
stoxwe_orig = stoxwe_post2005short
load("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/stoxmo_post2000short.Rdata")
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxda_post2005veryshort.Rdata")
stoxda_orig = stoxwe_post2005short

## Defining source of stocks information -----------
stoxmo_orig = stoxmo_post2000short

## Recreating equity mapper. May be commented for speed
cequity_mapper = redo_equity_mapper(comp_funda2, figfolder)

## Cleaning dataframes --------
print("Cleaning dataframes.")

patent_ik <- patent_ik_orig %>%  
  mutate(year = as.numeric(substr(filing_date,7,10)) ) %>%
  mutate(xi_real = coalesce(xi_real, 0)) %>%
  group_by(permno, year) %>%
  summarize(xi_yeartotal = sum(xi_real)) %>%
  ungroup() %>%
  group_by(permno) %>%
  mutate(xi_cumsum = cumsum(xi_yeartotal)) %>%
  ungroup() %>%
  rename(LPERMNO = permno)  

ff3fm <- ff3fm_orig %>%
  cleanff() %>%
  drop_na()

ff5fm <- ff5fm_orig %>%
  cleanff() %>%
  drop_na()

ff3fw <- ff3fw_orig %>%
  cleanffw() %>%
  drop_na()

ff5fw <- ff5fw_orig %>%
  cleanffw() %>%
  group_by(yw) %>%
  summarize(across(.cols = everything(), sum)) %>%
  ungroup()

linkt = linkt_orig %>%
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

expandgrid = expand.grid(YEAR = 2014:2022, naics4 = unique(skilldata %>% filter(YEAR == 2013) %>% pull(ind))) %>%
  rename(year = YEAR) 

skilldata <- skilldata_orig %>%
  rename(naics4 = ind) %>%
  rename(year = YEAR) %>%
  full_join(expandgrid, by = c("naics4", "year")) %>%
  arrange(year, naics4) %>%
  group_by(naics4) %>%
  fill("Skill", .direction = "down") %>%
  ungroup()

## Creating topic_map --------
compustat_pt = comp_funda2 %>%
  mutate(gvkey = as.integer(GVKEY)) %>%
  left_join(peterstaylor, by = c("fyear", "gvkey"))  %>%
  rename(year = fyear) %>%
  select(K_int_Know, K_int, at, LPERMNO, year)

# CIKs are not unique -- they may map to multiple PERMNOs. May need to consolidate later.
topic_map <- topic_map_orig %>%
  left_join(linkt, by = c("CIK" = "cik"), relationship = "many-to-many") %>%
  mutate(naics4 = as.numeric(naics4)) %>%
  left_join(skilldata, by = c("naics4", "year")) %>%
  left_join(patent_ik, by = c("LPERMNO", "year")) %>%
  arrange(year, LPERMNO) %>%
  group_by(LPERMNO) %>%
  fill(xi_cumsum, .direction = "down") %>%
  ungroup() %>%
  mutate(xi_cumsum = ifelse(is.na(xi_cumsum),0, xi_cumsum)) %>%
  mutate(xi_yeartotal = ifelse(is.na(xi_yeartotal), 0, xi_yeartotal)) %>%
  left_join(compustat_pt, by = c("LPERMNO", "year")) %>%
  mutate(xir_cumsum = ifelse(is.na(xi_cumsum/at), 0, xi_cumsum/at)) %>%
  mutate(xir_total = ifelse(is.na(xi_yeartotal/at), 0, xi_yeartotal/at))  %>%
  group_by(CIK, year) %>%
  fill(K_int_Know, K_int, at, Skill, .direction = "down") %>%
  fill(K_int_Know, K_int, at, Skill, .direction = "up")  %>%
  group_by(CIK, year) %>%
  slice(1) %>%
  ungroup()
 
topic_map = understand_topics(topic_map, figfolder) %>%
  select(-K_int_Know, -K_int)

stoxmo = stoxmo_orig %>%
  mutate(date = ymd(date)) %>%
  mutate(y = year(date)) %>%
  mutate(ym = y*100 + month(date)) %>%
  left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y")) %>%
  filter(crit_ALL == 1) %>%
  left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) 

stoxwe = stoxwe_orig %>%
  mutate(y = yw%/%100) %>%
  inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "y")) %>%
  filter(crit_ALL == 1) %>%
  inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) %>%
  filter(y >= min(topic_map$year)) %>%
  left_join(ff3fw, by = "yw") %>%
  mutate(eretw = retw - RF) %>%
  drop_na(retw) %>%
  nest_by(PERMNO) %>%
  mutate(mod = list(lm(formula3ff <- eretw ~ Mkt.RF, data = data))) %>% 
  mutate(augmented = list(broom::augment(mod, data = data))) %>%
  select(-mod, -data) %>%
  unnest(augmented)

ndim_mb = 5
ndim_me = 5
ndim_ik = 3
ndim_kkc = 2

# CLEANING COMP_FUNDA AND PETERSTAYLOR
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
  inner_join(stoxwe, by = c("y", "CUSIP8")) %>%
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
  mutate(meg = ifelse(me < med_NYSE_me, 1, 2),
         mbg = case_when(mb < med_NYSE_mb30p ~ 1,
                         mb >= med_NYSE_mb30p & mb <= med_NYSE_mb70p ~ 2,
                         mb > med_NYSE_mb70p ~ 3)) %>%
  mutate(pf6_name = 10*meg+mbg)%>%
  group_by(y) %>%
  mutate(mb_5tile = ntile(mb, 5), me_5tile = ntile(me, 5)) %>%
  ungroup() %>%
  mutate(pf_number = 10*me_5tile + mb_5tile) %>%
  group_by(yw) %>%
  mutate(kk_bin = cut(topic_kk, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(1, 2, 3, 4, 5), include.lowest = TRUE)) %>%
  mutate(kk_bin = ifelse(is.na(kk_bin), -999, kk_bin)) %>%
  ungroup()

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

second_stage_ols = lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = first_stage2)
second_stage_wls = lm(formula3ff, data = first_stage2, weights = first_stage2$sigmae)
summary(second_stage_ols)
summary(second_stage_wls)

latex_table <- stargazer(second_stage_ols, second_stage_wls, title = "Regression Summary", align = TRUE, out = paste0(figfolder, "summary_table.tex"))

we_ret_bybin = compustat_sel %>%
  mutate(ntile_topic_kk = factor(ntile_topic_kk)) %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), sderet = sd(eretw, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ntile_topic_kk) %>%
  mutate(eret_accum = cumsum(eret)) %>% 
  yw2day()

qt_ret_bygroup = we_ret_bybin %>%
  group_by(ntile_topic_kk) %>%
  mutate(eret3ma = rollmean(eret, k=13, fill=NA, align='right')) 

ggplot(we_ret_bybin, aes(x = yw, y = eret, col = ntile_topic_kk)) +
  geom_line() +
  labs(x = "Year-month", y = "Asset-weighted weekly returns")
ggsave(paste0(figfolder, "awwr.jpg"), plot = last_plot(), dpi = 300)
ggplot(qt_ret_bygroup, aes(x = yw, y = eret3ma, col = ntile_topic_kk)) + geom_line()  +
  labs(x = "Year-month", y = "Asset-weighted weekly returns, 3MA") 
ggsave(paste0(figfolder, "awwr3ma.jpg"), plot = last_plot(), dpi = 300)
ggplot(we_ret_bybin , aes(x = yw, y = eret_accum, col = ntile_topic_kk)) +
  geom_line() +
  labs(x = "Year-month", y = "Asset-weighted accumulated weekly returns")
ggsave(paste0(figfolder, "awawr.jpg"), plot = last_plot(), dpi = 300)
ggplot(we_ret_bybin %>% filter(ntile_topic_kk %in% c(1,4)) , aes(x = yw, y = sderet, col = ntile_topic_kk)) + geom_line() + labs(x = "Year-month", y = "(Non-asset-weighted) weekly standard deviation of returns")
ggsave(paste0(figfolder, "wsdr.jpg"), plot = last_plot(), dpi = 300)

we_ret_bygroup = compustat_sel %>%
  mutate(max_topic = factor(max_topic)) %>%
  group_by(yw, max_topic) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), sderet = sd(eretw, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(max_topic) %>%
  mutate(eret_accum = cumsum(eret)) %>%
  ungroup() %>%
  yw2day()

qt_ret_bygroup = we_ret_bygroup %>%
  group_by(max_topic) %>%
  mutate(eret3ma = rollmean(eret, k=13, fill=NA, align='right')) 

ggplot(we_ret_bygroup, aes(x = yw, y = eret, col = max_topic)) +
  geom_line() +
  labs(x = "Year-month", y = "Asset-weighted weekly returns")
ggsave(paste0(figfolder, "awwr_byg.jpg"), plot = last_plot(), dpi = 300)
ggplot(qt_ret_bygroup, aes(x = yw, y = eret3ma, col = max_topic)) + geom_line()  +
  labs(x = "Year-month", y = "Asset-weighted weekly returns, 3MA") 
ggsave(paste0(figfolder, "awwr3ma_byg.jpg"), plot = last_plot(), dpi = 300)
ggplot(we_ret_bygroup, aes(x = yw, y = eret_accum, col = max_topic)) +
  geom_line() +
  labs(x = "Year-month", y = "Asset-weighted accumulated weekly returns")
ggsave(paste0(figfolder, "awawr_byg.jpg"), plot = last_plot(), dpi = 300)
ggplot(we_ret_bygroup , aes(x = yw, y = sderet, col = max_topic)) +
  geom_line() +
  labs(x = "Year-month", y = "(Non-asset-weighted) weekly standard deviation of returns")
ggsave(paste0(figfolder, "wsdr_byg.jpg"), plot = last_plot(), dpi = 300)