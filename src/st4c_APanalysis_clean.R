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
print_kurtosis = FALSE

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
  distinct() %>%
  drop_na(LPERMNO, cik) %>%
  arrange(LPERMNO, cik) %>%
  group_by(LPERMNO) %>%
  slice(1) %>%
  ungroup()
  

patent_ik <- patent_ik_orig %>%
  left_join(linkt, by = c("permno" = "LPERMNO")) %>%
  mutate(year = as.numeric(substr(filing_date,7,10)) ) %>%
  mutate(xi_real = coalesce(xi_real, 0)) %>%
  group_by(gvkey, year) %>%
  summarize(xi_yeartotal = sum(xi_real)) %>%
  ungroup() %>%
  group_by(gvkey) %>%
  mutate(xi_cumsum = cumsum(xi_yeartotal)) %>%
  ungroup()  

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

skilldata <- skilldata_orig %>%
  rename(naics4 = ind) %>%
  rename(year = YEAR) %>%
  full_join(expandgrid, by = c("naics4", "year")) %>%
  arrange(year, naics4) %>%
  group_by(naics4) %>%
  fill("Skill", .direction = "down") %>%
  ungroup()

expandgrid = expand.grid(YEAR = 2014:2022, naics4 = unique(skilldata %>% filter(year == 2013) %>% pull(naics4))) 

## Creating topic_map --------
compustat_pt = comp_funda2 %>%
  mutate(gvkey = as.integer(GVKEY)) %>%
  left_join(peterstaylor, by = c("fyear", "gvkey"))  %>%
  rename(year = fyear) %>%
  select(K_int_Know, K_int, at, gvkey, year, prcc_f, prcc_c, ppegt, csho, ceq, cusip, exchg)

# CIKs are not unique -- they may map to multiple PERMNOs. May need to consolidate later.
topic_map_unlabeled <- topic_map_orig %>%
  left_join(linkt, by = c("CIK" = "cik"), relationship = "many-to-many") %>%
  mutate(naics4 = as.numeric(naics4)) %>%
  left_join(skilldata, by = c("naics4", "year")) %>%
  left_join(patent_ik, by = c("gvkey", "year")) %>%
  arrange(year, gvkey) %>%
  group_by(gvkey) %>%
  fill(xi_cumsum, .direction = "down") %>%
  ungroup() %>%
  mutate(xi_cumsum = ifelse(is.na(xi_cumsum),0, xi_cumsum)) %>%
  mutate(xi_yeartotal = ifelse(is.na(xi_yeartotal), 0, xi_yeartotal)) %>%
  left_join(compustat_pt, by = c("gvkey", "year")) %>%
  mutate(xir_cumsum = ifelse(is.na(xi_cumsum/at), 0, xi_cumsum/at)) %>%
  mutate(xir_total = ifelse(is.na(xi_yeartotal/at), 0, xi_yeartotal/at))  %>%
  group_by(gvkey, year) %>%
  fill(K_int_Know, K_int, at, Skill, .direction = "down") %>%
  fill(K_int_Know, K_int, at, Skill, .direction = "up")  %>%
  group_by(gvkey, year) %>%
  slice(1) %>%
  ungroup()

k <- 0
labels <- vector("list", 2)
labels[[1]] <- paste("topic", k, sep = "_")
labels[[2]] <- as.character(k)
quantiles = 4

topic_map = understand_topics(topic_map_unlabeled, labels, quantiles, figfolder) 
 # %>%select(-K_int_Know, -K_int) #removed 2023'11'25

stoxmo = stoxmo_orig %>%
  mutate(date = ymd(date)) %>%
  mutate(y = year(date)) %>%
  mutate(ym = y*100 + month(date)) %>%
  left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y"), relationship = "many-to-many") %>%
  filter(crit_ALL == 1) %>%
  left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") 

stoxwe = stoxwe_orig %>%
  mutate(y = yw %/% 100) %>%
  inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "y"), relationship = "many-to-many") %>%
  filter(crit_ALL == 1) %>%
  inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") %>%
  filter(y >= min(topic_map$year)) %>%
  left_join(ff3fw, by = "yw") %>%
  mutate(eretw = retw - RF) %>%
  drop_na(retw) #%>%
  # nest_by(PERMNO) %>%
  # mutate(mod = list(lm(formula3ff <- eretw ~ Mkt.RF, data = data))) %>% 
  # mutate(augmented = list(broom::augment(mod, data = data))) %>%
  # select(-mod, -data) %>%
  # unnest(augmented)

if(print_kurtosis){
  kurtosis_graph(stoxda_orig, cequity_mapper, topic_map)
}

# CLEANING COMP_FUNDA AND PETERSTAYLOR
print("Creating stoxwe_with_pfs...")

stoxwe_with_pfs = attributePortfolios(stoxwe)
  
pf_ret = stoxwe_with_pfs %>%
  group_by(yw, pf36_name) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RF = mean(RF))

kkhml_ret = stoxwe_with_pfs %>%
  drop_na(topic_kk) %>%
  ungroup() %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
  pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = eret) %>%
  transmute(yw, kkhml = kk4-kk1)

eret_we = pf_ret %>%
  inner_join(kkhml_ret, by = c("yw")) %>%
  rename(eretw = eret)  %>%
  drop_na() %>%
  ungroup()

formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF

first_stage1 = eret_we %>%
  ungroup() %>%
  nest_by(pf36_name) %>%
  mutate(mod = list(lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = data))) %>%
  summarize(eretw = mean(data$eretw), t = length(data$eretw), tidy(mod)) 

get_sigmae = first_stage1 %>%
  mutate(sigmae = t*std.error^2) %>%
  filter(term == "(Intercept)") %>%
  select(pf36_name, sigmae) %>%
  drop_na()

first_stage2 = first_stage1 %>%
  select(-std.error, -statistic, -p.value) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  full_join(get_sigmae, by = c("pf36_name")) %>%
  drop_na()

second_stage_ols = lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = first_stage2)
second_stage_wls = lm(formula3ff <- eretw ~ kkhml + HML + SMB + Mkt.RF, data = first_stage2, weights = first_stage2$sigmae)

summary(second_stage_ols)
summary(second_stage_wls)

latex_table <- stargazer(second_stage_ols, second_stage_wls, title = "Regression Summary", align = TRUE, out = paste0(figfolder, "summary_table.tex"))

we_ret_bybin = stoxwe_with_pfs %>%
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

we_ret_bygroup = stoxwe_with_pfs %>%
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