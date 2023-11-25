## Loading packages -----------

library(tidyverse)
library(lubridate)
library(stringr)
library(stargazer)
library(rsq)
library(reshape2)
library(moments)

## Loading functions -----------
splitvar = "ntile_topic_kk"
modelname = "dicfullmc10thr10defnob40noa1_4t"
#NYSE codes:

#11: NYSE
#12: AMEX
#14: NASDAQ

## Loading patent_ik --------
print("Cleaning patent database...")
patent_ik <- read.csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/KPSS_2020_public.csv") %>%
  mutate(year = as.numeric(substr(filing_date,7,10)) ) %>%
  mutate(xi_real = coalesce(xi_real, 0)) %>%
  group_by(permno, year) %>%
  summarize(xi_total = sum(xi_real)) %>%
  ungroup() %>%
  group_by(permno) %>%
  mutate(xi_cumsum = cumsum(xi_total)) %>%
  ungroup() %>%
  rename(LPERMNO = permno)  

## Creating figfolder out of dicname --------
dir.create(file.path("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/", modelname), showWarnings = FALSE)
figfolder = paste0("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/", modelname, "/")

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
  distinct(cik, .keep_all = TRUE) %>%
  drop_na(cik)

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
  mutate(xi_cumsum = ifelse(is.na(xi_cumsum),0, xi_cumsum)) %>%
  mutate(xi_total = ifelse(is.na(xi_total), 0, xi_total)) %>%
  left_join(compustat_thin, by = c("LPERMNO", "year")) %>%
  mutate(xir_cumsum = ifelse(is.na(xi_cumsum/at), 0, xi_cumsum/at)) %>%
  mutate(xir_total = ifelse(is.na(xi_total/at), 0, xi_total/at))  
  
topic_map = understand_topics(topic_map, figfolder) %>%
  select(-K_int_Know, -K_int)

## Cleaning stocks data --------
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxda_p2005sampled_sh.Rdata") 
cequity_mapper <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/cequity_mapper.csv")

stox = stoxda_p2005sampled_sh %>%
  mutate(date = ymd(date)) %>%
  mutate(y = year(date)) %>%
  mutate(ym = y*100 + month(date)) %>%
  left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "year")) %>%
  filter(crit_ALL == 1) %>%
  left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) 

### Skewness analysis

stox_by_kk = stox %>%
  mutate(ym = y) %>%
  group_by(ym, PERMNO) %>%
  summarize(moment = kurtosis(RET), group = mean(max_topic)) %>%
  ungroup() %>%
  filter(group - floor(group) == 0) %>%
  mutate(group = factor(group)) %>%
  drop_na(group) 

stox_by_kk2 = stox_by_kk %>%
  group_by(group, ym) %>%
  summarize(moment = mean(moment, na.rm = TRUE))
  
ggplot(stox_by_kk2, aes(x = ym, y = moment, group = group, color = group)) +
  geom_line() +
  labs(x = "y", y = "moment") +
  scale_color_discrete(name = "group") +
  theme_minimal() 

stox_by_kkd = stox %>%
  group_by(ym, PERMNO) %>%
  summarize(RET = mean(RET), max_topic = mean(max_topic)) %>%
  ungroup() %>%
  mutate(ym = ym(ym)) %>%
  drop_na(RET)   %>%
  group_by(ym, max_topic) %>%
  summarize(moment = kurtosis(RET)) %>%
  ungroup() %>%
  rename(group = max_topic)%>%
  filter(group - floor(group) == 0) %>%
  mutate(ym = factor(ym)) %>%
  drop_na(group)

stox_by_kk2 = stox_by_kkd %>%
  group_by(group, ym) %>%
  summarize(moment = mean(moment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group = factor(group))

ggplot(stox_by_kk2, aes(x = ym, y = moment, group = group, color = group)) +
  geom_line() +
  labs(x = "y", y = "moment") +
  scale_color_discrete(name = "group") +
  theme_minimal()  

bollerslev <- function(df){
  df = df %>%
    select(PERMNO, RET, date, ym) %>%
    group_by(date, PERMNO) %>%
    summarize(RET = mean(RET), ym = mean(ym)) %>%
    ungroup() %>%
    nest_by(ym) %>%
    mutate(wow = list(semicov(data))) %>%
    summarize(Pbar = wow$Pbar, Nbar = wow$Nbar, Mbar = wow$Mbar) %>%
    mutate(NPbar = Nbar+Pbar) %>%
    select(-Nbar, -Pbar) %>%
    mutate(Cbar = NPbar + Mbar) %>%
    mutate(ym = as.Date(as.character(100*ym + 1), format = "%Y%m%d"))
  
  ggplot(df, aes(x = ym)) +
    geom_line(aes(y = Cbar), color = "blue", linetype = "dotted") +
    geom_line(aes(y = NPbar), color = "green") +
    geom_line(aes(y = Mbar), color = "red") +
    labs(x = "Date", y = "Values") +
    ggtitle("Line Plot of Cbar, NPbar, and Mbar") +
    theme_minimal()
}  

average_jk <- function(matrix) {
  result <- matrix(FALSE, nrow = nrow(matrix), ncol = nrow(matrix))
  diag(result) <- TRUE
  mask = as.numeric(!(result | is.na(matrix)))
  average = sum(mask*matrix, na.rm = TRUE) / sum(mask, na.rm = TRUE)
  return(average)
}

semicov <- function(stox){
  stox  = stox %>%
  pivot_wider(names_from = date, values_from = RET) %>%
    column_to_rownames(var = "PERMNO")
  p = as.matrix(stox)
  p[p<0] <- 0
  n = as.matrix(stox)
  n[n>0] <- 0
  P = p %*% t(p)
  N = n %*% t(n)
  M = p %*% t(n) + n %*% t(p) 
  Pbar = average_jk(P)
  Nbar = average_jk(N)
  Mbar = average_jk(M)
  return(list(Pbar = Pbar,
              Nbar = Nbar, 
              Mbar = Mbar))
}

# stox_aw = stox %>%
#   filter(!is.na(.resid)) %>%
#   group_by(yw, ntile_topic_kk) %>%
#   summarize(aw_residuals = sum(.resid * at, na.rm = TRUE)/sum(at, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(ntile_topic_kk) %>%
#   mutate(cumsum_awr = cumsum(aw_residuals)) %>%
#   ungroup() %>%
#   yw2day()
#   
# ggplot(stox_aw, aes(x = yw, y = cumsum_awr, color = as.factor(ntile_topic_kk))) +
#   geom_line() +
#   labs(x = "Year-Week", y = "Value", color = "Ntile Topic KK") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 
# stox = stoxwe_post2005short %>%
#   mutate(y = yw%/%100) %>%
#   inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "year")) %>%
#   filter(crit_ALL == 1) %>%
#   inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) %>%
#   filter(y >= min(topic_map$year)) %>%
#   left_join(ff3f, by = "ym") %>%
#   mutate(eretm = retm - RF) %>%
#   drop_na(RET) %>%
#   nest_by(PERMNO) %>%
#   mutate(mod = list(lm(formula3ff <- retm ~ RF, data = data))) %>% #mutate(mod = list(lm(formula3ff <- eretm ~ Mkt.RF, data = data))) %>%
#   mutate(augmented = list(broom::augment(mod, data = data))) %>%
#   select(-mod, -data) %>%
#   unnest(augmented)





# cormat = stox %>%
#   filter(y == 2019) %>%
#   arrange(PERMNO) %>%
#   select(PERMNO, ym, .resid) %>%
#   group_by(PERMNO, ym) %>%
#   summarize(.resid = mean(.resid)) %>%
#   pivot_wider(names_from = PERMNO, values_from = .resid, values_fill = NA) %>%
#   select(-ym) %>%
#   cor(., use = "pairwise.complete.obs")


# cormat = stox %>%
#   filter(y == 2019) %>%
#   arrange(PERMNO) %>%
#   select(PERMNO, ym, retm) %>%
#   group_by(PERMNO, ym) %>%
#   summarize(retm = mean(retm)) %>%
#   pivot_wider(names_from = PERMNO, values_from = retm, values_fill = NA) %>%
#   select(-ym) %>%
#   cor(., use = "pairwise.complete.obs")
# diag(cormat) <- 0
# mean(cormat, na.rm = TRUE)



# Sanity check: sum(as.numeric(colnames(resmat))-kk_mask$PERMNO)

ndim_mb = 5
ndim_me = 5
ndim_ik = 3
ndim_kkc = 2

#' 
#' The following calculations and categorizations are made:
#' 
#' - Market-to-book ratios (`mb`) are found using the formula: `mb = (csho * prcc_f) / ceq`, where `csho` represents common shares outstanding, `prcc_f` represents the closing price of the stock, and `ceq` represents common equity.
#' - Market equity is calculated as the product of `csho` and `prcc_f`.
#' - Stocks are divided into `r ndim_mb` groups based on their market-to-book ratio.
#' - Stocks are also divided into `r ndim_me` groups based on their market equity.
#' - Stocks are categorized into `r ndim_kkc` groups based on their membership in the knowledge-capital-heavy cluster.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

#' 
#' - Returns for each portfolio, which are divided into two dimensions, are calculated as the asset-weighted sum of returns using `pf_ret`.
#' - Returns for each portfolio, which are divided into three dimensions, are calculated as the asset-weighted sum of returns using `ff3_wKKc_ret`.
#' - SMB (Small Minus Big) is the difference between the simple average of the returns on the three small stock portfolios and the average of the three big stock portfolios.
#' - HML (High Minus Low) is the difference between the simple average of the returns on the two low market-to-book (MB) portfolios (high book-to-market ratio) and the two high MB portfolios (low book-to-market ratio).
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## FIND RETURNS FOR 2F PORTFOLIO: MB, ME

pf_ret = compustat_sel %>%
  group_by(yw, pf_number) %>%
  summarize(ret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RF = mean(RF))

# ff3_hml = pf_ret %>%
#   group_by(ym, mb_ntile) %>% # Mean of all MB portfolios?
#   summarize(ret = mean(ret)) %>%
#   pivot_wider(names_from = mb_ntile, names_prefix = "mb", values_from = ret) %>%
#   transmute(ym, hml = mb1-mb5)
# 
# ff3_smb = pf_ret %>%
#   group_by(ym, me_ntile) %>%
#   summarize(ret = mean(ret)) %>%
#   pivot_wider(names_from = me_ntile, names_prefix = "me", values_from = ret) %>%
#   transmute(ym, smb = me1-me5) 

# Why am I redoing this? Because instead of averaging out returns of several portfolios with the same KK, I am averaging out returns for all stocks with same KK.
splitvar = "ntile_topic_kk"

kkhml_ret = compustat_sel %>%
  drop_na(topic_kk) %>%
  ungroup() %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(ret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
  pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = ret) %>%
  transmute(yw, kkhml = kk4-kk1)


#' 
#' ## Cross-Sectional Regression
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(broom)

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

library(nlme)
second_stage_wls = lm(formula3ff, data = first_stage2, weights = first_stage2$sigmae)
summary(second_stage_ols)
summary(second_stage_wls)

##### DOG 

#' 
#' ## Second stage regressions
#' 
#' Considering knowledge capital risk a non-traded factor, I run the regression in two stages. In the first one, I obtain estimates of betas from the time-series regression:
#' 
#' $$
#' \bar{ER}_t = a + \beta_{SMB, t} SMB_t + \beta_{HML, t} HML_t + \beta_{KKHML, t} KKHML_T + \beta_{ERM, t} (RM_t - RF_t) +\varepsilon_t
#' $$
#' 
#' In the second stage, I obtain a cross-sectional regression of average returns on betas. The betas here are the explanatory variables. Since the regression residuals are cross-sectionally correlated, I also run WLS, using as weights the variance of residuals for each portfolio. 
#' 
#' $$
#' \bar{R}_t + \widehat{B} \lambda + \alpha
#' $$
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(second_stage_ols)
summary(second_stage_wls)
latex_table <- stargazer(second_stage_ols, second_stage_wls, title = "Regression Summary", align = TRUE, out = paste0(figfolder, "summary_table.tex"))


#' 
#' 
#' ## First regressions
#' 
#' Stocks were grouped monthly into 12 portfolios based on three dimensions: book-to-market ratio (L, M, H), market value (S, B), and knowledge risk intensity (H, L). Book-to-market was divided into terciles (L, M, H), while market value was divided using the median (S, B). High knowledge risk intensity was determined based on cluster 1 in an LDA analysis, with other firms categorized as 0.
#' 
#' $$
#' R_{i,t} - R_{f,t} = \beta_{1,i} HML_{t} + \beta_{2,i} SMB_{t} + \beta_{3,i} KKHML_{t} + \epsilon_{i,t}
#' $$
#' 
#' Each of the tables below include coefficients for 12 stock portfolios formed on size (S/B), book-to-market equity (H/M/L), and KK loading (H/L).
#' 
#' ## Monthly returns by group
#' 
#' The graph below illustrates the monthly returns of high- and low-knowledge firms, presented as the mean asset-weighted returns. Additionally, the subsequent graph displays the standard deviation of monthly returns for the respective groups, providing further insight into their performance. It is clear that high-knowledge firms have been more volatile throughout the times, with the notable exception of March 2020.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lubridate)
we_ret_bybin = compustat_sel %>%
  mutate(ntile_topic_kk = factor(ntile_topic_kk)) %>%
  group_by(yw, ntile_topic_kk) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), sderet = sd(eretw, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ntile_topic_kk) %>%
  mutate(eret_accum = cumsum(eret)) %>% 
  yw2day()

library(zoo)
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
#' 
#' 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lubridate)
we_ret_bygroup = compustat_sel %>%
  mutate(max_topic = factor(max_topic)) %>%
  group_by(yw, max_topic) %>%
  summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), sderet = sd(eretw, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(max_topic) %>%
  mutate(eret_accum = cumsum(eret)) %>%
  ungroup() %>%
  yw2day()

library(zoo)
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