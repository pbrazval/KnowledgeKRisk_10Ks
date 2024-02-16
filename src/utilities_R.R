# Dog <editor-fold>
# Your code here
add_hi_tech_column <- function(df) {
  hi_tech_sic <- c(283, 357, 366, 367, 382, 384, 737)
  df$sic3 = as.numeric(substr(as.character(df$sic), 1, 3))
  df$hi_tech <- ifelse(df$sic3 %in% hi_tech_sic, 1, 0)
  
  return(df)
}

amazon_graph <- function(amazon_nov01_short, figfolder){
  amazon_nov01_short <- amazon_nov01_short %>%
    mutate(Date = as.Date(Date))
  
  # Finding the 'nasdaq' and 'amazon' values for the specific date
  specific_date_values <- amazon_nov01_short %>%
    filter(Date == as.Date("2001-11-13")) %>%
    select(nasdaq, amazon)
  
  # Divide all the values in 'nasdaq' and 'amazon' by these specific values
  amazon_nov01_short <- amazon_nov01_short %>%
    mutate(nasdaq = 100*nasdaq / specific_date_values$nasdaq,
           amazon = 100*amazon / specific_date_values$amazon)
  ggplot(data = amazon_nov01_short, aes(x = Date)) +
    geom_line(aes(y = nasdaq, color = "NASDAQ", group = 1)) +
    geom_vline(xintercept = as.Date("2001-11-13"), linetype = "dashed", color = "black") +
    geom_line(aes(y = amazon, color = "Amazon", group = 1)) +
    labs(title = "NASDAQ vs Amazon Stock Prices in November 2001 (11/13/01 = 100)", 
         x = "Date", 
         y = "Stock Price") +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),   # X-axis labels
          axis.text.y = element_text(size = 14),                          # Y-axis labels
          legend.title = element_blank(),                                 # Legend title
          legend.text = element_text(size = 14),                          # Legend labels
          plot.title = element_text(size = 14, hjust = 0.5),              # Plot title
          legend.position = "bottom")  
  ggsave(paste0(figfolder, "amazon_nov01.png"), plot = last_plot(), dpi = 600, width = 10, height = 6, units = "in")
}

attributePortfoliosWe <- function(stoxwe){
  stoxwe_add = stoxwe %>%
    mutate(fiscalyear = sapply(yw, getFiscalYearWe)) %>%
    mutate(mb = (csho*prcc_f)/ceq, 
           me = csho*prcc_f,
           kk_share = K_int_Know/ppegt,
           CUSIP8 = str_sub(cusip, 1, -2))
  
  pfs = stoxwe_add %>%
    filter(yw%%100 == 26)  %>%
    select(-cusip) %>%
    group_by(y) %>%
    drop_na(me, mb) %>%
    mutate(med_NYSE_me = median(me[exchg == 11], na.rm = TRUE)) %>%
    mutate(med_NYSE_mb70p = quantile(mb[exchg == 11], prob = 0.7, na.rm = TRUE)) %>%
    mutate(med_NYSE_mb30p = quantile(mb[exchg == 11], prob = 0.3, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(me_group = ifelse(me < med_NYSE_me, 1, 2),
           mb_group = case_when(mb < med_NYSE_mb30p ~ 1,
                                mb >= med_NYSE_mb30p & mb <= med_NYSE_mb70p ~ 2,
                                mb > med_NYSE_mb70p ~ 3)) %>%
    select(-med_NYSE_me, -med_NYSE_mb30p, -med_NYSE_mb70p) %>%
    mutate(pf6_name = 10*me_group+mb_group)%>%
    group_by(y) %>%
    mutate(me_3tile = ntile(me, 3)) %>%
    mutate(mb_3tile = ntile(mb, 3)) %>%
    mutate(pf25_name = 10*ntile(me, 5) + ntile(mb, 5)) %>%
    mutate(pf24_name = 100*ntile_topic_kk + 10*me_group + mb_group) %>%
    mutate(pf36_name = 100*ntile_topic_kk + 10*ntile(me, 3) + ntile(mb, 3)) %>% 
    group_by(me_3tile, mb_3tile) %>%
    mutate(kkr_3tile_inner = ntile(topic_kk, 3)) %>%
    ungroup() %>%
    mutate(pf27_name = 100*kkr_3tile_inner + 10*me_3tile + mb_3tile) %>%
    ungroup() %>%
    select(gvkey.x, pf36_name, pf6_name, pf25_name, pf24_name, pf27_name, fiscalyear) %>%
    rename(gvkey = gvkey.x) %>%
    mutate(fiscalyear = fiscalyear+1)
  
  stoxwe_add2 = stoxwe_add %>%
    rename(gvkey = gvkey.x) %>%
    inner_join(pfs, by = c("fiscalyear", "gvkey"))
  
  return(stoxwe_add2)
}

attributePortfoliosMo <- function(stoxmo){
  stoxmo_add = stoxmo %>%
    mutate(fiscalyear = sapply(ym, getFiscalYearMo)) %>%
    mutate(mb = (csho*prcc_f)/ceq, 
           me = csho*prcc_f,
           kk_share = K_int_Know/ppegt,
           CUSIP8 = str_sub(cusip, 1, -2))
  
  pfs = stoxmo_add %>%
    filter(ym%%100 == 6)  %>%
    select(-cusip) %>%
    group_by(y) %>%
    drop_na(me, mb) %>%
    mutate(med_NYSE_me = median(me[exchg == 11], na.rm = TRUE)) %>%
    mutate(med_NYSE_mb70p = quantile(mb[exchg == 11], prob = 0.7, na.rm = TRUE)) %>%
    mutate(med_NYSE_mb30p = quantile(mb[exchg == 11], prob = 0.3, na.rm = TRUE)) %>%
    ungroup() 
  
  pfs = pfs %>%
    mutate(me_group = ifelse(me < med_NYSE_me, 1, 2),
           mb_group = case_when(mb < med_NYSE_mb30p ~ 1,
                                mb >= med_NYSE_mb30p & mb <= med_NYSE_mb70p ~ 2,
                                mb > med_NYSE_mb70p ~ 3)) %>%
    select(-med_NYSE_me, -med_NYSE_mb30p, -med_NYSE_mb70p) %>%
    mutate(pf6_name = 10*me_group+mb_group)%>%
    group_by(y) %>%
    mutate(me_3tile = ntile(me, 3)) %>%
    mutate(mb_3tile = ntile(mb, 3)) %>%
    mutate(pf25_name = 10*ntile(me, 5) + ntile(mb, 5)) %>%
    mutate(pf36_name = 100*ntile_topic_kk + 10*me_3tile + mb_3tile) %>% 
    group_by(me_3tile, mb_3tile) %>%
    mutate(kkr_3tile_inner = ntile(topic_kk, 3)) %>%
    ungroup() %>%
    mutate(pf27_name = 100*kkr_3tile_inner + 10*me_3tile + mb_3tile) %>%
    ungroup() %>%
    select(gvkey, pf36_name, pf6_name, pf25_name, pf27_name, fiscalyear) %>%
    mutate(fiscalyear = fiscalyear+1)
  
  stoxmo_add = stoxmo_add %>%
    inner_join(pfs, by = c("fiscalyear", "gvkey"))
  
  return(stoxmo_add)
}

average_jk <- function(matrix) {
  result <- matrix(FALSE, nrow = nrow(matrix), ncol = nrow(matrix))
  diag(result) <- TRUE
  mask = as.numeric(!(result | is.na(matrix)))
  average = sum(mask*matrix, na.rm = TRUE) / sum(mask, na.rm = TRUE)
  return(average)
}

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
    theme_minimal()+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
}  

cor_comparison = function(stox, year){
  print("Comparing correlations between subsets...")
  cormat = stox %>% #filter(y == year) %>%
    arrange(PERMNO) %>%
    select(PERMNO, yw, .resid) %>%
    group_by(PERMNO, yw) %>%
    summarize(.resid = mean(.resid)) %>%
    pivot_wider(names_from = PERMNO, values_from = .resid, values_fill = NA) %>%
    select(-yw) %>%
    cor(., use = "pairwise.complete.obs")
  dummy_mask <- matrix(1, nrow = nrow(cormat), ncol = ncol(cormat))
  
  full1 = mean_cor(cormat, dummy_mask)
  full2 = mean_cor(abs(cormat), dummy_mask)
  
  kk_mask = stox %>%
    mutate(kk_uptile = ntile_topic_kk == 4) %>% #filter(y == year) %>%
    arrange(PERMNO) %>%
    group_by(PERMNO) %>%
    summarize(kk_uptile = prod(kk_uptile, na.rm = TRUE)) %>%
    ungroup() %>%
    select(PERMNO, kk_uptile) 
  full_mask = as.matrix(kk_mask$kk_uptile %*% t(kk_mask$kk_uptile))
  
  subset1 = mean_cor(cormat, full_mask)
  subset2 = mean_cor(abs(cormat), full_mask)
  df <- data.frame(matrix(c(full1, full2, subset1, subset2), nrow = 2, ncol = 2))
  rownames(df) = c("True", "Abs")
  colnames(df) = c("Full", "Subset")
  return(df)
}

create_ind12 <- function(df){
  df = df %>% add_column(ind12 = NA)
  seq1 = c(seq(0100,0999),seq(2000,2399),seq(2700,2749),seq(2770,2799),seq(3100,3199),seq(3940,3989))
  seq2 = c(seq(2500,2519),seq(2590,2599),seq(3630,3659),seq(3710,3711),seq(3714,3714),seq(3716,3716),seq(3750,3751),seq(3792,3792),seq(3900,3939),seq(3990-3999))
  seq3 = c(seq(2520,2589),seq(2600,2699),seq(2750,2769),seq(3000,3099),seq(3200,3569),seq(3580,3629),seq(3700,3709),seq(3712,3713),seq(3715,3715),seq(3717,3749),seq(3752,3791),seq(3793,3799),seq(3830,3839),seq(3860,3899))
  seq4 = c(seq(1200,1399), seq(2900,2999))
  seq5 = c(seq(2800,2829), seq(2840,2899))
  seq6 = c(seq(3570,3579),seq(3660,3692),seq(3694,3699),seq(3810,3829),seq(7370,7379))
  seq7 = c(seq(4800,4899))
  seq8 = c(seq(4900,4949))
  seq9 = c(seq(5000,5999),seq(7200,7299),seq(7600,7699))
  seq10 = c(seq(2830,2839),seq(3693,3693),seq(3840,3859),seq(8000,8099))
  seq11 = seq(6000,6999)
  
  df$ind12[df$sic %in% seq1] = 1
  df$ind12[df$sic %in% seq2] = 2
  df$ind12[df$sic %in% seq3] = 3
  df$ind12[df$sic %in% seq4] = 4
  df$ind12[df$sic %in% seq5] = 5
  df$ind12[df$sic %in% seq6] = 6
  df$ind12[df$sic %in% seq7] = 7
  df$ind12[df$sic %in% seq8] = 8
  df$ind12[df$sic %in% seq9] = 9
  df$ind12[df$sic %in% seq10] = 10
  df$ind12[df$sic %in% seq11] = 11
  df$ind12[is.na(df$ind12)] = 12
  invisible(df)
}

cleanff <- function(ffdf){
  outdf = ffdf %>%
    rename(ym = X) %>%
    mutate_at(vars(-ym), ~ log(1+./100))
  invisible(outdf)
}

cleanffw <- function(ffdf){
  outdf = ffdf %>%
    mutate(date = ymd(X)) %>%
    mutate(yw = year(date)*100+isoweek(date)) %>%
    select(-X, -date) %>%
    mutate_at(vars(-yw), ~ log(1+./100))
  invisible(outdf)
}

cleanff_all <- function(){
  ff3fm <- ff3fm_orig %>% cleanff() %>% drop_na()
  
  ff5fm <- ff5fm_orig %>% cleanff() %>% drop_na()
  
  ff3fw <- ff3fw_orig %>% cleanffw() %>% drop_na()
  
  ff5fw <- ff5fw_orig %>% cleanffw() %>% group_by(yw) %>% summarize(across(.cols = everything(), sum)) %>% ungroup()
  return(list(ff3fm, ff5fm, ff3fw, ff5fw))
}

clean_dataframes <- function() {
  linkt <- clean_linkt_orig(linkt_orig)
  patent_ik <- clean_patent_ik_orig(patent_ik_orig, linkt)
  ff3fm <- ff3fm_orig %>% cleanff() %>% drop_na()
  ff5fm <- ff5fm_orig %>% cleanff() %>% drop_na()
  ff3fw <- ff3fw_orig %>% cleanffw() %>% drop_na()
  ff5fw <- ff5fw_orig %>% cleanffw() %>% group_by(yw) %>%
    summarize(across(.cols = everything(), sum)) %>%
    ungroup()
  skilldata <- clean_skilldata(skilldata_orig)
}

clean_compustat <- function(comp_funda2, peterstaylor){
  compustat_pt = comp_funda2 %>%
    mutate(gvkey = as.integer(GVKEY)) %>%
    left_join(peterstaylor, by = c("fyear", "gvkey"))  %>%
    rename(year = fyear) %>%
    select(K_int_Know, K_int, at, gvkey, year, prcc_f, prcc_c, ppegt, csho, ceq, cusip, exchg)
  return(compustat_pt)
}

clean_linkt_orig <- function(linkt_orig){
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
  return(linkt)
}

clean_patent_ik_orig <- function(patent_ik_orig, linkt){
  patent_ik <- patent_ik_orig %>%
    left_join(linkt, by = c("permno" = "LPERMNO")) %>%
    mutate(year = as.numeric(substr(issue_date,7,10)) ) %>%
    mutate(xi_real = coalesce(xi_real, 0)) %>%
    group_by(gvkey, year) %>%
    summarize(xi_yeartotal = sum(xi_real)) %>%
    ungroup() %>%
    group_by(gvkey) %>%
    mutate(xi_cumsum = cumsum(xi_yeartotal)) %>%
    ungroup()  
  return(patent_ik)
}

clean_skilldata <- function(skilldata_orig){
  expandgrid = expand.grid(YEAR = 2014:2022, naics4 = unique(skilldata_orig %>% filter(YEAR == 2013) %>% pull(ind))) %>%
    rename(year = YEAR) 
  
  skilldata <- skilldata_orig %>%
    rename(naics4 = ind) %>%
    rename(year = YEAR) %>%
    full_join(expandgrid, by = c("naics4", "year")) %>%
    arrange(year, naics4) %>%
    group_by(naics4) %>%
    fill("Skill", .direction = "down") %>%
    ungroup()
  return(skilldata)
}

clean_stoxmo <- function(stoxmo_orig){
  stoxmo = stoxmo_orig %>%
    mutate(retm = as.numeric(RET)) %>%
    select(date, PERMNO, retm) %>%
    mutate(date = ymd(date)) %>%
    mutate(y = year(date)) %>%
    mutate(ym = y*100 + month(date)) %>%
    left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y"), relationship = "many-to-many") %>%
    filter(crit_ALL == 1) %>%
    left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(y >= min(topic_map$year)) %>%
    left_join(ff3fm, by = "ym") %>%
    drop_na(retm) %>%
    mutate(eretm = retm - RF) 
  return(stoxmo)
}

clean_stoxmo_ff5 <- function(stoxmo_orig){
  
  stoxmo = stoxmo_orig %>%
    mutate(retm = as.numeric(RET)) %>%
    select(date, PERMNO, retm) %>%
    mutate(date = ymd(date)) %>%
    mutate(y = year(date)) %>%
    mutate(ym = y*100 + month(date)) %>%
    left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y"), relationship = "many-to-many") %>%
    filter(crit_ALL == 1) %>%
    left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(y >= min(topic_map$year)) %>%
    left_join(ff5fm, by = "ym") %>%
    drop_na(retm) %>%
    mutate(eretm = retm - RF) 
  return(stoxmo)
}

create_eret_we_panel <- function(stoxwe_orig, cequity_mapper, topic_map, ffmat, pfn){
  stoxwe = stoxwe_orig %>%
    mutate(y = yw %/% 100) %>%
    inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(crit_ALL == 1) %>%
    inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(y >= min(topic_map$year)) %>%
    left_join(ffmat, by = "yw") %>%
    mutate(eretw = retw - RF) %>%
    drop_na(retw)
  
  stoxwe_with_pfs = attributePortfoliosWe(stoxwe)
  pf_ret = stoxwe_with_pfs %>%
    drop_na(eretw, me) %>%
    group_by(yw, !!sym(pfn)) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RF = mean(RF))
  
  kkrhml_ret = stoxwe_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(yw, ntile_topic_kk) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = eret) %>%
    transmute(yw, kkrhml = kk4-kk1)
  
  kkpthml_ret = stoxwe_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(yw, kkpt_ntile) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = kkpt_ntile, names_prefix = "kk", values_from = eret) %>%
    transmute(yw, kkpthml = kk4-kk1)
  
  eret_we = pf_ret %>%
    inner_join(kkrhml_ret, by = c("yw")) %>% #inner_join(kkpthml_ret, by = c("yw")) %>%
    rename(eretw = eret)  %>%
    drop_na() %>%
    ungroup()
  return(list(eret_we, stoxwe_with_pfs))
}

create_eret_mo_panel <- function(stoxwe_orig, cequity_mapper, topic_map){
  stoxmo = clean_stoxmo(stoxmo_orig)
  
  stoxmo_with_pfs = attributePortfoliosMo(stoxmo)
  
  pf_ret = stoxmo_with_pfs %>%
    drop_na(eretm, me) %>%
    group_by(ym, pf36_name) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RF = mean(RF))
  
  kkrhml_ret = stoxmo_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(ym, ntile_topic_kk) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = eret) %>%
    transmute(ym, kkrhml = kk4-kk1)
  
  kkpthml_ret = stoxmo_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(ym, kkpt_ntile) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = kkpt_ntile, names_prefix = "kk", values_from = eret) %>%
    transmute(ym, kkpthml = kk4-kk1)
  
  eret_mo = pf_ret %>%
    inner_join(kkrhml_ret, by = c("ym")) %>% #inner_join(kkpthml_ret, by = c("ym")) %>%
    rename(eretm = eret)  %>%
    drop_na() %>%
    ungroup()
  return(list(eret_mo, stoxmo_with_pfs))
}

create_eret_we_panel_ff5 <- function(stoxwe_orig, cequity_mapper, topic_map, pfn){
  stoxwe = stoxwe_orig %>%
    mutate(y = yw %/% 100) %>%
    inner_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(crit_ALL == 1) %>%
    inner_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year"), relationship = "many-to-many") %>%
    filter(y >= min(topic_map$year)) %>%
    left_join(ff5fw, by = "yw") %>%
    mutate(eretw = retw - RF) %>%
    drop_na(retw)
  
  stoxwe_with_pfs = attributePortfoliosWe(stoxwe)
  
  pf_ret = stoxwe_with_pfs %>%
    drop_na(eretw, me) %>%
    group_by(yw, !!sym(pfn)) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), CMA = mean(CMA), RMW = mean(HML), RF = mean(RF))
  
  kkrhml_ret = stoxwe_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(yw, ntile_topic_kk) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = eret) %>%
    transmute(yw, kkrhml = kk4-kk1)
  
  kkpthml_ret = stoxwe_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(yw, kkpt_ntile) %>%
    summarize(eret = sum(eretw*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = kkpt_ntile, names_prefix = "kk", values_from = eret) %>%
    transmute(yw, kkpthml = kk4-kk1)
  
  eret_we = pf_ret %>%
    inner_join(kkrhml_ret, by = c("yw")) %>% #inner_join(kkpthml_ret, by = c("yw")) %>%
    rename(eretw = eret)  %>%
    drop_na() %>%
    ungroup()
  return(list(eret_we, stoxwe_with_pfs))
}

create_eret_mo_panel_ff5 <- function(stoxwe_orig, cequity_mapper, topic_map, pfn){
  stoxmo = clean_stoxmo_ff5(stoxmo_orig)
  
  stoxmo_with_pfs = attributePortfoliosMo(stoxmo)
  
  pf_ret = stoxmo_with_pfs %>%
    drop_na(eretm, me) %>%
    group_by(ym, !!sym(pfn)) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE), Mkt.RF = mean(Mkt.RF), SMB = mean(SMB), HML = mean(HML), RMW = mean(RMW), CMA = mean(CMA), RF = mean(RF))
  
  kkrhml_ret = stoxmo_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(ym, ntile_topic_kk) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = ntile_topic_kk, names_prefix = "kk", values_from = eret) %>%
    transmute(ym, kkrhml = kk4-kk1)
  
  kkpthml_ret = stoxmo_with_pfs %>%
    drop_na(topic_kk) %>%
    ungroup() %>%
    group_by(ym, kkpt_ntile) %>%
    summarize(eret = sum(eretm*me, na.rm = TRUE)/sum(me, na.rm = TRUE)) %>%
    pivot_wider(names_from = kkpt_ntile, names_prefix = "kk", values_from = eret) %>%
    transmute(ym, kkpthml = kk4-kk1)
  
  eret_mo = pf_ret %>%
    inner_join(kkrhml_ret, by = c("ym")) %>% #inner_join(kkpthml_ret, by = c("ym")) %>%
    rename(eretm = eret)  %>%
    drop_na() %>%
    ungroup()
  return(list(eret_mo, stoxmo_with_pfs))
}

create_topic_plots <- function(df, figfolder) {
  # Calculate the average topic intensity for each year
  df = df %>% select(year, starts_with("topic_"))
  avg_df <- aggregate(. ~ year, data = df, FUN = mean)
  
  # Gather the topic columns into long format
  long_df <- tidyr::gather(avg_df, topic, intensity, starts_with("topic_"))
  
  # Plot the line plots
  ggplot(long_df, aes(x = year, y = intensity, color = topic)) +
    geom_line() +
    labs(x = "Year", y = "Topic Intensity", title = "Mean Topic Intensity by Year") +
    scale_color_discrete(name = "Topic") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(figfolder, "mean_tiy.jpg"), plot = last_plot(), dpi = 600)
}

create_topic_map_unlabeled <- function(topic_map_orig, linkt, skilldata, patent_ik, compustat_pt){
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
    mutate(xi_cumsum = ifelse(is.na(xi_cumsum), 0, xi_cumsum)) %>%
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
}

descriptive_statistics <- function(topic_map, figfolder){
  selection = topic_map %>%
    select(starts_with('topic'), Skill, xi_yeartotal, xi_cumsum, xir_cumsum, xir_total, K_int, K_int_Know, at)
  descstat = describe(selection) %>%
    select(mean, median, sd, min, max, n) %>%
    mutate(across(c(mean, median, sd, min, max), round, digits = 2)) %>%
    rename(Mean = mean) %>%
    rename(Median = median) %>%
    rename(`St. dev.` = sd) %>%
    rename(Min = min) %>%
    rename(Max = max) %>%
    mutate(across(everything(), ~ifelse(. > 10, (. %/% .1)/10, .))) %>%
    mutate(across(everything(), ~ifelse(. > 100, . %/% 1, .)))
  
  rownames(descstat)[rownames(descstat) == "xi_yeartotal"] <- "Annual patent value added"
  rownames(descstat)[rownames(descstat) == "xi_cumsum"] <- "Total patent value accumulated (millions)"
  rownames(descstat)[rownames(descstat) == "xir_cumsum"] <- "Total patent value / total assets"
  rownames(descstat)[rownames(descstat) == "xir_total"] <- "Annual patent value / total assets"
  rownames(descstat)[rownames(descstat) == "at"] <- "Total assets (millions)"
  rownames(descstat)[rownames(descstat) == "K_int"] <- "Intangible capital (millions)"
  rownames(descstat)[rownames(descstat) == "K_int_Know"] <- "Knowledge capital (millions)"
  
  stargazer(descstat, digits = 2, digits.extra = 0, title = "Descriptive Statistics", align = FALSE, header = FALSE, summary = FALSE, rownames = TRUE, label = "tab:descstat", out = paste0(figfolder, "desc_stat", ".tex")) 
}

filecounter <- function(textfolder){
  path <- "/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/1A files/"
  year_file_counts <- vector("numeric", length = 2022-2006+1)
  names(year_file_counts) <- 2006:2022
  for (year in 2006:2022) {
    folder_path <- paste0(path, "/", year)
    if (dir.exists(folder_path)) {
      subfolder_names <- c("Q1", "Q2", "Q3", "Q4")
      year_file_count <- 0
      
      for (subfolder in subfolder_names) {
        subfolder_path <- file.path(folder_path, subfolder)
        if (dir.exists(subfolder_path)) {
          year_file_count <- year_file_count + length(list.files(subfolder_path))
        }
      }
      
      year_file_counts[as.character(year)] <- year_file_count
    }
  }
  
  lemmat_counts = read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/descriptive/lemmat_counts.csv")
  lemmat_counts$Total_1As = as.vector(year_file_counts)
  lemmat_counts = lemmat_counts %>%
    rename(Filtered = Count) %>%
    select(Year, Total_1As, Filtered)
  tex_table <- stargazer(lemmat_counts, title = "File Counts by Year", 
                         align = TRUE, header = FALSE, 
                         summary = FALSE, rownames = FALSE, digit.separator = "", out = paste0(textfolder, "file_counts", ".tex"))
  
}

find_k <- function(topic_map){
  bytech = topic_map %>%
    group_by(hi_tech) %>%
    summarize(across(starts_with('topic'), ~ mean(.x))) %>%
    filter(hi_tech == 1) %>%
    select(starts_with('topic'))
  topic_name = names(bytech)[which.max(bytech[1, ])]
  topic_number = str_split(topic_name, "_")[[1]][2]
  return(list(topic_name, topic_number))
}

getFiscalYearWe <- function(week) {
  year <- week %/% 100 # Extract the year part
  week_num <- week %% 100 # Extract the week part
  
  if (week_num > 26) {
    year <- year + 1
  }
  return(year)
}

getFiscalYearMo <- function(ym) {
  year <- ym %/% 100 # Extract the year part
  month_num <- ym %% 100 # Extract the week part
  
  if (month_num > 6) {
    year <- year + 1
  }
  return(year)
}

label_table_map <- function(name){
if (name == "dicfullmc10thr10defnob40noa1_4t"){
  k <- 0
  labels <- vector("list", 2)
  labels[[1]] <- paste("topic", k, sep = "_")
  labels[[2]] <- as.character(k)
  
  topic_map_labeled = topic_map_unlabeled
  
  names(topic_map_labeled)[names(topic_map_labeled) == "topic_0"] <- "topic_kk"
  names(topic_map_labeled)[names(topic_map_labeled) == "topic_1"] <- "topic_finl"
  names(topic_map_labeled)[names(topic_map_labeled) == "topic_2"] <- "topic_sw"
  names(topic_map_labeled)[names(topic_map_labeled) == "topic_3"] <- "topic_rawm"  
}
  else if (name == "dicfullmc10thr10defnob40noa0_8_4t") {
    k <- 1
    labels <- vector("list", 2)
    labels[[1]] <- paste("topic", k, sep = "_")
    labels[[2]] <- as.character(k)
    
    topic_map_labeled = topic_map_unlabeled
    
    names(topic_map_labeled)[names(topic_map_labeled) == "topic_0"] <- "topic_0"
    names(topic_map_labeled)[names(topic_map_labeled) == "topic_1"] <- "topic_1"
    names(topic_map_labeled)[names(topic_map_labeled) == "topic_2"] <- "topic_2"
    names(topic_map_labeled)[names(topic_map_labeled) == "topic_3"] <- "topic_kk"   
  }
  return(topic_map_labeled)
}

mean_cor = function(cormatrix, mask){
  diag(cormatrix) <- 0
  diag(mask) <- 0
  return(sum(cormatrix*mask, na.rm = TRUE)/sum(mask, na.rm = TRUE))
}

odds <- function(p){
  return(p/(1-p))
}

plot_kurtosis <- function(stoxda, cequity_mapper, topic_map){
  print("Plotting kurtosis graph...")
  stox = stoxda %>%
    mutate(date = ymd(date)) %>%
    mutate(y = year(date)) %>%
    mutate(ym = y*100 + month(date)) %>%
    left_join(cequity_mapper, by = c("PERMNO" = "PERMNO", "y")) %>%
    filter(crit_ALL == 1) %>%
    left_join(topic_map, by = c("PERMNO" = "LPERMNO", "y" = "year")) 
  
  stox_by_kk = stox %>%
    mutate(ym = y) %>%
    group_by(ym, PERMNO) %>%
    summarize(moment = kurtosis(RET), group = mean(max_topic)) %>%
    ungroup() %>%
    filter(group - floor(group) == 0) %>%
    mutate(group = factor(group)) %>%
    drop_na(group) %>%
    group_by(group, ym) %>%
    summarize(moment = mean(moment, na.rm = TRUE))  %>%
    mutate(group = case_when(
      group == 0 ~ "kk",
      group == 1 ~ "finl",
      group == 2 ~ "sw",
      group == 3 ~ "rawm",
      TRUE ~ as.character(group)))
  
  ggplot(stox_by_kk, aes(x = ym, y = moment, group = group, color = group)) +
    geom_line() +
    labs(x = "y", y = "moment") +
    scale_color_discrete(name = "group") +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(figfolder, "kurtosis_maxtopic.jpg"), plot = last_plot(), dpi = 600)
  
  stox_by_kk = stox %>%
    mutate(ym = y) %>%
    group_by(ym, PERMNO) %>%
    summarize(moment = kurtosis(RET), group = mean(ntile_topic_kk)) %>%
    ungroup() %>%
    filter(group - floor(group) == 0) %>%
    mutate(group = factor(group)) %>%
    drop_na(group) %>%
    group_by(group, ym) %>%
    summarize(moment = mean(moment, na.rm = TRUE))
  
  ggplot(stox_by_kk, aes(x = ym, y = moment, group = group, color = group)) +
    geom_line() +
    labs(x = "y", y = "moment") +
    scale_color_discrete(name = "N-tile") +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(figfolder, "kurtosis_ntile.jpg"), plot = last_plot(), dpi = 600)
}

redo_equity_mapper <- function(comp_funda2, textfolder){
  
  load("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/stoxmo_post2000short.Rdata")
  cpi_orig = read_csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/CPIAUCSL.csv")
  
  crit_exchg = comp_funda2 %>% 
    select(cik, LPERMNO, exchg, fyear) %>%
    mutate(crit_EXCHG = exchg %in% c(11, 12, 14))
  
  cpi = cpi_orig %>%
    rename(cpi = CPIAUCSL) %>%
    mutate(ym = as.numeric(format(as.Date(DATE), "%Y%m"))) %>%
    mutate(m = as.numeric(format(as.Date(DATE), "%m"))) %>%
    mutate(y = as.numeric(format(as.Date(DATE), "%Y")))
  
  ref_cpi =  cpi$cpi[cpi$ym == 201501]
  
  cequity_mapper = stoxmo_post2000short %>%
    mutate(ym = date%/%100) %>%
    left_join(cpi, by = "ym") %>%
    left_join(crit_exchg, by = c("PERMNO" = "LPERMNO", "y" = "fyear"), relationship = "many-to-many") %>%
    filter(m == 6) %>%
    mutate(constp_PRC = PRC*ref_cpi/cpi) %>%
    mutate(crit_PRC = constp_PRC > 5) %>%
    mutate(crit_SHRCD = SHRCD %in% c(10, 11)) %>%
    mutate(crit_EXCHG = as.double(crit_EXCHG)) %>%
    arrange(y) %>%
    group_by(PERMNO)%>%
    mutate(crit_EXCHG = as.logical(data.table::nafill(crit_EXCHG, type = "locf"))) %>%
    mutate(crit_EXCHG = ifelse(is.na(crit_EXCHG), FALSE, crit_EXCHG)) %>%
    mutate(crit_ALL = crit_PRC & crit_SHRCD & crit_EXCHG) %>%
    drop_na(crit_ALL) %>%
    select(PERMNO, y, starts_with("crit_"))
  
  mean_groups = cequity_mapper %>%
    filter(y >= 2013) %>%
    group_by(y) %>% 
    summarize(mean_EXCHG = round(mean(crit_EXCHG), 3), mean_COMEQ = round(mean(crit_SHRCD), 3), mean_PRC = round(mean(crit_PRC, na.rm = TRUE), 3), mean_ALL = round(mean(crit_ALL), 3))
  
  tex_table <- stargazer(mean_groups, label = "tab:stocks_filtering_criteria", title = "Stocks filtering criteria", align = TRUE, header = FALSE, summary = FALSE, rownames = FALSE, digit.separator = "", out = paste0(textfolder, "stocks_filtering_criteria", ".tex")) 
  
  return(cequity_mapper)
}

roll_reg_full5ff <- function(z) {
  model <- lm(eret ~ Mkt.RF + SMB + HML + CMA + RMW + kkrhml, data = as.data.frame(z))
  return(coef(model))}

roll_reg_1 <- function(z) {
  model <- lm(eret ~ 1, data = as.data.frame(z))
  return(coef(model))}

roll_reg_mktrf <- function(z) {
  model <- lm(eret ~ Mkt.RF - 1, data = as.data.frame(z))
  return(coef(model))}

roll_reg_smb <- function(z) {
  model <- lm(eret ~ SMB, data = as.data.frame(z)) # You need to include a constant
  # i.e. not include a -1. See Jagannathan/Wang, 1996
  return(coef(model))}

roll_reg_hml <- function(z) {
  model <- lm(eret ~ HML, data = as.data.frame(z))
  return(coef(model))}

roll_reg_cma <- function(z) {
  model <- lm(eret ~ CMA, data = as.data.frame(z))
  return(coef(model))}

roll_reg_rmw <- function(z) {
  model <- lm(eret ~ RMW, data = as.data.frame(z))
  return(coef(model))}

roll_reg_kkrhml <- function(z) {
  model <- lm(eret ~ kkrhml, data = as.data.frame(z))
  return(coef(model))}

roll_reg_full3ff <- function(z) {
  model <- lm(eret ~ Mkt.RF + SMB + HML + kkrhml, data = as.data.frame(z))
  return(coef(model))}

rwin_coefs_mo <- function(window_size, pfn){
  function_list <- list(
    roll_reg_full5ff = roll_reg_full5ff
    # roll_reg_1 = roll_reg_1,
    # roll_reg_mktrf = roll_reg_mktrf,
    # roll_reg_smb = roll_reg_smb,
    # roll_reg_hml = roll_reg_hml,
    # roll_reg_cma = roll_reg_cma, 
    # roll_reg_rmw = roll_reg_rmw, 
    # roll_reg_kkrhml = roll_reg_kkrhml
  )
  
  pfnames = unique(eret_mo[, pfn])
  output = data.frame()
  for (pfname in pfnames){
    subdf = eret_mo %>% 
      filter(!!sym(pfn) == pfname) %>% filter(ym < 202000)
    results_list <- list()
    for (func_name in names(function_list)) {
      results <- rollapply(data = subdf %>% rename(eret = eretm), width = window_size, 
                           FUN = function_list[[func_name]], 
                           by.column = FALSE, 
                           align = "right", 
                           fill = NA)
      results <- as.data.frame(results) 
      # Store the results in the list
      results_list[[func_name]] <- results
    }
    browser()
    final_results <- do.call(cbind, results_list)
    colnames(final_results) <- c("(Intercept)", "Mkt.RF", "SMB", "HML", "kkrhml")
    # results <- rollapply(data = subdf %>% rename(eret = eretm), width = window_size, 
    #                      FUN = roll_reg_1, 
    #                      by.column = FALSE, 
    #                      align = "right", 
    #                      fill = NA)
    # results <- as.data.frame(results)
    final_results$eretm = subdf$eretm
    final_results$ym = subdf$ym
    final_results$pfname = pfname
    output = rbind(output, final_results) 
  }
  output = output %>% drop_na()
  return(output)
}

rwin_coefs_mo_mult <- function(window_size, pfn){
  output = data.frame()
  pfnames = as.matrix(unique(eret_mo[, pfn]))
  for (pfname in pfnames){
    subdf = eret_mo %>% filter(!!sym(pfn) == pfname) %>% filter(ym < 202000)
    results <- rollapply(data = subdf %>% rename(eret = eretm), width = window_size,
                         FUN = roll_reg_full5ff,
                         by.column = FALSE,
                         align = "right",
                         fill = NA)
    final_results <- as.data.frame(results)
    final_results$eretm = subdf$eretm
    final_results$ym = subdf$ym
    final_results$pfname = pfname
    output = rbind(output, final_results) 
  }
  output = output %>% drop_na()
  return(output)
}


rwin_coefs_we <- function(window_size){
  roll_reg <- function(z) {
    model <- lm(eretw ~ Mkt.RF + SMB + HML + kkrhml, data = as.data.frame(z))
    return(coef(model))
  }
  output = data.frame()
  for (pfname in unique(eret_we$pf36_name)){
    subdf = eret_we %>% filter(pf36_name == pfname)
    results <- rollapply(data = subdf, width = window_size, FUN = roll_reg, by.column = FALSE, align = "right", fill = NA)
    results <- as.data.frame(results)
    results$eretw = subdf$eretw
    results$yw = subdf$yw
    results$pf36_name = pfname
    output = rbind(output, results) 
  }
  output = output %>% drop_na()
  return(output)
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

stargaze_comparison <- function(comparison_measures, figfolder){
  stargazer(comparison_measures, title = "Correlation of textual-based measure of KK risk with other firm-level measures", 
            align = TRUE, header = FALSE, summary = FALSE, rownames = FALSE, digit.separator = "", label = "fig:bytech", out = paste0(figfolder, "corr_measures", ".tex")) 
}

understand_topics <- function(topic_map_labeled, labels, quantiles, textfolder){
  print("Understanding topics...")
  # results = find_k(topic_map)
  # topic_k = labels[[1]]
  # k = labels[[2]]
  nt = quantiles
  
  topic_map = topic_map_labeled %>% #rename(topic_kk = !!sym(topic_k)) %>%
    group_by(year) %>%
    mutate(ntile_topic_kk = ntile(topic_kk, nt)) %>%
    mutate(kkpt_ntile = ntile(K_int_Know/at, nt)) %>%
    mutate(ikpt_ntile = ntile(K_int/at, nt)) %>%
    ungroup() 

  create_topic_plots(topic_map, textfolder)  
  
  bytech = topic_map_labeled %>%
    drop_na(hi_tech) %>%
    group_by(hi_tech) %>%
    summarize(across(starts_with('topic'), ~ round(mean(.x),3)))
  
  stargazer(bytech, title = "Topic averages by hi-tech status", 
            align = TRUE, header = FALSE, 
            summary = FALSE, rownames = FALSE, digit.separator = "", label = "fig:bytech", out = paste0(textfolder, "tpcavg_tech", ".tex"))  
  
  set.seed(139)
  sample_topics = topic_map_labeled %>%
    select(conm, year, starts_with("topic_")) %>%
    sample_n(10) %>%
    arrange(conm) %>%
    rename(Company_Name = conm) %>%
    mutate_at(vars(grep("^topic_", names(.))), ~round(., 3))
  
  stargazer(sample_topics, title = "Sample of a topic map", align = TRUE, header = FALSE, summary = FALSE, rownames = FALSE, digit.separator = "", out = paste0(textfolder, "sample_map", ".tex"))  
  
  skillcor = topic_map %>%
    select(starts_with("topic"), "Skill")
  
  cor_matrix <- melt(cor(skillcor, use = "complete.obs"))
  
  heatmap <- ggplot(cor_matrix, aes(Var1, Var2)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value,3))) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(textfolder, "heatmap.png"), plot = heatmap, dpi = 600)
  
  patentcor = topic_map %>%
    select(starts_with("topic"), "xir_cumsum") 
  patentcor_matrix <- melt(cor(patentcor, use = "complete.obs"))
  
  heatmap <- ggplot(patentcor_matrix, aes(Var1, Var2)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value,3))) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(textfolder, "heatmap_patents.png"), plot = heatmap, dpi = 600)
  
  firms_by_ind = topic_map %>%
    filter(ntile_topic_kk == nt) %>%
    group_by(year, ind12) %>%
    summarize(count = n(), totalat = sum(at, na.rm = TRUE)) %>%
    ungroup()
  
  industry_names <- c("Cnsmr non-dur.", "Cnsmr durbl", "Manuf", "Enrgy", "Chems", "BusEq", "Telcm", 
                      "Utils", "Whlsl/Retail", "Hlth", "Other", "NoDef")
  
  firms_by_ind$ind12 <- factor(firms_by_ind$ind12, levels = 1:12, labels = industry_names)
  
  stackedplot_n <- ggplot(firms_by_ind, aes(x = factor(year), y = count, fill = ind12)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ylab("Share of all dominant-KK firms") +
    xlab("Year") +
    labs(fill = "Industry")  +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(angle = 45, hjust = 1,size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(textfolder, "stackedplot_n.png"), plot = stackedplot_n, dpi = 600)
  
  stackedplot_at <- ggplot(firms_by_ind, aes(x = factor(year), y = totalat, fill = ind12)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ylab("Share of all dominant-KK firms") +
    xlab("Year") +
    labs(fill = "Industry")  +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(angle = 45, hjust = 1, size = 14)     # Adjust axis label font size
    )
  stackedplot_at
  
  ggsave(paste0(textfolder, "stackedplot_at.png"), plot = stackedplot_at, dpi = 600)
  
  firms_by_kk = topic_map %>%
    drop_na(ntile_topic_kk, kkpt_ntile) %>%
    group_by(ntile_topic_kk, kkpt_ntile) %>%
    summarize(count = n())

  topicvskkpt_hm <- ggplot(firms_by_kk, aes(ntile_topic_kk, kkpt_ntile)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = count)) + 
    geom_text(aes(label = round(count,2))) + 
    scale_fill_gradient2(low = "white", high = "red")+
    ylab("Quartiles of Knowledge Capital Intensity") +
    xlab("Quartiles of Knowledge Capital Risk measured by Topic_kk") +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )

  ggsave(paste0(textfolder, "topicvskkpt_hm.png"), plot = topicvskkpt_hm, dpi = 600)
  
  firms_by_ik = topic_map %>%
    drop_na(ntile_topic_kk, ikpt_ntile) %>%
    group_by(ntile_topic_kk, ikpt_ntile) %>%
    summarize(count = n())
  
  topicvsikpt_hm <- ggplot(firms_by_ik, aes(ntile_topic_kk, ikpt_ntile)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = count)) + 
    geom_text(aes(label = round(count,2))) + 
    scale_fill_gradient2(low = "white", high = "red")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  
  ggsave(paste0(textfolder, "topicvsikpt_hm.png"), plot = topicvsikpt_hm, dpi = 600)
  
  mean_topic_kk = topic_map %>%
    group_by(year, ind12) %>%
    summarize(mean_kk = mean(topic_kk)) %>%
    ungroup() %>%
    pivot_wider(names_from = ind12, names_prefix = "ind", values_from = mean_kk)

  return(topic_map)
}

yw2day <- function(df){
  library(ISOweek)
  df$yw  <- paste0(df$yw , "1")
  df$yw  <- as.Date(df$yw  , format = "%Y%W%u")
  invisible(df)
}

plot_returns <- function(){
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
    labs(x = "Year-month", y = "Asset-weighted weekly returns")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awwr.jpg"), plot = last_plot(), dpi = 600)
  
  ggplot(qt_ret_bygroup, aes(x = yw, y = eret3ma, col = ntile_topic_kk)) + geom_line()  +
    labs(x = "Year-month", y = "Asset-weighted weekly returns, 3MA") +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awwr3ma.jpg"), plot = last_plot(), dpi = 600)
  
  ggplot(we_ret_bybin , aes(x = yw, y = eret_accum, col = ntile_topic_kk)) +
    geom_line() +
    labs(x = "Year-month", y = "Asset-weighted accumulated weekly returns")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awawr.jpg"), plot = last_plot(), dpi = 600)
  we_ret_bybin_filt = we_ret_bybin %>% 
    filter(ntile_topic_kk %in% c(1,4)) %>%
    group_by(ntile_topic_kk) %>%
    mutate(moving_average = (lag(sderet, order_by = yw) 
                             + lag(sderet, order_by = yw, n = 2) + lag(sderet, order_by = yw, n = 3) 
                             + sderet) / 4)
  
  ggplot(we_ret_bybin_filt, aes(x = yw, y = moving_average, col = ntile_topic_kk)) + geom_line() + labs(x = "Year-month", y = "Weekly standard deviation of returns by n-tile, four-week MA")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "wsdr.jpg"), plot = last_plot(), dpi = 600)
  
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
    labs(x = "Year-month", y = "Asset-weighted weekly returns")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awwr_byg.jpg"), plot = last_plot(), dpi = 600)
  ggplot(qt_ret_bygroup, aes(x = yw, y = eret3ma, col = max_topic)) + geom_line()  +
    labs(x = "Year-month", y = "Asset-weighted weekly returns, 3MA") +
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awwr3ma_byg.jpg"), plot = last_plot(), dpi = 600)
  ggplot(we_ret_bygroup, aes(x = yw, y = eret_accum, col = max_topic)) +
    geom_line() +
    labs(x = "Year-month", y = "Asset-weighted accumulated weekly returns")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "awawr_byg.jpg"), plot = last_plot(), dpi = 600)
  ggplot(we_ret_bygroup , aes(x = yw, y = sderet, col = max_topic)) +
    geom_line() +
    labs(x = "Year-month", y = "(Non-asset-weighted) weekly standard deviation of returns")+
    theme(
      legend.text = element_text(size = 14),  # Adjust legend font size
      axis.text = element_text(size = 14)     # Adjust axis label font size
    )
  ggsave(paste0(figfolder, "wsdr_byg.jpg"), plot = last_plot(), dpi = 600)
}

fama_macbeth <- function(mo_window, pfn){
  first_stage_rollwin_mo = rwin_coefs_mo_mult(mo_window, mo_window)
  
  second_stage_rollwin_mo = first_stage_rollwin_mo %>%
    ungroup() %>%
    nest_by(ym) %>%
    mutate(mod = list(lm(eretm ~ kkrhml + HML + SMB + Mkt.RF + `(Intercept)`-1, data = data))) %>%
    summarize(tidy(mod)) %>%
    select(-std.error, -statistic, -p.value) %>%
    pivot_wider(names_from = term, values_from = estimate)
  return(list(first_stage_rollwin_mo, second_stage_rollwin_mo))
}

first_stage <- function(formula, eretdf, pfn){
  first_stage1 = eretdf %>%
    ungroup() %>%
    nest_by(!!sym(pfn)) %>%
    mutate(mod = list(lm(formula, data = data))) %>%
    summarize(eretw = mean(data$eretw), t = length(data$eretw), tidy(mod)) 
  
  get_sigmae = first_stage1 %>%
    mutate(sigmae = t*std.error^2) %>%
    filter(term == "(Intercept)") %>%
    select(!!sym(pfn), sigmae) %>%
    drop_na()
  
  first_stage2 = first_stage1 %>%
    select(-std.error, -statistic, -p.value) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    full_join(get_sigmae, by = c(pfn)) %>%
    drop_na()
  return(first_stage2)
}

# </editor-fold>