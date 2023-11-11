
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

add_hi_tech_column <- function(df) {
  hi_tech_sic <- c(283, 357, 366, 367, 382, 384, 737)
  df$sic3 = as.numeric(substr(as.character(df$sic), 1, 3))
  df$hi_tech <- ifelse(df$sic3 %in% hi_tech_sic, 1, 0)
  
  return(df)
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
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(figfolder, "mean_tiy.jpg"), plot = last_plot(), dpi = 300)
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

odds <- function(p){
  return(p/(1-p))
}


understand_topics <- function(topic_map, textfolder){
  print("Understanding topics...")
  results = find_k(topic_map)
  topic_k = results[[1]]
  k = results[[2]]
  nt = 4
  
  topic_map = topic_map %>%
    rename(topic_kk = !!sym(topic_k)) %>%
    group_by(year) %>%
    mutate(ntile_topic_kk = ntile(topic_kk, nt)) %>%
    mutate(kkpt_ntile = ntile(K_int_Know/at, nt)) %>%
    mutate(ikpt_ntile = ntile(K_int/at, nt)) %>%
    ungroup() 
  
  create_topic_plots(topic_map, textfolder)  
  
  bytech = topic_map %>%
    group_by(hi_tech) %>%
    summarize(across(starts_with('topic'), ~ round(mean(.x),2)))
  
  stargazer(bytech, title = "Topic averages by hi-tech status", 
            align = TRUE, header = FALSE, 
            summary = FALSE, rownames = FALSE, digit.separator = "", label = "fig:bytech", out = paste0(textfolder, "tpcavg_tech", ".tex"))  
  
  skillcor = topic_map %>%
    select(starts_with("topic"), "Skill")
  
  cor_matrix <- melt(cor(skillcor, use = "complete.obs"))
  
  heatmap <- ggplot(cor_matrix, aes(Var1, Var2)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value,2))) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
  
  ggsave(paste0(textfolder, "heatmap.png"), plot = heatmap, width = 10, height = 8, dpi = 300)
  
  patentcor = topic_map %>%
    select(starts_with("topic"), "xir_cumsum") 
  patentcor_matrix <- melt(cor(patentcor, use = "complete.obs"))
  
  patentcor = topic_map %>%
    select(starts_with("topic"), "xir_cumsum") 
  patentcor_matrix <- melt(cor(patentcor, use = "complete.obs"))
  
  heatmap <- ggplot(patentcor_matrix, aes(Var1, Var2)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value,2))) + 
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
  
  ggsave(paste0(textfolder, "heatmap_patents.png"), plot = heatmap, width = 10, height = 8, dpi = 300)
  
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0(textfolder, "stackedplot_n.png"), plot = stackedplot_n, width = 10, height = 8, dpi = 300)
  
  stackedplot_at <- ggplot(firms_by_ind, aes(x = factor(year), y = totalat, fill = ind12)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ylab("Share of all dominant-KK firms") +
    xlab("Year") +
    labs(fill = "Industry")  +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  stackedplot_at
  
  ggsave(paste0(textfolder, "stackedplot_at.png"), plot = stackedplot_at, width = 10, height = 8, dpi = 300)
  
  firms_by_kk = topic_map %>%
    drop_na(ntile_topic_kk, kkpt_ntile) %>%
    group_by(ntile_topic_kk, kkpt_ntile) %>%
    summarize(count = n())
  
  topicvskkpt_hm <- ggplot(firms_by_kk, aes(ntile_topic_kk, kkpt_ntile)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = count)) + 
    geom_text(aes(label = round(count,2))) + 
    scale_fill_gradient2(low = "white", high = "red")
  topicvskkpt_hm
  ggsave(paste0(textfolder, "topicvskkpt_hm.png"), plot = topicvskkpt_hm, width = 10, height = 8, dpi = 300)
  
  firms_by_ik = topic_map %>%
    drop_na(ntile_topic_kk, ikpt_ntile) %>%
    group_by(ntile_topic_kk, ikpt_ntile) %>%
    summarize(count = n())
  
  topicvsikpt_hm <- ggplot(firms_by_ik, aes(ntile_topic_kk, ikpt_ntile)) +    # Create default ggplot2 heatmap
    geom_tile(aes(fill = count)) + 
    geom_text(aes(label = round(count,2))) + 
    scale_fill_gradient2(low = "white", high = "red")
  
  ggsave(paste0(textfolder, "topicvsikpt_hm.png"), plot = topicvsikpt_hm, width = 10, height = 8, dpi = 300)
  
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

average_jk <- function(matrix) {
  result <- matrix(FALSE, nrow = nrow(matrix), ncol = nrow(matrix))
  diag(result) <- TRUE
  mask = as.numeric(!(result | is.na(matrix)))
  average = sum(mask*matrix, na.rm = TRUE) / sum(mask, na.rm = TRUE)
  return(average)
}
