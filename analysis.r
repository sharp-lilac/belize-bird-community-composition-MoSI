# analysis.r

## Source code ------------------------
source("helper_scripts/data_load.r")
source("helper_scripts/theme.r")

# Create abundance dataframe ----------------------------------
df_unique_net_opens <- df_effort %>%
    select(Station, NetNum = `Net #`, Year, Month, Day) %>%
    distinct()
df_counts <- df %>%
    filter(Code != "R" & Code != "U") %>%
    mutate(Year = as.integer(YYYY), NetNum = as.integer(`Net #`), Month = as.integer(MM), Day = as.integer(DD)) %>%
    group_by(Year, Month, Day, Station, NetNum) %>%
    summarise(Count = n()) %>%
    na.omit() %>%
    right_join(df_unique_net_opens, by = c("Station", "NetNum", "Year", "Month", "Day")) %>%
    mutate(Count = replace_na(Count, 0))
df_effort_time <- df_effort %>%
    mutate(NetNum = `Net #`) %>%
    mutate(
        TimeOpened_str = sprintf("%04d", TimeOpened),
        TimeClosed_str = sprintf("%04d", TimeClosed),
        TimeOpened_parsed = hm(paste0(substr(TimeOpened_str, 1, 2), ":", substr(TimeOpened_str, 3, 4))),
        TimeClosed_parsed = hm(paste0(substr(TimeClosed_str, 1, 2), ":", substr(TimeClosed_str, 3, 4))),
        TimeOpen = as.numeric(TimeClosed_parsed - TimeOpened_parsed, "hours")
    ) %>%
    select(Year, Month, Day, Station, NetNum, TimeOpen) %>%
    group_by(Year, Month, Day, Station, NetNum) %>%
    summarise(Effort = sum(TimeOpen)) %>%
    na.omit()
df_abun_effort <- df_counts %>%
    left_join(df_effort_time, by = c("Year", "Month", "Day", "Station", "NetNum")) %>%
    group_by(Year, Month, Day, Station) %>%
    summarise(Effort = sum(Effort, na.rm = TRUE), Count = sum(Count, na.rm = TRUE), Nets = n_distinct(NetNum)) %>%
    ungroup() %>%
    mutate(
        Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"),
        Season = if_else(Month %in% c(11, 12), paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year)),
        Count_Stand = Count / Effort * 50
    ) %>%
    filter(Effort > 10) %>%
    mutate(Station = factor(Station, levels = c("BRL1", "FWC1", "CBS2", "RCR2", "TPPL", "CT"), ordered = TRUE))

# Create abundance figure ----------------------------------
fig_abun <- ggplot(df_abun_effort, aes(x = Season, y = Count_Stand)) +
    geom_boxplot() +
    stat_summary(fun.y = "mean", shape = 2) +
    facet_wrap(~Station) +
    theme_light() +
    labs(y = "Count per 50 Net Hours") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("outputs/fig_abun.jpg", fig_abun, width = 8, height = 6)

# Create diversity dataframes ----------------------------------
df_species <- df %>%
    filter(Code != "R" & Code != "U") %>%
    filter(`Species Code` != "BADE" & `Species Code` != "BALO") %>%
    mutate(Year = as.integer(YYYY), NetNum = as.integer(`Net #`), Month = as.integer(MM), Day = as.integer(DD)) %>%
    group_by(Year, Month, Day, Station, `Species Code`) %>%
    summarise(Count = n()) %>%
    na.omit() %>%
    mutate(
        Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"),
        Season = if_else(Month %in% c(11, 12), paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
    ) %>%
    select(Station, Date, Season, `Species Code`, Count) %>%
    left_join(df_abun_effort %>% select(Station, Date, Effort), by = c("Station", "Date")) %>%
    mutate(Count_Stand = Count / Effort * 50)
df_shannon <- df_species %>%
    group_by(Station, Date, Season) %>%
    summarise(
        Shannon = diversity(Count_Stand, index = "shannon"),
        Richness = specnumber(Count_Stand),
        Effective = exp(Shannon),
        .groups = "drop"
    ) %>%
    mutate(Station = factor(Station, levels = c("BRL1", "FWC1", "CBS2", "RCR2", "TPPL", "CT"), ordered = TRUE))
df_shannon_long <- df_shannon %>%
    pivot_longer(cols = c("Shannon", "Richness", "Effective"), names_to = "Index", values_to = "Value") %>%
    filter(Index != "Shannon")

# Create diversity figures ----------------------------------
fig_shannon <- ggplot(df_shannon, aes(x = Date, y = Shannon)) +
    geom_point() +
    geom_smooth(method = "lm", color = palette[2]) +
    facet_wrap(~Station) +
    labs(y = "Shannon Diversity Index") +
    theme_light()
ggsave("outputs/fig_shannon.jpg", fig_shannon, width = 8, height = 6)
fig_richness <- ggplot(df_shannon_long, aes(x = Date, y = Value, color = Index)) +
    geom_smooth(method = "lm") +
    labs(y = "Species Count per 50 Net Hours") +
    facet_wrap(~Station) +
    theme_light() +
    scale_color_manual(values = palette)
ggsave("outputs/fig_richness.jpg", fig_richness, width = 9, height = 6)

# Create dominance dataframe ----------------------------------
df_rank_all <- df_species %>%
    group_by(Station, `Species Code`) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    group_by(Station) %>%
    arrange(Station, desc(Count)) %>%
    mutate(
        Rank = row_number(),
        RelAbund = Count / sum(Count)
    ) %>%
    mutate(Station = factor(Station, levels = c("BRL1", "FWC1", "CBS2", "RCR2", "TPPL", "CT"), ordered = TRUE))

# Create dominance table ----------------------------------
table_rank_top5 <- df_rank_all %>%
    filter(Rank <= 5) %>%
    mutate(
        RelAbund_fmt = sprintf("%.1f%%", RelAbund * 100),
        Combo = paste0(`Species Code`, " (", RelAbund_fmt, ")")
    ) %>%
    select(Station, Rank, Combo) %>%
    pivot_wider(
        names_from = Rank,
        values_from = Combo,
        names_prefix = "Rank "
    ) %>%
    left_join(
        df_rank_all %>%
            group_by(Station) %>%
            summarise(`Total Ranks` = max(Rank, na.rm = TRUE)),
        by = "Station"
    )
write.csv(table_rank_top5, "outputs/table_ranks.csv")

# Create dominance figures ----------------------------------
fig_rank <- ggplot(df_rank_all, aes(x = Rank, y = RelAbund, color = Station)) +
    geom_line(linewidth = 0.7, alpha = 0.5) +
    geom_point(size = 0.7, alpha = 0.5) +
    labs(x = "Species Rank", y = "Relative Abundance") +
    theme_light() +
    guides(color = guide_legend(override.aes = list(linewidth = 1))) +
    scale_color_manual(values = palette)
ggsave("outputs/fig_rank.jpg", fig_rank, width = 8, height = 6)
fig_rank_log <- ggplot(df_rank_all, aes(x = Rank, y = RelAbund, color = Station)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 0.7, alpha = 0.8) +
    scale_y_log10(
        breaks = 10^(-4:0),
        minor_breaks = rep(1:9, each = 5) * 10^rep(-4:0, times = 9),
    ) +
    scale_x_continuous(breaks = seq(0, max(df_rank_all$Rank, na.rm = TRUE), by = 20)) +
    labs(x = "Species Rank", y = "Relative Abundance (log10)") +
    theme_light() +
    guides(color = guide_legend(override.aes = list(linewidth = 1))) +
    scale_color_manual(values = palette) +
    theme(
        panel.grid.major.y = element_line(color = "grey40", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey80", linewidth = 0.3)
    )
ggsave("outputs/fig_rank_log.jpg", fig_rank_log, width = 8, height = 6)

# Create iNEXT object ----------------------------------
df_clean <- df %>%
    filter(!is.na(`Species Code`), !Code %in% c("R", "U")) %>%
    group_by(Station, `Species Code`) %>%
    summarise(count = n(), .groups = "drop")
list_abundance <- df_clean %>%
    group_split(Station) %>%
    setNames(unique(df_clean$Station)) %>%
    lapply(function(x) x$count)
list_inext_all <- iNEXT(list_abundance, q = 0, datatype = "abundance")
df_inext_all <- fortify(list_inext_all, type = 1)
vector_station_order <- c("BRL1", "FWC1", "CBS2", "RCR2", "TPPL", "CT")
df_inext_all$Assemblage <- factor(df_inext_all$Assemblage, levels = vector_station_order, ordered = TRUE)
df_transition_points <- df_inext_all %>%
    filter(Method == "Extrapolation") %>%
    group_by(Assemblage) %>%
    slice(1) %>%
    ungroup()

# Create rarefaction figure ----------------------------------
fig_rarefaction <- ggplot(df_inext_all, aes(x = x, y = y, color = Assemblage)) +
    geom_line() +
    geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = Assemblage), alpha = 0.2) +
    geom_point(data = df_transition_points, aes(x = x, y = y), shape = 8, size = 5, color = "black") +
    labs(x = "Number of Individuals", y = "Species Richness") +
    theme_classic() +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette)
ggsave("outputs/fig_rarefaction.jpg", fig_rarefaction, width = 8, height = 6)

# Create species matrices ----------------------------------
df_species_matrix <- df_species %>%
    group_by(Station, `Species Code`) %>%
    summarise(Total = sum(Count_Stand, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = `Species Code`, values_from = Total, values_fill = 0) %>%
    column_to_rownames(var = c("Station"))
df_species_matrix_long <- df_species_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "Station") %>%
    pivot_longer(-Station, names_to = "Species", values_to = "Abundance")
df_species_matrix_alternate <- df_species %>%
    filter(`Species Code` != "BADE" & `Species Code` != "BALO") %>%
    group_by(Station, Season, `Species Code`) %>%
    summarise(Total = sum(Count_Stand, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = `Species Code`, values_from = Total, values_fill = 0)
df_site_info <- df_species_matrix_alternate %>%
    select(Station, Season)
matrix_community <- df_species_matrix_alternate %>%
    select(-Station, -Season) %>%
    as.matrix()
rownames(matrix_community) <- df_site_info$Station

# Create exclusive species table ----------------------------------
table_exclusive_species <- df_species_matrix_long %>%
    filter(Abundance > 0) %>%
    group_by(Species) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    group_by(Station) %>%
    summarise(`Exclusive Species` = paste(Species, collapse = ", "))
write.csv(table_exclusive_species, "outputs/table_exclusive_species.csv")

# Run NMDS ----------------------------------
mds_nmds <- metaMDS(matrix_community, distance = "bray", k = 2, trymax = 100)
df_nmds_sites <- as.data.frame(scores(mds_nmds, display = "sites"))
df_nmds_sites$Station <- df_site_info$Station
df_nmds_sites$Season <- df_site_info$Season
df_nmds_species <- as.data.frame(scores(mds_nmds, display = "species"))
df_nmds_species$species <- rownames(df_nmds_species)
mds_nmds$stress

# Create NMDS centroid means table ----------------------------------
df_nmds_sites_label <- df_nmds_sites %>%
    group_by(Station) %>%
    summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))
write.csv(df_nmds_sites_label, "outputs/table_centroid_means.csv")

# Create NMDS figure ----------------------------------
fig_nmds <- ggplot(df_nmds_sites, aes(x = NMDS1, y = NMDS2, color = Station)) +
    geom_point(size = 3) +
    stat_ellipse(aes(group = Station), linetype = 2) +
    geom_text(
        data = df_nmds_sites_label, aes(x = NMDS1, y = NMDS2, label = Station),
        inherit.aes = FALSE, size = 4, fontface = "bold"
    ) +
    geom_text(
        data = df_nmds_species, aes(x = NMDS1, y = NMDS2, label = species),
        inherit.aes = FALSE, color = "black", alpha = 0.5, size = 1.8
    ) +
    theme_bw() +
    scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
    coord_equal() +
    labs(x = "NMDS1", y = "NMDS2")
ggsave("outputs/fig_nmds.jpg", fig_nmds, width = 8, height = 9)
ggsave("outputs/fig_nmds_wide.jpg", fig_nmds, width = 14, height = 6)

# Run PERMANOVA ----------------------------------
df_site_info$Station <- as.factor(df_site_info$Station)
df_site_info$Season <- as.factor(df_site_info$Season)
permanova_community <- adonis2(
    matrix_community ~ Station + Season,
    data = df_site_info,
    method = "bray",
    permutations = 999
)
permanova_community
bd_station <- betadisper(
    vegdist(matrix_community, method = "bray"),
    df_site_info$Station
)
permutest(bd_station)
