Previously we looked at the statistical properties of measurements. Now, lets
analyze the raw data. The measurements seems to be temporal with a timestamp
ordered by the row index. I will plot the data with index on the `x` axis to 
see the measurements relation with time.

```{r}
# Removing stat variables
cols <- names(dataset)
statistic_abbrev <- c("avg", "stddev", "skewness", "var", "max", "min", 
                      "kurtosis", "total", "amplitude")
nonstat_cols <- cols[reduce(map(statistic_abbrev, ~ !grepl(.x, cols)), `&`)]

temporal <- dataset %>% 
  filter(new_window != "yes") %>%
  select_(.dots = nonstat_cols) %>% 
  select(-contains("timestamp"), -contains("window"), -X) %>% 
  group_by(user_name) %>% 
  mutate(index = row_number()) %>% 
  ungroup()

temporal_flat_pos <- temporal %>% 
  gather(colname, colname_val, -user_name, -index, -classe) %>% 
  mutate(pos = ifelse(endsWith(colname, "_x"), "X", 
                       ifelse(endsWith(colname, "_y"), "Y", 
                               ifelse(endsWith(colname, "_z"), "Z", NA_character_)))) %>% 
  mutate(colname = if_else(!is.na(pos), 
                           substr(colname, 1, nchar(colname) - 2), 
                           colname)) %>% 
  filter(!is.na(pos))

ggplot(temporal_flat_pos, aes(index, colname_val)) +
  geom_line(aes(color = classe)) +
  facet_grid(colname ~ pos, scales = "free_y")

```

