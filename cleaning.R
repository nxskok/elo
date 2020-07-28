clean_names <- function(games) {
  games %>% group_by(t1) %>% count(t1_name) %>% 
    select(id=t1, name=t1_name, n) -> h
  games %>% group_by(t2) %>% count(t2_name) %>% 
    select(id=t2, name=t2_name, n) -> a
  h %>% bind_rows(a) %>% 
    group_by(id, name) %>% 
    summarize(nn=sum(n)) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    slice(which.max(nn)) %>% 
    select(-nn) -> cleaned_names
  cleaned_names
}

lookup_cleaned_names <- function(games, names) {
  games %>% select(time_stamp, t1, t2, score) %>% 
    left_join(names, by=c("t1"="id")) %>% 
    left_join(names, by=c("t2"="id")) %>% 
    select(time_stamp, t1=name.x, t2=name.y, score) 
}

get_clean_games <- function(comp_ids, games) {
  games %>% filter(comp %in% comp_ids) -> gg
  cleaned_names <- clean_names(gg)
  lookup_cleaned_names(gg, cleaned_names) %>% 
    filter(str_detect(score, " - ")) %>% 
    arrange(time_stamp) %>% 
    separate(score, into=c("s1", "s2"), convert = T) %>% 
    mutate(r=case_when(
      s1>s2 ~ 1,
      s1<s2 ~ 0,
      TRUE  ~ 0.5
    )) -> d
  d
}

ei_comp <- function(comp_id, games) {
  d <- get_clean_games(comp_id, games)
  estimate_initial(d)
}
