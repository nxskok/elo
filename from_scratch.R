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
  games %>% select(time_stamp, t1, t2, score, stat, comp) %>% 
    left_join(names, by=c("t1"="id")) %>% 
    left_join(names, by=c("t2"="id")) %>% 
    select(time_stamp, t1=name.x, t2=name.y, score, stat, comp) 
}

get_clean_games <- function(comp_ids, games) {
  games %>% filter(comp %in% comp_ids) -> gg
  cleaned_names <- clean_names(gg)
  lookup_cleaned_names(gg, cleaned_names) %>% 
    filter(str_detect(stat, "FT")) %>% 
    select(-stat) %>% 
    arrange(time_stamp) %>% 
    separate(score, into=c("s1", "s2"), convert = T) %>% 
    mutate(r=case_when(
      s1>s2 ~ 1,
      s1<s2 ~ 0,
      TRUE  ~ 0.5
    )) -> d
  list(d=d, names = cleaned_names)
}

# modify  this to account for structure

new_ratings <- function(k, d, r) {
  # k is the elo k
  # d is a dataframe of the current season's games with teams as t1 and t2, results as r, on a 1-0.5-0 scale
  # r is dataframe of initial teams, ratings and home adv
  # returns updated ratings and home adv, and updated df with predicted probs and deltas
  
  # return(list(k, d, r))
  ratings <- r$rat
  names(ratings) <- r$name
  h <- r$h[1]
  
  d %>% mutate(p=NA, delta=NA, r1=NA, r2=NA) -> d
  for (i in 1:nrow(d)) {
    t1name <- d$t1[i]
    t2name <- d$t2[i]
    result <- d$r[i]
    rat1 <- ratings[t1name]
    rat2 <- ratings[t2name]
    diff <- rat1-rat2+h
    p <- 1/(1+10^(-diff/400))
    delta <- k*(result-p)
    ratings[t1name] <- ratings[t1name] + delta
    ratings[t2name] <- ratings[t2name] - delta
    
    h <- h + delta/10
    # print(c(p, delta, h))
    d$p[i] <- p
    d$delta[i] <- delta
    d$r1[i] <- ratings[t1name]
    d$r2[i] <- ratings[t2name]
    
  }
  enframe(ratings, name="team", value="rating") %>% mutate(h=h) -> r
  # print("p from new_ratings:")
  # print(d$p)
  pb$tick()
  list(d=d, r=r, k=k)
}


this_ratings <- function(comp, pre, k=15, games) {
  d2 <- get_clean_games(comp, games) 
  new_ratings(k, d2, pre)
}

