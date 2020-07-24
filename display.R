these_ratings <- function(post, league_name) {
  # post is contents of wpost.rds
  post %>% filter(league==league_name) %>% 
    unnest(post) %>% 
    select(team, rating, h) %>% arrange(desc(rating))
}
these_games <- function(post, league_name) {
  post %>% filter(league==league_name) %>% 
    unnest(updates) %>% 
    select(time_stamp:r2) %>% 
    mutate(the_week=iso_week_half(time_stamp)) %>% 
    filter(the_week==max(the_week)) %>% 
    select(-the_week)
}
combined_table <- function(post, league_name) {
  ratings <- these_ratings(with_post, league_name)
  games <- these_games(with_post, league_name)
  ratings %>% left_join(games, by=c("team"="t1")) %>% 
    left_join(games, by=c("team"="t2")) %>% 
    mutate(opponent = coalesce(t1, t2)) %>% 
    select(-t1, -t2) %>% 
    mutate(venue = if_else(is.na(s1.x), "a", "h")) %>% 
    mutate(time_stamp = coalesce(time_stamp.x, time_stamp.y)) %>% 
    mutate(score = if_else(venue=="h", str_c(s1.x, "-", s2.x), str_c(s2.y, "-", s1.y))) %>% 
    mutate(p = coalesce(p.x, 1-p.y)) %>% 
    mutate(delta = coalesce(delta.x, -delta.y)) %>% 
    select(-contains(".")) %>% 
    mutate(rank_old = min_rank(desc(rating+delta))) %>%
    mutate(rank = row_number()) %>% 
    mutate(rank_ch = rank - rank_old) %>% 
    select(rank, rank_ch, team, rating, delta, opponent, venue, score, p, time_stamp, h)
}
