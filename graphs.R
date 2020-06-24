
# display ratings and latest for a league

display <- function(d, g, want) {
  # input d is post, g is updates, want is league name
  d %>% arrange(desc(rating)) %>% 
    mutate(dplyr::across(-team, ~round(., 0))) %>% 
    View(str_c(want, ": ratings"))
  g %>% arrange(desc(time_stamp)) %>% 
    mutate(p=round(p, 2)) %>% 
    mutate(dplyr::across(c(delta, r1, r2), ~round(., 0))) %>% 
    View(str_c(want, ": latest"))
}



# make time graph of ratings

time_graph <- function(lg, df, extra_days=40) {
  # input: data frame like with_post containing updated ratings, league name wanted, extra days on right for team names
  df %>% 
    filter(league==lg) %>% 
    pluck("updates", 1) -> d
  d %>% summarise(first=min(time_stamp), last=max(time_stamp)) -> date_range
  mytitle <- str_c(lg, " ", date_range$last[1]) 
  # date_range
  d %>% select(time_stamp, team=t1, rating=r1) -> d1
  d %>% select(time_stamp, team=t2, rating=r2) -> d2
  bind_rows(d1, d2) -> d3
  d3 %>% arrange(time_stamp) -> d3
  d3 %>% group_by(team) %>% summarise(last_date=max(time_stamp), last_rat=last(rating)) -> lasts
  lines <- crossing(aaa=factor(1:6), bbb=factor(1:5))
  teams <- tibble(team=unique(d3$team))
  lines %>% slice(1:nrow(teams)) %>% 
    bind_cols(teams) -> combos
  d3 %>% left_join(combos) -> d4
  lasts %>% left_join(combos) -> lasts
  ggplot(d4, aes(x=time_stamp, y=rating, label=team)) + 
    geom_line(aes(colour=bbb, linetype=aaa)) + 
    # geom_text(data=lasts, aes(x=last_date, y=last_rat, colour=bbb), hjust="left", vjust="center") +
    geom_point(data=lasts, aes(x=last_date, y=last_rat, colour=bbb)) +
    geom_text_repel(data=lasts, 
      aes(x=last_date, y=last_rat, colour=bbb),
      nudge_x      = 0.15,
      direction    = "y",
      hjust        = 0,
      segment.size = 0.2
    ) +
    guides(colour=F) + guides(linetype=F) +
    xlim(date_range$first[1], date_range$last[1] + extra_days*24*60*60) + # add 30 days for team names
    ggtitle(mytitle)
}