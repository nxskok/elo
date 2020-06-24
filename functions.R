elo_prob <- function(d) {
  p <- 1/(1+10^(-d/400))
  p
}
p <- function(rat1, rat2=0, h=0) {
  d <- rat1-rat2+h
  elo_prob(d)
}

make_team_lookup <- function(games) {
  v <- with(games, fct_c(factor(t1), factor(t2)))
  enframe(levels(v), name="id", value="name")
}

mloglik <- function(rat, games, team_lookup) { # minus twice loglik
  # team lookups
  games %>% left_join(team_lookup, by=c("t1"="name")) %>% 
    left_join(team_lookup, by=c("t2"="name")) -> dd
  nt <- nrow(team_lookup)
  dd %>% mutate(d=rat[id.x]-rat[id.y]+rat[nt+1]) %>% 
    mutate(p=elo_prob(d)) %>% 
    mutate(loglik=r*log(p)+(1-r)*log(1-p)) %>% 
    summarize(ll=sum(-2*loglik)) %>% pull(ll)
}

m2 <- function(rat0, games, team_lookup) {
  # rat is n-1 independent ratings with the last one being the home adv
  n <- length(rat0)
  h <- rat0[n]
  target_total <- 1500*n
  actual_total <- sum(rat0[1:(n-1)])
  rat0[n] <- target_total-actual_total
  rat <- c(rat0, h)
  mloglik(rat, games, team_lookup)
}

estimate_initial <- function(games) {
  team_lookup <- make_team_lookup(games)
  n <- nrow(team_lookup)
  # print(glue::glue("n = {n}"))
  rat0 <- c(rep(1500, n-1), 0)
  ans <- optim(rat0, fn=m2, games=games, team_lookup=team_lookup)
  rat0 <- ans$par
  # reconstitute
  h <- rat0[n]
  target_total <- 1500*n
  actual_total <- sum(rat0[1:(n-1)])
  rat0[n] <- target_total - actual_total
  team_lookup %>% mutate(rat=rat0, h=h)
}

initial_from_comp <- function(comp, games) {
  d1 <- get_clean_games(comp, games)
  r1 <- estimate_initial(d1)
  pb$tick()
  r1
}

initial_to_pre <- function(with_initial, want, new_teams) {
  with_initial %>% filter(league==want) -> d
  d %>% pluck("initial", 1) -> initial
  d %>% pluck("gone", 1) -> gone
  d %>% pluck("new", 1) -> new
  initial %>% anti_join(gone, by=c("name"="team")) -> dd
  dd %>% summarise(min_rat=min(rat), max_rat=max(rat)) -> extremes
  new %>% left_join(new_teams) %>% 
    filter(league==want) %>% # just to be safe
    mutate(rat=case_when(
      start=="l"              ~ extremes$min_rat,
      start=="h"              ~ extremes$max_rat,
      as.numeric(start) > 0   ~ as.numeric(start),
      TRUE                    ~ 1500
    )) %>% 
    select(name=team, rat) %>% 
    bind_rows(dd) %>% 
    arrange(desc(h)) %>% 
    mutate(id=row_number()) %>% 
    fill(h) -> ddd
  pb$tick()
  ddd
}