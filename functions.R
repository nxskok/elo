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
  print(glue::glue("n = {n}"))
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

