new_ratings <- function(k, d, r) {
  # k is the elo k
  # d is a dataframe of the current season's games with teams as t1 and t2, results as r, on a 1-0.5-0 scale
  # r is dataframe of initial teams, ratings and home adv
  # returns updated ratings and home adv, and updated df with predicted probs and deltas
  
  # return(list(k, d, r))
  ratings <- r$rat
  names(ratings) <- r$name
  h <- r$h[1]
  
  d %>% mutate(p=NA, delta=NA) -> d
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
  }
  enframe(ratings, name="team", value="rating") %>% mutate(h=h) -> r
  # print("p from new_ratings:")
  # print(d$p)
  list(d=d, r=r, k=k)
}


log_lik <- function(k, d, r) {
  # print("In loglik")
  # print(list(k=k, d=d, r=r))
  ans <- new_ratings(k, d, r)
  # print("ans from new ratings")
  # print(ans)
  # print("p in loglik:")
  # print(ans$d$p)
  ans$d %>% 
    summarize(ll=-sum(log(p))) %>%  pull(ll)
}


# estimate ratings given k. There is a lot of repetition here.

est_k <- function(k, comp1, comp2) {
  # current ratings for fixed k, estimating the initial ones
  # the_ratings really ought to do this
  games <- readRDS("~/teaching/scoresway/rds/games.rds")
  d1 <- get_clean_games(comp1, games)
  r1 <- estimate_initial(d1)
  d2 <- get_clean_games(comp2, games) 
  with(d2, unique(c(t1, t2))) %>% enframe(name=NULL, value="team") -> teams_this
  teams_this %>% anti_join(r1, by=c("team"="name")) %>% pull(team) -> up # came up
  r1 %>% anti_join(teams_this, by=c("name"="team")) %>% pull(name) -> down # went down
  tibble(up, down) -> changes
  # try to automate changes. Might fail if league changes size. In fact, previous line will probably fail.
  r1 %>% left_join(changes, by=c("name"="down")) %>% 
    mutate(name=ifelse(!is.na(up), up, name)) %>% 
    select(-up) -> r1
  pb$tick()$print()
  log_lik(k, d2, r1)
}


# then optimize for k

best_k <- function(d_initial, d_current) {
  # print("in best_k")
  # print(list(initial=d_initial, current=d_current))
  ans <- optimize(log_lik, c(0, 100), d_current, d_initial)
  # print("optimized k")
  # print(ans$minimum)
  ans$minimum
}

# try for a function to do the whole thing

the_ratings <- function(comp1, comp2) {
  games <- readRDS("~/teaching/scoresway/rds/games.rds")
  d1 <- get_clean_games(comp1, games)
  r1 <- estimate_initial(d1)
  d2 <- get_clean_games(comp2, games) 
  with(d2, unique(c(t1, t2))) %>% enframe(name=NULL, value="team") -> teams_this
  teams_this %>% anti_join(r1, by=c("team"="name")) %>% pull(team) -> up # came up
  r1 %>% anti_join(teams_this, by=c("name"="team")) %>% pull(name) -> down # went down
  # if number of rows different, select number of rows in up: needs thought
  # n <- length(down)
  tibble(up, down) -> changes
  # try to automate changes. Might fail if league changes size. In fact, previous line will probably fail.
  r1 %>% left_join(changes, by=c("name"="down")) %>% 
    mutate(name=ifelse(!is.na(up), up, name)) %>% 
    select(-up) -> r1
  # r1 might now have repeats in it (if more teams went down than came up). Check to see what kind of thing this has.
  # print("After league changes")
  k <- best_k(r1, d2)
  # print("after best k")
  new_ratings(k, d2, r1)
}
