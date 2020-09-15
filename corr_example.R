### --------------------------------------------------
### Corr example
### --------------------------------------------------


### --------------------------------------------------
### Libraries
### --------------------------------------------------
library(tidyverse)
library(gganimate)
library(transformr)

theme_set(theme_minimal())

### --------------------------------------------------
### No school like the old school
### --------------------------------------------------

### Direct! Quick! I'd do it this way myself if I
### really had no interest in keeping the data or
### saving any of the plots etc etc

r <- seq(-.99, .99, by=.05)

for(i in 1:length(r)){

  u <- matrix(rnorm(2000,0,1), 1000, 2)
  x <- u%*%chol(matrix(c(1, r[i], r[i], 1), 2, 2))

  plot(x[,1], x[,2], xlim=c(-3,3), ylim=c(-3,3),
       xlab="x1", ylab="x2",
       main=paste("r=",round(r[i],2),"actual=", round(cor(x)[1,2],2)))

  abline(0,r[i])

  Sys.sleep(.2)

}

### --------------------------------------------------
### Meanwhile in Tidytown
### --------------------------------------------------

## There's more than one way to do approach it.


### --------------------------------------------------
### 1. Imitating base R a bit
### --------------------------------------------------

## One option is to stick closely to the base
## version, even though ggplot isn't really
## designed for faking animation by repeatedly
## drawing to a live graphics device. Here's
## half a step away from that. The main difference
## is that instead of a loop we use map() to
## feed our vector of r values to functions,
## and we save the data before graphing it.

## A function to make one matrix of data
## Same as Scott's version except we have an
## extra line to convert it to a tibble (a data frame)
## while not being loud about it.
gen_dat <- function(r) {
  u <- matrix(rnorm(2000,0,1), 1000, 2)
  x <- u %*% chol(matrix(c(1, r, r, 1), 2, 2))
  suppressMessages(as_tibble(x, .name_repair = "universal")) %>%
    rename(x = ...1, y = ...2)
}

## Start with a tibble (a data frame) of correlation values and a
## numerical index of the rows
dat <- tibble(r = seq(-.99, .99, by=.01))

## Generate the data
out <- dat %>%
  mutate(ind = seq_along(r)) %>%
  group_by(r) %>%
  mutate(data = map(r, gen_dat),
         actual = map_dbl(data, ~ cor(.$x, .$y)))

## Animate it
p <- out %>%
  unnest(cols = c(data)) %>%
  group_by(ind) %>%
  ggplot(mapping = aes(x = x, y = y, group = ind)) +
  geom_point(pch = 1, color = "gray40") +
  geom_abline(mapping = aes(intercept = 0, slope = r),
              size = 1.1,
              color = "firebrick") +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-3, 3)) +
  transition_manual(ind) +
  labs(x = "x1",
       y = "x2",
       title = "r = {round(out$r[as.integer(current_frame)], 2)},
                actual = {round(out$actual[as.integer(current_frame)], 2)}")

## Save to a single file
animate(p, fps = 25, duration = 30,
        width = 400, height = 400,
        renderer = gifski_renderer(file = "figures/cor.gif"))


### --------------------------------------------------
### 2. Other things we might do
### --------------------------------------------------

## Here the idea is to tidily save all the plots as objects alongside the table of data
## and figure out what to do with them later.
## I'll repeat some code from above here, so that it's self-contained.

## A function to make one matrix of data
## and quietly convert it to a tibble
gen_dat <- function(r) {
  u <- matrix(rnorm(2000,0,1), 1000, 2)
  x <- u %*% chol(matrix(c(1, r, r, 1), 2, 2))
  suppressMessages(as_tibble(x, .name_repair = "universal")) %>%
    rename(x = ...1, y = ...2)
}

## A function that returns a specific plot as an object
gen_plot <- function(.x, slp = r) {
  corval <- round(cor(.x$x, .x$y), 2)
  p <- ggplot(data = .x,
              mapping = aes(x = x, y = y)) +
    geom_point() +
    geom_abline(mapping = aes(intercept = 0,
                              slope = slp),
                color = "orange",
                size = 1.1) +
    labs(x = "x1",
         y = "x2",
         title = paste("Actual =", corval))
  p
}

## A function to print a plot and wait
show_plot <- function(x, sleep = .2){
  print(x)
  Sys.sleep(sleep)
}

## Now we can generate a sequence of r values ...
## We round to avoid the horrors of double precision math errors
## when filtering
dat <- tibble(r = round(seq(-.99, .99, by=.01), 2))

## ... and use map() to feed the functions to each r value, generating
## a list column of simulated data and a list column of plots.
out <- dat %>%
  mutate(data = map(r, gen_dat),
         imgs = map2(data, r, gen_plot))

## Look what we hath wrought
out

## Look at any particular plot
out %>%
  filter(r == 0.62) %>%
  pull(imgs) %>%
  show_plot()

## Or print them all to the graphics device
## Launch an x11() or quartz() window first
## This is ugly and slow though
show_plot(out$imgs)

## Or save them all out to individual files, to look at later
## or process with imagemagick or whatever
## It's map2() and not map() here because we have two things to do
map2(paste0("figures/cors/r_", out$r, ".png"), out$imgs, ggsave)

