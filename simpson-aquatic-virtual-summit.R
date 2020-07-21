## ----setup-options, echo = FALSE, results = "hide", message = FALSE-----------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(cache = TRUE, dev = 'svg', echo = FALSE, message = FALSE, warning = FALSE,
                      fig.height=6, fig.width = 1.777777*6)

library('here')
library('readr')
library('readxl')
library('dplyr')
library('tibble')
library('tidyr')
library('ggplot2')
library('cowplot')

## plot defaults
theme_set(theme_minimal(base_size = 16, base_family = 'Fira Sans'))


## ----high-resilience----------------------------------------------------------
knitr::include_graphics(here('resources', 'scheffer-2012-high-resilience.jpg'))


## ----low-resilience-----------------------------------------------------------
knitr::include_graphics(here('resources', 'scheffer-2012-low-resilience.jpg'))


## ----moving-window------------------------------------------------------------
knitr::include_graphics(here('resources', 'dakos-rolling-window-figure-modified.png'))


## ----lake-227-pigment-data----------------------------------------------------
## Load data from ~/work/data/ela/lake227
lake227 <- read_excel('~/work/data/ela/lake227/CONC227.xlsx')
## Peter highlighted Fuco, Allox, Lutzeax, Pheo_b, Bcarot
vars <- c('YEAR', 'FUCO', 'ALLOX', 'LUTZEAX', 'PHEO_B', 'BCAROT')#, 'ECHINENO', 'MYXO')
lake227 <- lake227[, vars]
names(lake227) <- c('Year', 'Fucoxanthin', 'Alloxanthin', 'LuteinZeaxanthin',
                    'Pheophytinb', 'BetaCarotene')#, 'Echinenone', 'Myxoxnthophyll')
## take data from 1943 onwards
lake227 <- subset(lake227, Year >= 1943)
lake227 <- as_tibble(lake227)
## to long format for modelling
lake227 <- gather(lake227, key = Pigment, value = Concentration, - Year)
lake227 <- lake227 %>%
    mutate(Pigment = as.character(Pigment),
           Pigment = case_when(
               Pigment == 'BetaCarotene' ~ 'beta ~ carotene',
               Pigment == 'Pheophytinb' ~ 'Pheophytin ~ italic(b)',
               Pigment == 'LuteinZeaxanthin' ~ 'atop(Lutein, Zeaxanthin)',
               TRUE ~ Pigment),
           )

ggplot(lake227, aes(x = Year, y = Concentration, group = Pigment, colour = Pigment)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    facet_grid(Pigment ~ ., scales = 'free_y', labeller = 'label_parsed') +
    theme(legend.position = 'none',
          strip.text.y = element_text(size = 14, angle = 0),
          strip.background = element_rect(colour = '#fdb338', fill = '#fdb338')) +
    labs(x = NULL, y = expression(Concentration ~ (italic(n) * g ~ g^{-1})))


## ----gaussian-distributions-plt-----------------------------------------------
x <- seq(8, -8, length = 500)
df <- data.frame(density = c(dnorm(x, 0, 1), dnorm(x, 0, 2), dnorm(x, 2, 1), dnorm(x, -2, 1)),
                 x = rep(x, 4),
                 distribution = factor(rep(c("mean = 0; var = 1", "mean = 0; var = 4",
                                             "mean = 2; var = 1", "mean = -2; var = 1"), each = 500),
                                       levels = c("mean = 0; var = 1", "mean = 0; var = 4",
                                                  "mean = 2; var = 1", "mean = -2; var = 1")))
plt1 <- ggplot(subset(df, distribution %in% c("mean = 0; var = 1", "mean = 0; var = 4")),
               aes(x = x, y = density, colour = distribution)) +
    geom_line(size = 1) + theme(legend.position = "top") +
    guides(col = guide_legend(title = "Distribution", nrow = 2, title.position = "left")) +
    labs(x = 'x', y = "Probability density")

plt2 <- ggplot(subset(df, distribution %in% c("mean = 2; var = 1", "mean = -2; var = 1")),
               aes(x = x, y = density, colour = distribution)) +
    geom_line(size = 1) + theme(legend.position = "top") +
    guides(col = guide_legend(title = "Distribution", nrow = 2, title.position = "left")) +
    labs(x = 'x', y = "Probability density")

plt <- plot_grid(plt2, plt1, ncol = 2, align = "vh")
plt


## ----gamma-dist---------------------------------------------------------------
shape <- c(2,2,3,3,5,5)
scale <- c(2,4,2,1,1,0.5)
x <- seq(0, 20, length.out = 500)
df <- as_tibble(as.data.frame(mapply(dgamma, shape, scale, MoreArgs = list(x = x))))
names(df) <- paste('shape =', shape, ', scale =', scale)
df <- add_column(df, x = x, row = seq_len(nrow(df)))
df <- gather(df, 'distribution', 'density', -row, -x)
plt <- ggplot(df, aes(x = x, y = density, colour = distribution)) +
    geom_line(lwd = 1) + guides(colour = "none") +
    scale_color_discrete(name = "") + ylim(0, 1.5) +
    labs(y = "Probability Density")
plt


## ----load-lake-227-results----------------------------------------------------
m2_summ <- read_rds(here('data', 'lake-227-m2-model-output.rds')) %>%
    mutate(Pigment = as.character(Pigment),
           Pigment = case_when(
               Pigment == 'BetaCarotene' ~ 'beta ~ carotene',
               Pigment == 'Pheophytinb' ~ 'Pheophytin ~ italic(b)',
               Pigment == 'LuteinZeaxanthin' ~ 'atop(Lutein, Zeaxanthin)',
               TRUE ~ Pigment),
           )


## ----lake-227-fitted-mean, dependson = -1-------------------------------------
ggplot(filter(m2_summ, Parameter == "Mean"),
       aes(x = Year, group = Pigment, fill = Pigment)) +
    geom_ribbon(aes(ymax = upper, ymin = lower, fill = Pigment), alpha = 0.2) +
    geom_line(aes(y = est, colour = Pigment), size = 1) +
    facet_grid(Pigment ~ ., scales = 'free_y', labeller = 'label_parsed') +
    theme(legend.position = 'none',
          strip.text.y = element_text(size = 14, angle = 0),
          strip.background = element_rect(colour = '#fdb338', fill = '#fdb338')) +
    labs(x = NULL, y = expression(Estimated ~ concentration ~ (italic(n) * g ~ g^{-1})))


## ----lake-227-fitted-sigma, dependson = -2------------------------------------
ggplot(filter(m2_summ, Parameter == "Sigma"),
       aes(x = Year, group = Pigment, fill = Pigment)) +
    geom_ribbon(aes(ymax = upper, ymin = lower, fill = Pigment), alpha = 0.2) +
    geom_line(aes(y = est, colour = Pigment), size = 1) +
    facet_grid(Pigment ~ ., scales = 'free_y', labeller = 'label_parsed') +
    theme(legend.position = 'none', strip.text.y = element_text(size = 14, angle = 0),
          strip.background = element_rect(colour = '#fdb338', fill = '#fdb338')) +
    labs(x = NULL, y = expression(Estimated ~ sigma ~ (italic(n) * g ~ g^{-1})))

