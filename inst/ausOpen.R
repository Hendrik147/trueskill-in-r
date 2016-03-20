library(dplyr)
library(magrittr)
library(ggplot2)
#   library(trueskill) 

example <- function() {

  #   library(devtools)
  #   dev_mode()
  #   load_all("../")

  data("ausopen2012")

  aus.open <- ausopen2012
  aus.open$match_id <- row.names(aus.open)

  aus.open %<>% select(Player = Winner, Opponent = Loser, Round, WRank, LRank, match_id) %>% 
                mutate(margin = 1, Player = as.character(Player), Opponent = as.character(Opponent))
  
  aus.open.otherway <- copy(aus.open)
  aus.open.otherway %<>% mutate(Player.temp = Opponent, Opponent = Player, margin = -1) %>% mutate(Player = Player.temp) %>% select(-Player.temp)

  aus.open <- rbind(aus.open, aus.open.otherway)

  base.top <- 300
  aus.open %<>% mutate( mu1 = base.top - WRank) %>% 
                mutate( sigma1 = mu1 - mu1/3)
  aus.open %<>% mutate( mu2 = base.top - LRank) %>% 
                mutate( sigma2 = mu2 - mu2/3)

  first.round <- "1st Round"

  replaceNonFirstRound <- function(value, round) {
    replace(value, round != first.round, NA)
  }

  aus.open %<>% mutate(mu1 = replaceNonFirstRound(mu1, Round), 
                       sigma1 = replaceNonFirstRound(sigma1, Round), 
                       mu2 = replaceNonFirstRound(mu2, Round),
                       sigma2= replaceNonFirstRound(sigma2, Round))

  # Error in the data!
  aus.open %<>% mutate( Player = replace(Player, match_id == 21 & Opponent == "Serra F.", "Serra F."))
  aus.open %<>% mutate( Opponent = replace(Opponent, match_id == 21 & Opponent == "Serra F.", "Darcis S."))

  parameters <- Parameters()

  # Trueskill expects aus.open with columns mu1, sigma1, mu2 and sigma2, 
  # will set mu and sigma to 25 and 25 / 3 if NA.

  skills <- Trueskill(aus.open, parameters)
  top4 <- subset(skills, Player == "Djokovic N." | Player == "Nadal R." | 
                 Player == "Federer R." | Player == "Murray A." )
  top4 <- top4[order(top4$Player,top4$Round),]
  subset(top4, Player == "Djokovic N.")      
  subset(top4, Player == "Federer R.")      

  g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + 
  geom_point(aes(colour=factor(Player))) + geom_line(aes())       
  g1

  n <- 1000
  top4.sim <- top4 %>% group_by(Player, Round) %>% sample_n(n, TRUE) %>% mutate( dd = rnorm(n, mean = mu1, sd = sigma1))

  ggplot(top4.sim, aes(dd)) + geom_density( aes(colour = Player)) + facet_grid( Round ~ .) 
}
