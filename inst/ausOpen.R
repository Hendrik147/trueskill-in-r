  # This second example runs Trueskill on a tennis tournament, the Australian Open.
  # Note that actual computation is commented out as it takes about ~40 seconds to 
  # update skill ratings over 127 matches.
  
#   library(trueskill) 

  data("ausopen2012")
  
  aus.open <- data
  aus.open$match_id <- row.names(aus.open)
  
  # reshape wide to long on match_id such that we have
  # 2 rows per match, 1 with Player1 as Player and 1 with 
  # Player2 as Opponent and vice versa.
  
  aus.open <- aus.open[c("Winner", "Loser", "Round", "WRank", "LRank")]
  aus.open <- reshape(aus.open,
                  idvar = "match_id",
                  varying = list(c(1, 2), c(2, 1), c(4, 5), c(5,4)),
                  v.names = c("Player", "Opponent", "WRank", "LRank"),
                  new.row.names = 1:1000, 
                  timevar = "t",
                  direction = "long")
    
  # aus.open comes preformatted with winner in Player column
  # set margin to 1 for win and -1 for loss.
  
  aus.open$margin[aus.open$t == "1"] <- 1
  aus.open$margin[aus.open$t != "1"] <- -1
  aus.open$t <- NULL
  
  aus.open$mu1 <- NA
  aus.open$sigma1 <- NA
  aus.open$mu2 <- NA
  aus.open$sigma2 <- NA
  
  # For the first round, set Mu to 300 less the ATP rank
  # Skill tends to be stable at the higher rankings (descending from 1), 
  # so set sigma at mu less mu / 3, rather than the recommended mu / 3
                                  
  aus.open[c("mu1","sigma1")] <- c(300 - aus.open$WRank, round(300 - aus.open$WRank - ((300 - aus.open$WRank) / 3), 1))
  aus.open[c("mu2","sigma2")] <- c(300 - aus.open$LRank, round(300 - aus.open$LRank - ((300 - aus.open$WRank) / 3), 1)) 
  
  aus.open[!aus.open$Round == "1st Round",][c("mu1","sigma1")] <- c(NA, NA)
  aus.open[!aus.open$Round == "1st Round",][c("mu2","sigma2")] <- c(NA, NA)

  #   aus.open %>% filter(match_id == 21 & Opponent == "Serra F.") %>% mutate( margin = -1)
  library(dplyr)
  aus.open <- aus.open %>% mutate( Player = replace(Player, match_id == 21 & Opponent == "Serra F.", "Serra F."))
  aus.open <- aus.open %>% mutate( Opponent = replace(Opponent, match_id == 21 & Opponent == "Serra F.", "Darcis S."))
  parameters <- Parameters()
  
  # Trueskill expects aus.open with columns mu1, sigma1, mu2 and sigma2, 
  # will set mu and sigma to 25 and 25 / 3 if NA.
  
  skills <- Trueskill(aus.open, parameters)
  top4 <- subset(skills, Player == "Djokovic N." | Player == "Nadal R." | 
                 Player == "Federer R." | Player == "Murray A." )
  top4 <- top4[order(top4$Player,top4$Round),]
  subset(top4, Player == "Djokovic N.")      
  subset(top4, Player == "Federer R.")      

  #   
  # For a visualisation, load up our favourite package ggplot2...  
  library(ggplot2)
  g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + 
  geom_point(aes(colour=factor(Player))) + geom_line(aes())       
  g1
  
  library(data.table)
  top4 <- data.table(top4)
  top4.sim <- top4[, .(dd = rnorm(100000, mean = mu1, sigma1)), by = c("Player", "Round")]

  ggplot(top4.sim, aes(dd)) + geom_density( aes(colour = Player)) + facet_grid( Round ~ .) 
  #   ggplot(top4) + stat_function(top4, fun = dnorm, args = list(mean = mu1, sd = sigma1)) 

  # Without having adjusted the input parameters, Trueskill does not predict 
  # match outcomes well, as it appears that facing stiffer opposition
  # (higher skilled players) tends to diminish a player's chances of
  # progressing in the subsequent round.
  
  # This is consistent with commentators describing players with softer draws and 
  # playing shorter matches (3 sets as opposed to 5 sets) as being 
  # fresher in later rounds.          
  
  # The other feature is that the skill of the better players is weighted 
  # towards the losing player even if the better player wins, so we have
  # this effect of the 4 semifinalists having their skills dropping as the 
  # tournament progresses. This could be symptomatic of high starting values,
  # which is necessary due to some of the very low rankings. 
  # E.g Lleyton Hewitt with 181.
