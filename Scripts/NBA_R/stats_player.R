rm(list=ls())


#Librerias

library(ggplot2)
library(tidyverse)
#install.packages("nbastatR")
library(nbastatR)
library(dplyr)

#Si se presenta algun problema al correr funcion de nbastatR, descomentar la siguiente linea de Sys
Sys.setenv("VROOM_CONNECTION_SIZE" = 999999)


### PLAYER STATS
# Panel que contiene las estadisticas de los jugadores de las temporadas 2011 a 2022, cuenta con estadisticas por juego, avanzadas y totales

player_stats <- bref_players_stats(seasons = 2011:2022, tables = c("per_game","advanced", "totals"), join_data = T)


## Modificacion de panel para mejor uso
# Recodificando para solo tener 5 categorias para posicion
nba_data <- mutate( player_stats, position = recode( slugPosition, "SF-SG" = "SF", "PG-SG" = "PG", "C-PF" = "C", "PF-SF" = "PF", "SG-PG" = "SG", "SF-PF"= "SF", "SG-SF" = "SG", "PF-C" = "PF", "SG-PF"= "SG", "SF-C"= "SF", "SG-PG-SF" = "SG")) %>%
  rename(name = namePlayer) %>%
  mutate( player_stats, team = recode (slugTeamBREF, "CHO" = "CHA", "NJN" = "BRK" , "NOH" = "NOP") )%>%
  filter(nchar(slugTeamsBREF) <= 3) %>%
  # Seleccionando solo columnas relevantes
  select( name, yearSeason, agePlayer, position, team, countGames, pctFG, pctFG3, pctFG2, pctEFG, pctFT,idPlayerNBA,
          minutesPerGame,fgmPerGame, fgaPerGame,fg3mPerGame,fg3aPerGame,fg2mPerGame,fg2aPerGame,ftmPerGame,ftaPerGame,orbPerGame,drbPerGame,trbPerGame,astPerGame,stlPerGame,blkPerGame,tovPerGame,pfPerGame,ptsPerGame,
          minutes,ratioPER,pctTrueShooting,pct3PRate,pctFTRate,pctORB,pctDRB,pctTRB,pctAST,pctSTL,pctBLK,pctTOV,pctUSG,ratioOWS,ratioDWS,ratioWS,ratioWSPer48,ratioOBPM,ratioDBPM,ratioBPM,ratioVORP,
          minutesTotals,fgmTotals,fgaTotals,fg3mTotals,fg3aTotals,fg2mTotals,fg2aTotals,ftmTotals,ftaTotals,orbTotals,drbTotals,trbTotals,astTotals,stlTotals,blkTotals,tovTotals,pfTotals,ptsTotals)
  


# Creando dataframes por cada posicion del jugador

unique_pos <- unique(nba_data$position)
for (posicion in unique_pos) {
  player_stat <- paste("player_stat", posicion, sep = "_")
  assign(player_stat, subset(nba_data, position == posicion))
}


### Guardando los paneles

#Especificar la ruta donde se guardara
ruta_carpeta <- "C:/Users/famwa/Tesina/NBA/Data/Panel"

write.csv(nba_data, file.path(ruta_carpeta, "player_stats_panel.csv"), row.names = FALSE)
write.csv(player_stat_C, file.path(ruta_carpeta, "player_stat_C_panel.csv"), row.names = FALSE)
write.csv(player_stat_PF, file.path(ruta_carpeta, "player_stat_PF_panel.csv"), row.names = FALSE)
write.csv(player_stat_PG, file.path(ruta_carpeta, "player_stat_PG_panel.csv"), row.names = FALSE)
write.csv(player_stat_SF, file.path(ruta_carpeta, "player_stat_SF_panel.csv"), row.names = FALSE)
write.csv(player_stat_SG, file.path(ruta_carpeta, "player_stat_SG_panel.csv"), row.names = FALSE)

ruta_carpeta <- "C:/Users/famwa/Tesina/NBA/Data/Stats"

#Guardando por año
unique_pos <- unique(nba_data$yearSeason)
for (posicion in unique_pos) {
  player_stat <- paste("player_stat", posicion, sep = "_")
  assign( player_stat, subset(complete_data_teams, yearSeason == posicion))
}

ruta_carpeta <- "C:/Users/famwa/Tesina/NBA/Data/Stats"

write.csv(nba_data, file.path(ruta_carpeta, "player_stat_panel.csv"), row.names = FALSE)
write.csv(player_stat_2011, file.path(ruta_carpeta, "player_stat_2011.csv"), row.names = FALSE)
write.csv(player_stat_2012, file.path(ruta_carpeta, "player_stat_2012.csv"), row.names = FALSE)
write.csv(player_stat_2013, file.path(ruta_carpeta, "player_stat_2013.csv"), row.names = FALSE)
write.csv(player_stat_2014, file.path(ruta_carpeta, "player_stat_2014.csv"), row.names = FALSE)
write.csv(player_stat_2015, file.path(ruta_carpeta, "player_stat_2015.csv"), row.names = FALSE)
write.csv(player_stat_2016, file.path(ruta_carpeta, "player_stat_2016.csv"), row.names = FALSE)
write.csv(player_stat_2017, file.path(ruta_carpeta, "player_stat_2017.csv"), row.names = FALSE)
write.csv(player_stat_2018, file.path(ruta_carpeta, "player_stat_2018.csv"), row.names = FALSE)
write.csv(player_stat_2019, file.path(ruta_carpeta, "player_stat_2019.csv"), row.names = FALSE)
write.csv(player_stat_2020, file.path(ruta_carpeta, "player_stat_2020.csv"), row.names = FALSE)
write.csv(player_stat_2021, file.path(ruta_carpeta, "player_stat_2021.csv"), row.names = FALSE)
write.csv(player_stat_2022, file.path(ruta_carpeta, "player_stat_2022.csv"), row.names = FALSE)



### Teams Stats

teams_stats <- bref_teams_stats( seasons = 2011:2022, widen_data = FALSE)


#nba_team_data_all <- mutate( dataBREFTeamJoined, nteam = recode( nameTeam, "Charlotte Bobcats" = "Charlotte Hornets", "New Orleans Hornets" = "New Orleans Pelicans", "New Jersey Nets" = "Brooklyn Nets"))%>%
#  mutate( dataBREFTeamJoined, team = recode (slugTeamBREF, "CHO" = "CHA", "NJN" = "BRK" , "NOH" = "NOP") )%>%
#  select( yearSeason, team, nteam, winsTeam, lossesTeam, pctWins, gamesBehind1DivisionStandingsDiv, ptsOppPerGameStandingsDiv, 
#         ptsTeamPerGameStandingsDiv, ratingStrengthOfScheduleStandingsDiv, gamesBehind1ConferenceStandingsConf, ptsOppPerGameStandingsConf, 
#         ptsTeamPerGameStandingsConf, ratingStrengthOfScheduleStandingsConf, astPerGameOpponent, astPerGameTeam, blkPerGameOpponent, 
#         blkPerGameTeam, drbPerGameOpponent, drbPerGameTeam, fg2aPerGameOpponent, fg2aPerGameTeam, fg2mPerGameOpponent, fg2mPerGameTeam, 
#         fg3aPerGameOpponent, fg3aPerGameTeam, fg3mPerGameOpponent, fg3mPerGameTeam, fgaPerGameOpponent, fgaPerGameTeam, fgmPerGameOpponent, 
#         fgmPerGameTeam, ftaPerGameOpponent, ftaPerGameTeam, ftmPerGameOpponent, ftmPerGameTeam, minutesPerGameOpponent, minutesPerGameTeam, 
#         numberPlayerPerGameOpponent, numberPlayerPerGameTeam, orbPerGameOpponent, orbPerGameTeam, pctFG2PerGameOpponent, pctFG2PerGameTeam, 
#         pctFG3PerGameOpponent, pctFG3PerGameTeam, pctFGPerGameOpponent, pctFGPerGameTeam, pctFTPerGameOpponent, pctFTPerGameTeam, pfPerGameOpponent, 
#        pfPerGameTeam, ptsPerGameOpponent, ptsPerGameTeam, stlPerGameOpponent, stlPerGameTeam, tovPerGameOpponent, tovPerGameTeam, 
#         trbPerGameOpponent, trbPerGameTeam, astTotalsOpponent, astTotalsTeam, blkTotalsOpponent, blkTotalsTeam, drbTotalsOpponent, 
#         drbTotalsTeam, fg2aTotalsOpponent, fg2aTotalsTeam, fg2mTotalsOpponent, fg2mTotalsTeam, fg3aTotalsOpponent, fg3aTotalsTeam, 
#         fg3mTotalsOpponent, fg3mTotalsTeam, fgaTotalsOpponent, fgaTotalsTeam, fgmTotalsOpponent, fgmTotalsTeam, ftaTotalsOpponent, 
#         ftaTotalsTeam, ftmTotalsOpponent, ftmTotalsTeam, minutesTotalsOpponent, minutesTotalsTeam, numberPlayerTotalsOpponent, 
#         numberPlayerTotalsTeam, orbTotalsOpponent, orbTotalsTeam, pctFG2TotalsOpponent, pctFG2TotalsTeam, pctFG3TotalsOpponent, 
#         pctFG3TotalsTeam, pctFGTotalsOpponent, pctFGTotalsTeam, pctFTTotalsOpponent, pctFTTotalsTeam, pfTotalsOpponent, pfTotalsTeam, 
#         ptsTotalsOpponent, ptsTotalsTeam, stlTotalsOpponent, stlTotalsTeam, tovTotalsOpponent, tovTotalsTeam, trbTotalsOpponent, 
#         trbTotalsTeam, astPerPossTeam, blkPerPossTeam, drbPerPossTeam, fg2aPerPossTeam, fg2mPerPossTeam, fg3aPerPossTeam, 
#         fg3mPerPossTeam, fgaPerPossTeam, fgmPerPossTeam, ftaPerPossTeam, ftmPerPossTeam, minutesPerPossTeam, numberPlayerPerPossTeam, 
#        orbPerPossTeam, pctFG2PerPossTeam, pctFG3PerPossTeam, pctFGPerPossTeam)





## Final db of teams

# Panel más condensado

nba_team_data <- mutate( dataBREFTeamJoined, nteam = recode( nameTeam, "Charlotte Bobcats" = "Charlotte Hornets", "New Orleans Hornets" = "New Orleans Pelicans", "New Jersey Nets" = "Brooklyn Nets"))%>%
  mutate( dataBREFTeamJoined, team = recode (slugTeamBREF, "CHO" = "CHA", "NJN" = "BRK" , "NOH" = "NOP") )%>%
  select( yearSeason, team, nteam, winsTeam, lossesTeam, pctWins)%>%
  slice(1:360)

## Team salaries
setwd("C:/Users/famwa/Tesina/NBA/Scripts/NBA_R")
team_finance <- read.csv("teams_finance.csv")


complete_data_teams <- merge(nba_team_data, team_finance, by = c("team", "yearSeason"), all.x = TRUE)

### Guardando los paneles

unique_pos <- unique(complete_data_teams$yearSeason)
for (posicion in unique_pos) {
  team_victories <- paste("team_victories", posicion, sep = "_")
  assign( team_victories, subset(complete_data_teams, yearSeason == posicion))
}


### Guardando los paneles

#Especificar la ruta donde se guardara
ruta_carpeta <- "C:/Users/famwa/Tesina/NBA/Data/Teams"

write.csv(complete_data_teams, file.path(ruta_carpeta, "team_victories_panel.csv"), row.names = FALSE)
write.csv(team_victories_2011, file.path(ruta_carpeta, "team_victories_2011.csv"), row.names = FALSE)
write.csv(team_victories_2012, file.path(ruta_carpeta, "team_victories_2012.csv"), row.names = FALSE)
write.csv(team_victories_2013, file.path(ruta_carpeta, "team_victories_2013.csv"), row.names = FALSE)
write.csv(team_victories_2014, file.path(ruta_carpeta, "team_victories_2014.csv"), row.names = FALSE)
write.csv(team_victories_2015, file.path(ruta_carpeta, "team_victories_2015.csv"), row.names = FALSE)
write.csv(team_victories_2016, file.path(ruta_carpeta, "team_victories_2016.csv"), row.names = FALSE)
write.csv(team_victories_2017, file.path(ruta_carpeta, "team_victories_2017.csv"), row.names = FALSE)
write.csv(team_victories_2018, file.path(ruta_carpeta, "team_victories_2018.csv"), row.names = FALSE)
write.csv(team_victories_2019, file.path(ruta_carpeta, "team_victories_2019.csv"), row.names = FALSE)
write.csv(team_victories_2020, file.path(ruta_carpeta, "team_victories_2020.csv"), row.names = FALSE)
write.csv(team_victories_2021, file.path(ruta_carpeta, "team_victories_2021.csv"), row.names = FALSE)
write.csv(team_victories_2022, file.path(ruta_carpeta, "team_victories_2022.csv"), row.names = FALSE)










# Esta base contiene todas las estadistica por juego, por posesión, etc

#nba_team_data_all <- mutate( dataBREFTeamJoined, nteam = recode( nameTeam, "Charlotte Bobcats" = "Charlotte Hornets", "New Orleans Hornets" = "New Orleans Pelicans", "New Jersey Nets" = "Brooklyn Nets"))%>%
#  mutate( dataBREFTeamJoined, team = recode (slugTeamBREF, "CHO" = "CHA", "NJN" = "BRK" , "NOH" = "NOP") )%>%
#  select( yearSeason, team, nteam, winsTeam, lossesTeam, pctWins, gamesBehind1DivisionStandingsDiv, ptsOppPerGameStandingsDiv, 
#         ptsTeamPerGameStandingsDiv, ratingStrengthOfScheduleStandingsDiv, gamesBehind1ConferenceStandingsConf, ptsOppPerGameStandingsConf, 
#         ptsTeamPerGameStandingsConf, ratingStrengthOfScheduleStandingsConf, astPerGameOpponent, astPerGameTeam, blkPerGameOpponent, 
#         blkPerGameTeam, drbPerGameOpponent, drbPerGameTeam, fg2aPerGameOpponent, fg2aPerGameTeam, fg2mPerGameOpponent, fg2mPerGameTeam, 
#         fg3aPerGameOpponent, fg3aPerGameTeam, fg3mPerGameOpponent, fg3mPerGameTeam, fgaPerGameOpponent, fgaPerGameTeam, fgmPerGameOpponent, 
#         fgmPerGameTeam, ftaPerGameOpponent, ftaPerGameTeam, ftmPerGameOpponent, ftmPerGameTeam, minutesPerGameOpponent, minutesPerGameTeam, 
#         numberPlayerPerGameOpponent, numberPlayerPerGameTeam, orbPerGameOpponent, orbPerGameTeam, pctFG2PerGameOpponent, pctFG2PerGameTeam, 
#         pctFG3PerGameOpponent, pctFG3PerGameTeam, pctFGPerGameOpponent, pctFGPerGameTeam, pctFTPerGameOpponent, pctFTPerGameTeam, pfPerGameOpponent, 
#         pfPerGameTeam, ptsPerGameOpponent, ptsPerGameTeam, stlPerGameOpponent, stlPerGameTeam, tovPerGameOpponent, tovPerGameTeam, 
#         trbPerGameOpponent, trbPerGameTeam, astTotalsOpponent, astTotalsTeam, blkTotalsOpponent, blkTotalsTeam, drbTotalsOpponent, 
#         drbTotalsTeam, fg2aTotalsOpponent, fg2aTotalsTeam, fg2mTotalsOpponent, fg2mTotalsTeam, fg3aTotalsOpponent, fg3aTotalsTeam, 
#         fg3mTotalsOpponent, fg3mTotalsTeam, fgaTotalsOpponent, fgaTotalsTeam, fgmTotalsOpponent, fgmTotalsTeam, ftaTotalsOpponent, 
#         ftaTotalsTeam, ftmTotalsOpponent, ftmTotalsTeam, minutesTotalsOpponent, minutesTotalsTeam, numberPlayerTotalsOpponent, 
#         numberPlayerTotalsTeam, orbTotalsOpponent, orbTotalsTeam, pctFG2TotalsOpponent, pctFG2TotalsTeam, pctFG3TotalsOpponent, 
#         pctFG3TotalsTeam, pctFGTotalsOpponent, pctFGTotalsTeam, pctFTTotalsOpponent, pctFTTotalsTeam, pfTotalsOpponent, pfTotalsTeam, 
#         ptsTotalsOpponent, ptsTotalsTeam, stlTotalsOpponent, stlTotalsTeam, tovTotalsOpponent, tovTotalsTeam, trbTotalsOpponent, 
#         trbTotalsTeam, astPerPossTeam, blkPerPossTeam, drbPerPossTeam, fg2aPerPossTeam, fg2mPerPossTeam, fg3aPerPossTeam, 
#         fg3mPerPossTeam, fgaPerPossTeam, fgmPerPossTeam, ftaPerPossTeam, ftmPerPossTeam, minutesPerPossTeam, numberPlayerPerPossTeam, 
#        orbPerPossTeam, pctFG2PerPossTeam, pctFG3PerPossTeam, pctFGPerPossTeam)
  



























