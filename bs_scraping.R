##functions
#date
date <- function(x){
  a <- xml_find_all(x, "/boxscore/@game_id")
  val <- trimws(xml_text(a))
  val <- substring(val,1,10)
  val <- gsub("/","-",val)
  return(val)
}
#away team
awayteam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[1]))
  return(toupper(val))
}
#home team
hometeam <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@team_code")
  val <- trimws(xml_text(a[2]))
  return(toupper(val))
}
#home wins
home_wins <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@wins")
  val <- trimws(xml_text(a[2]))
  val <- as.integer(val)
  return(val)
}
#home losses
home_losses <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@losses")
  val <- trimws(xml_text(a[2]))
  val <- as.integer(val)
  return(val)
}
#away wins
away_wins <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@wins")
  val <- trimws(xml_text(a[1]))
  val <- as.integer(val)
  return(val)
}
#away losses
away_losses <- function(x){
  a <- xml_find_all(x, "/boxscore/team/@losses")
  val <- trimws(xml_text(a[1]))
  val <- as.integer(val)
  return(val)
}
#attendance
attendance <- function(x){
  a <- xml_find_all(x, "/boxscore/@attendance")
  val <- trimws(xml_text(a))
  val <- as.integer(gsub(",","", val))
  return(val)
}
#time
time <- function(x){
  a <- xml_find_all(x, "/boxscore/@elapsed_time")
  val <- trimws(xml_text(a))
  val <- substring(val,1,4)
  val <- gsub(" ","",paste(val,":00"))
  val <- 60 * 24 * as.numeric(times(val))
  return(val)
}
#runs home
home_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#runs away
away_runs <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_runs")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#errors home
home_errors <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@home_team_errors")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#errors away
away_errors <- function(x){
  a <- xml_find_all(x, "/boxscore/linescore/@away_team_errors")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
##master function
box <- function(x){
  date <- date(x)
  at <- awayteam(x)
  ht <- hometeam(x)
  att <- attendance(x)
  tim <- time(x)
  ar <- away_runs(x)
  ah <- away_hits(x)
  hr <- home_runs(x)
  hh <- home_hits(x)
  aw <- away_wins(x)
  al <- away_losses(x)
  hw <- home_wins(x)
  hl <- home_losses(x)
  ea <- away_errors(x)
  eh <- home_errors(x)
  game <- data.frame(DATE = date,
                     AWAY=at,
                     aR = ar,aH = ah,aE = ea,
                     AW = aw,AL = al,
                     HOME=ht,
                     hR = hr,hH = hh,hE = eh,
                     HW = hw,HL=hl,
                     ATT=att,TIME=tim
  )
  return(game)
}
##scoreborad example
exa <- read_xml("http://gd.mlb.com/components/game/aaa/year_2017/month_06/day_15/gid_2017_06_15_oaxaaa_pueaaa_1/rawboxscore.xml")
exascr <- box(exa)
write.csv(exascr,"lmb_boxscore.csv")