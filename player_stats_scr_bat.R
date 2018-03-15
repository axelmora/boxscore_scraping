library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
##functions
#names
names <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@name_display_first_last")
  val <- trimws(xml_text(a))
  return(toupper(val))
}
#AB
ab <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@ab")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#R
r <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@r")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#H
h <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@h")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#d
d <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@d")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#T
t <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@t")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HR
hr <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hr")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#RBI
rbi <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@rbi")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BB
bb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@bb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SO
so <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@so")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HBP
hbp <- function(x){
  a <- xml_find_all(x, "/boxscore/team/batting/batter/@hbp")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
##master function
py_st_bt <- function(x){

  game <- data.frame(Player_Name = names(x),
                     AB = ab(x),
                     R = r(x),
                     H = h(x),
                     D = d(x),
                     Tr = t(x),
                     HR = hr(x),
                     RBI = rbi(x),
                     BB = bb(x),
                     SO = so(x),
                     HBP = hbp(x)
                     )
  return(game)
}
##Example
exa <- read_xml("http://gd.mlb.com/components/game/aaa/year_2017/month_06/day_15/gid_2017_06_15_oaxaaa_pueaaa_1/rawboxscore.xml")
exascr <- py_st_bt(exa)