library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
##functions
#names
name_p <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@name_display_first_last")
  val <- trimws(xml_text(a))
  return(toupper(val))
}
#OUT
out <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@out")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BB
bb <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@bb")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#H
h <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@h")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#HR
hr <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@hr")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#R
r <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@r")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#ER
er <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@er")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#BK
bk <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@bk")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#SO
so <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher/@so")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}
#win
w <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "win")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#loss
l <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "loss")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#hold
hd <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "hold")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
#save
sv <- function(x){
  a <- xml_find_all(x, "/boxscore/team/pitching/pitcher")
  b <- xml_attr(a, "save")
  
  for (i in 1:length(b)){
    if (is.na(b[i])){
      b[i] <- 0
    }else if (b[i] == 'true'){
      b[i] <- 1
    }
  }
  b <- as.integer(b)
  return(b)
}
##master function
py_st_pt <- function(x){
  
  game <- data.frame(Player_Name = name_p(x),
                     OUT = out(x),
                     H = h(x),
                     HR = hr(x),
                     R = r(x),
                     ER = er(x),
                     SO = so(x),
                     BB = bb(x),
                     BK = bk(x),
                     W = w(x),
                     L = l(x),
                     HLD = hd(x),
                     SV = sv(x)
  )
  return(game)
}
##Example
exa <- read_xml("http://gd.mlb.com/components/game/aaa/year_2017/month_06/day_15/gid_2017_06_15_oaxaaa_pueaaa_1/rawboxscore.xml")
exascr <- py_st_pt(exa)