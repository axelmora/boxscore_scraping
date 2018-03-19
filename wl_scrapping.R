library(RCurl)
library(XML)
library(xml2)
library(chron)
library(xts)
library(tidyverse)
library(dygraphs)

##import data for
dias <- c(1:31)

aux <- list()
for(i in c(1,2,3,4,5,6,7,8,9)){
  bx <- read_xml(gsub(" ","",paste("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_08/day_0",i,"/master_scoreboard.xml")))
  aux[[i]] <- wl(bx)
  bxmaster <- rbind(bxmaster,aux[[i]])
}

##import data manual
bx <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_03/day_31/master_scoreboard.xml")
bxmaster <- wl(bx)
bxmaster <- rbind(bxmaster,bxmaster1)

##functions
date <- function(x){
  a <- xml_find_all(x, "/games/game/@id")
  val <- trimws(xml_text(a))
  val <- substring(val,1,10)
  val <- gsub("/","-",val)
  return(val)
}

awayteam <- function(x){
  a <- xml_find_all(x, "/games/game/@away_code")
  val <- trimws(xml_text(a))
  return(toupper(val))
}

hometeam <- function(x){
  a <- xml_find_all(x, "/games/game/@home_code")
  val <- trimws(xml_text(a))
  return(toupper(val))
}

home_wins <- function(x){
  a <- xml_find_all(x, "/games/game/@home_win")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

home_losses <- function(x){
  a <- xml_find_all(x, "/games/game/@home_loss")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_wins <- function(x){
  a <- xml_find_all(x, "/games/game/@away_win")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

away_losses <- function(x){
  a <- xml_find_all(x, "/games/game/@away_loss")
  val <- trimws(xml_text(a))
  val <- as.integer(val)
  return(val)
}

##master function
wl <- function(x){
  date <- date(x)
  at <- awayteam(x)
  ht <- hometeam(x)
  aw <- away_wins(x)
  al <- away_losses(x)
  hw <- home_wins(x)
  hl <- home_losses(x)
  
  records <- data.frame(DATE = date,AWAY=at,HOME=ht,AW = aw,AL = al,HW = hw,HL=hl)
  return(records)
}

bxmaster$HOME <- tolower(bxmaster$HOME) 
bxmaster$AWAY <- tolower(bxmaster$AWAY)

######
write.csv(bxmaster,"bxmaster.csv")

yuch <- sqldf::read.csv.sql("bxmaster",sql="select DATE,HOME as TEAM,HW as W,HL as L from bxmaster where HOME = 'yuc' 
                            order by DATE asc")
yuca <- sqldf::read.csv.sql("bxmaster",sql="select DATE,AWAY as TEAM,AW as W,AL as L from bxmaster where AWAY = 'yuc' 
                            order by DATE asc")
yuc <- rbind(yuch,yuca)
yuc <- sqldf::read.csv.sql("yuc",sql="select * from yuc order by DATE asc")
yuc$yuc <- ((yuc$W/(yuc$W+yuc$L)))

#####
write.csv(agu,"agu.csv")
write.csv(cam,"cam.csv")
write.csv(dur,"dur.csv")
write.csv(leo,"leo.csv")
write.csv(mty,"mty.csv")
write.csv(mva,"mva.csv")
write.csv(mxo,"mxo.csv")
write.csv(oax,"oax.csv")
write.csv(tij,"tij.csv")
write.csv(qui,"qui.csv")
write.csv(vra,"vra.csv")
write.csv(vaq,"vaq.csv")
write.csv(tab,"tab.csv")
write.csv(pue,"pue.csv")
write.csv(slt,"slt.csv")
write.csv(yuc,"yuc.csv")
write.csv(bxmaster,"masterlmb17.csv")
#####

pru <- sqldf::read.csv.sql("mxo",sql="select DATE,MXO from mxo order by DATE asc")
pru <- cbind(as.Date(mxo$DATE,"%Y-%m-%d"),mxo$TEAM)
#ggplot(pru, aes(as.Date(DATE), mxo)) + geom_line() + xlab("") + ylab("Daily Views")
pru.ts <- xts(pru$mxo, order.by = as.Date(pru[,1], "%Y-%m-%d"))
dygraph(pru.ts)

pru2 <- sqldf::read.csv.sql("leo",sql="select DATE,LEO from leo order by DATE asc")
pru2 <- cbind(as.Date(mxo$DATE,"%Y-%m-%d"),mxo$TEAM)
#ggplot(pru, aes(as.Date(DATE), mxo)) + geom_line() + xlab("") + ylab("Daily Views")
pru2.ts <- xts(pru2$leo, order.by = as.Date(pru2[,1], "%Y-%m-%d"))
dygraph(pru2.ts)
#####

library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)


#####
lmb.ts <- xts(lmbts[,-1], order.by = as.Date(lmbts$DATE, "%Y-%m-%d"))
dygraph(lmb.ts)

dygraph(lmb.ts, main = "2017 LMB standings", ylab = "Win/Loss percentage") %>%
  dySeries("mxo", label = "Diablos Rojos") %>%
  dySeries("leo", label = "Bravos") %>%
  dySeries("vra", label = "Rojos del Aguila") %>%
  dySeries("vaq", label = "Vaqueros") %>%
  dySeries("mty", label = "Sultanes") %>%
  dySeries("mva", label = "Acereros") %>%
  dySeries("yuc", label = "Leones") %>%
  dySeries("cam", label = "Piratas") %>%
  dySeries("tab", label = "Olmecas") %>%
  dySeries("pue", label = "Pericos") %>%
  dySeries("oax", label = "Guerreros") %>%
  dySeries("qui", label = "Tigres") %>%
  dySeries("slt", label = "Saraperos") %>%
  dySeries("tij", label = "Toros") %>%
  dySeries("dur", label = "Generales") %>%
  dySeries("agu", label = "Rieleros") %>%
  dyLegend(show = "follow") %>%
  dyLimit(0.5, color = 'red') %>%
  dyRangeSelector()