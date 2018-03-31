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
  wls <- rbind(wls,aux[[i]])
}

##import data manual
bx <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_03/day_31/master_scoreboard.xml")
wls <- wl(bx)
wls <- rbind(wls,wls1)

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

wls$HOME <- tolower(wls$HOME) 
wls$AWAY <- tolower(wls$AWAY)

######
write.csv(wls,"wls.csv")


yuch <- rename(
          select(
            filter(wls, HOME == 'yuc'),
              DATE, HOME,HW,HL),
               TEAM = HOME,
               W = HW,
               L = HL)

yuca <- rename(
          select(
            filter(wls, AWAY == 'yuc'),
              DATE, AWAY,AW,AL),
                TEAM = AWAY,
                W = AW,
                L = AL)

yuc <- rbind(yuch,yuca)
yuc$yuc <- ((yuc$W/(yuc$W+yuc$L)))


##########
wls$HOME <- tolower(wls$HOME) 
wls$AWAY <- tolower(wls$AWAY)

att_time <- wls %>%
  group_by(DATE) %>%
  summarise(TIME = mean(TIME, na.rm=TRUE),
            ATT = mean(ATT, na.rm=TRUE))
write.csv(att_time, file="LMBatt_time.csv")
print(att_time)

# # # # WL

wls$HOME <- tolower(wls$HOME) 
wls$AWAY <- tolower(wls$AWAY)

teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")

for(i in teams){
  
  assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
    select(
      filter(wls, HOME == i),
      DATE, HOME,HW,HL),
    TEAM = HOME,
    W = HW,
    L = HL)
  ),
  assign(paste0(i,'a'),rename(
    select(
      filter(wls, AWAY == i),
      DATE, AWAY,AW,AL),
    TEAM = AWAY,
    W = AW,
    L = AL)
  )
  )
  )
}

  vaqx <- vaqx[order(vaqx$DATE),]
  tijx <- tijx[order(tijx$DATE),]
  larx <- larx[order(larx$DATE),]
  mxox <- mxox[order(mxox$DATE),]
  mtyx <- mtyx[order(mtyx$DATE),]
  mvax <- mvax[order(mvax$DATE),]
  yucx <- yucx[order(yucx$DATE),]
  quix <- quix[order(quix$DATE),]
  agux <- agux[order(agux$DATE),]
  durx <- durx[order(durx$DATE),]
  leox <- leox[order(leox$DATE),]
  oaxx <- oaxx[order(oaxx$DATE),]
  sltx <- sltx[order(sltx$DATE),]
  puex <- puex[order(puex$DATE),]
  tabx <- tabx[order(tabx$DATE),]
  camx <- camx[order(camx$DATE),]


  vaqx$vaq <- ((vaqx$W/(vaqx$W+vaqx$L)))
  tijx$tij <- ((tijx$W/(tijx$W+tijx$L)))
  larx$lar <- ((larx$W/(larx$W+larx$L)))
  mxox$mxo <- ((mxox$W/(mxox$W+mxox$L)))
  mtyx$mty <- ((mtyx$W/(mtyx$W+mtyx$L)))
  mvax$mva <- ((mvax$W/(mvax$W+mvax$L)))
  yucx$yuc <- ((yucx$W/(yucx$W+yucx$L)))
  quix$qui <- ((quix$W/(quix$W+quix$L)))
  agux$agu <- ((agux$W/(agux$W+agux$L)))
  durx$dur <- ((durx$W/(durx$W+durx$L)))
  leox$leo <- ((leox$W/(leox$W+leox$L)))
  oaxx$oax <- ((oaxx$W/(oaxx$W+oaxx$L)))
  sltx$slt <- ((sltx$W/(sltx$W+sltx$L)))
  puex$pue <- ((puex$W/(puex$W+puex$L)))
  tabx$tab <- ((tabx$W/(tabx$W+tabx$L)))
  camx$cam <- ((camx$W/(camx$W+camx$L)))
  
  lmbts <- cbind(vaqx[1],
                 vaqx[5],
                 tijx[5],
                 larx[5],
                 mxox[5],
                 mtyx[5],
                 mvax[5],
                 yucx[5],
                 quix[5],
                 agux[5],
                 durx[5],
                 leox[5],
                 oaxx[5],
                 sltx[5],
                 puex[5],
                 tabx[5],
                 camx[5])

  write.csv(lmbts, file="LMBts.csv")

# # # # # STANDING
  
  wls$HOME <- tolower(wls$HOME) 
  wls$AWAY <- tolower(wls$AWAY)
  
  teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")
  
  for(i in teams){
    
    assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
      select(
        filter(wls, HOME == i),
        HOME,hR,aR),
      TEAM = HOME,
      R = hR,
      RA = aR)
    ),
    assign(paste0(i,'a'),rename(
      select(
        filter(wls, AWAY == i),
        AWAY,aR,hR),
      TEAM = AWAY,
      R = aR,
      RA = hR)
    )
    )
    )
  }
  
  vaqx <- vaqx[order(vaqx$DATE),]
  tijx <- tijx[order(tijx$DATE),]
  larx <- larx[order(larx$DATE),]
  mxox <- mxox[order(mxox$DATE),]
  mtyx <- mtyx[order(mtyx$DATE),]
  mvax <- mvax[order(mvax$DATE),]
  yucx <- yucx[order(yucx$DATE),]
  quix <- quix[order(quix$DATE),]
  agux <- agux[order(agux$DATE),]
  durx <- durx[order(durx$DATE),]
  leox <- leox[order(leox$DATE),]
  oaxx <- oaxx[order(oaxx$DATE),]
  sltx <- sltx[order(sltx$DATE),]
  puex <- puex[order(puex$DATE),]
  tabx <- tabx[order(tabx$DATE),]
  camx <- camx[order(camx$DATE),]
  
  
  vaqx$vaq <- ((vaqx$W/(vaqx$W+vaqx$L)))
  tijx$tij <- ((tijx$W/(tijx$W+tijx$L)))
  larx$lar <- ((larx$W/(larx$W+larx$L)))
  mxox$mxo <- ((mxox$W/(mxox$W+mxox$L)))
  mtyx$mty <- ((mtyx$W/(mtyx$W+mtyx$L)))
  mvax$mva <- ((mvax$W/(mvax$W+mvax$L)))
  yucx$yuc <- ((yucx$W/(yucx$W+yucx$L)))
  quix$qui <- ((quix$W/(quix$W+quix$L)))
  agux$agu <- ((agux$W/(agux$W+agux$L)))
  durx$dur <- ((durx$W/(durx$W+durx$L)))
  leox$leo <- ((leox$W/(leox$W+leox$L)))
  oaxx$oax <- ((oaxx$W/(oaxx$W+oaxx$L)))
  sltx$slt <- ((sltx$W/(sltx$W+sltx$L)))
  puex$pue <- ((puex$W/(puex$W+puex$L)))
  tabx$tab <- ((tabx$W/(tabx$W+tabx$L)))
  camx$cam <- ((camx$W/(camx$W+camx$L)))
  
  lmbts <- cbind(vaqx[1],
                 vaqx[5],
                 tijx[5],
                 larx[5],
                 mxox[5],
                 mtyx[5],
                 mvax[5],
                 yucx[5],
                 quix[5],
                 agux[5],
                 durx[5],
                 leox[5],
                 oaxx[5],
                 sltx[5],
                 puex[5],
                 tabx[5],
                 camx[5])
  
  write.csv(lmbts, file="LMBts.csv")

#yuch <- sqldf::read.csv.sql("wls",sql="select DATE,HOME as TEAM,HW as W,HL as L from wls where HOME = 'yuc' 
#                            order by DATE asc")
#yuca <- sqldf::read.csv.sql("wls",sql="select DATE,AWAY as TEAM,AW as W,AL as L from wls where AWAY = 'yuc' 
#                            order by DATE asc")


#yuc <- sqldf::read.csv.sql("yuc",sql="select * from yuc order by DATE asc")


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
write.csv(wls,"masterlmb17.csv")
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

