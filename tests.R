source('player_stats_scr_bat.R')
source('player_stats_scr_pit.R')
library(dplyr)

j1 <- read_xml("http://www.milb.com/gdcross/components/game/aaa/year_2017/month_05/day_04/gid_2017_05_04_camaaa_leoaaa_1/rawboxscore.xml")

##EXTRACT
pit1 <- py_st_pt(j1)
bat1 <- py_st_bt(j1)


##PITCHING STATS
pit_bas <- select(pit1, Pitcher_Name, H:SV)

pit_bas <- mutate(pit_bas,
       IP = round(as.numeric(paste0(trunc(pit1$OUT/3),".",pit1$OUT%%3)),2),
       ERA = round(9*(ER/IP),2),
       WHIP = round((BB+H)/IP,2)
)

pit_sab <- select(pit_bas, Pitcher_Name, SO:BB,IP:WHIP)

pit_sab <- mutate(pit_sab,
'BB/9' = round(9*(BB/IP),2),
'K/9' = round(9*(SO/IP),2)
)

##BATTING STATS

bat_bas <- mutate(bat1,
      PA = AB+BB+SF+SH
      )

bat_bas <- bat_bas %>% 
  filter(PA > 0)

bat_bas <- mutate(bat_bas,
     AVG = round((H/AB),3),
     OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),3),
     SLG = round((((1*H)+(2*D)+(3*Tr)+(4*HR))/AB),3),
     OPS = round((OBP+SLG),3)
)
