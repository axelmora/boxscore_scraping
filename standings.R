wls$HOME <- tolower(wls$HOME) 
wls$AWAY <- tolower(wls$AWAY)

teams <- c("cam","pue","agu","mxo","mva", "mty","oax","qui","tab","leo","vaq","dur","lar","yuc","slt","tij")

for(i in teams){
  
  assign(paste0(i,'x'),rbind(assign(paste0(i,'h'),rename(
    select(
      filter(wls, HOME == i),
      HOME,hR,aR,HW,HL),
    TEAM = HOME,
    W = HW,
    L = HL,
    R = hR,
    RA = aR)
  ),
  assign(paste0(i,'a'),rename(
    select(
      filter(wls, AWAY == i),
      AWAY,aR,hR,AW,AL),
    TEAM = AWAY,
    W = AW,
    L = AL,
    R = aR,
    RA = hR)
  )
  )
  )
}

vaqx <- vaqx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
tijx <- tijx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
larx <- larx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
mxox <- mxox %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
mtyx <- mtyx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
mvax <- mvax %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
yucx <- yucx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
quix <- quix %>%
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
agux <- agux %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
durx <- durx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
leox <- leox %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
oaxx <- oaxx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
sltx <- sltx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
puex <- puex %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
tabx <- tabx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n())
camx <- camx %>% 
  group_by(TEAM) %>% 
  summarise(R = sum(R),RA = sum(RA),W = max(W),L = max(L), GP = n()) 

lmbstan <- rbind(vaqx,tijx,larx,mxox,mtyx,mvax,yucx,quix,agux,durx,leox,oaxx,sltx,puex,tabx,camx) %>%
  mutate(PCT = ((W/(W+L))),
         RperG = R/GP,
         RAperG = RA/GP,
         WinRatio = W/L,
         RunsRatio = R/RA,
         PCTexp = R**2/(R**2+RA**2),
         Wexp = round(57 * PCTexp),
         Lexp = round(57 - Wexp),
         Rdif = R - RA)

lmbstan <- select(lmbstan,TEAM,GP,W:PCT,WinRatio,PCT:Lexp,RperG:RAperG,RunsRatio,Rdif)

lmbstan_S <- lmbstan %>%
                filter(TEAM %in% c("cam","pue","mxo","oax","qui","tab","leo","yuc")) %>%
                arrange(-PCT)
lmbstan_N <- lmbstan %>%
                filter(TEAM %in% c("mty","mva","vaq","slt","dur","tij","lar","agu")) %>%
                arrange(-PCT)

write.csv(lmbstan, file="LMB2018_stan.csv")
write.csv(lmbstan_S, file="LMB2018_stan_S.csv")
write.csv(lmbstan_N, file="LMB2018_stan_N.csv")




mutate(TEAM = replace(TEAM, TEAM %in% c("cam","pue","mxo","oax","qui","tab","leo","yuc"), 
                      c('Piratas de campeche',
                        'Pericos de Puebla',
                        'Diablos Rojos del México',
                        'Guerreros de Oaxaca',
                        'Tigres de Quintana Roo',
                        'Olmecas de Tabasco',
                        'Bravos de León',
                        'Leones de Yucatán')))