library(poLCA)
source('lcaCode/poLCA.r')

drinkMods0 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin,nclass=.,nrep=10,na.rm=FALSE)
                  )

drinkMods1 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=.,nrep=10,na.rm=FALSE)
                  )

save(drinkMods0,drinkMods1,file='mods1-5.RData')

bics0 <- map_dbl(drinkMods0,~.$bic)
bics1 <- map_dbl(drinkMods1,~.$bic)

ggplot(mapping=aes(x=rep(1:5,2),y=c(bics0,bics1),color=rep(c('Old','New'),each=5)))+geom_point()+geom_line()
ggplot(mapping=aes(x=rep(2:5,2),y=c(bics0[-1],bics1[-1]),color=rep(c('Old','New'),each=4)))+geom_point()+geom_line()

mod3 <- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=3,na.rm=FALSE,nrep=30)
mod30 <- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin,nclass=3,na.rm=FALSE,nrep=30)

save(mod3,mod30,file='niceMod.RData')

makeplot2(mod3,cnames=c('No','Yes'))
ggsave('plotNew.pdf')
makeplot2(mod30,cnames=c('No','Yes'))
ggsave('plotOld.pdf')


table(mod3$predclass,mod30$predclass)
