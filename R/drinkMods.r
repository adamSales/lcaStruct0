library(poLCA)
source('lcaCode/poLCA.r')

drinkMods0 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin,nclass=.,nrep=10,na.rm=FALSE,verbose=FALSE)
                  )

drinkMods1 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=.,nrep=20,na.rm=FALSE,verbose=FALSE,maxiter=2000)
                  )

drinkMods1a <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=.,nrep=20,na.rm=FALSE,verbose=FALSE,impVal=1,maxiter=2000)
                  )

drinkMods1b <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=.,nrep=20,na.rm=FALSE,verbose=FALSE,impVal=.5,maxiter=2000)
                  )


save(drinkMods0,drinkMods1,drinkMods1a,drinkMods1b,file='mods1-5.RData')

bics0 <- map_dbl(drinkMods0,~.$bic)
bics1 <- map_dbl(drinkMods1,~.$bic)
bics1a <- map_dbl(drinkMods1a,~.$bic)
bics1b <- map_dbl(drinkMods1b,~.$bic)

pairs(cbind(bics1,bics1a,bics1b))

rbind(bics1,bics1a,bics1b)

ggplot(mapping=aes(x=rep(1:5,2),y=c(bics0,bics1),color=rep(c('Old','New'),each=5)))+geom_point()+geom_line()
ggplot(mapping=aes(x=rep(2:5,2),y=c(bics0[-1],bics1[-1]),color=rep(c('Old','New'),each=4)))+geom_point()+geom_line()

which.min(bics0)
which.min(bics1)

mod2 <- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=2,na.rm=FALSE,nrep=50,verbose=FALSE,maxiter=2000)
mod2a<- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=2,na.rm=FALSE,nrep=50,verbose=FALSE,impVal=.5,maxiter=2000)
mod2b<- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=2,na.rm=FALSE,nrep=50,verbose=FALSE,impVal=1,maxiter=2000)
mod30 <- poLCAord(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin,nclass=3,na.rm=FALSE,nrep=50,verbose=FALSE,maxiter=2000)

save(mod2,mod2a,mod2b,mod30,file='models.RData')


makeplot2(mod2,cnames=c('No','Yes'))
ggsave('plotNew.pdf')
makeplot2(mod30,cnames=c('No','Yes'))
ggsave('plotOld.pdf')


table(mod2$predclass,mod30$predclass)

drinkBin2[which(mod2$predclass==1&mod30$predclass==3),]
