library(poLCA)

drinkMods0 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin,nclass=.,na.rm=FALSE)
                  )

drinkMods1 <- map(1:5,
                  ~poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=.,na.rm=FALSE)
                  )

mod3 <- poLCA(cbind(everDrink,preteen,drink30days,bought,drinkHeavy,drinkVheavy)~1,data=drinkBin2,nclass=3,na.rm=FALSE,nrep=30)
save(mod3,file='niceMod.RData')
