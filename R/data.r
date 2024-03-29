library(tidyverse)
library(haven)
library(poLCA)

select <- dplyr::select

dat <- read_sas('data/yrbs2017.sas7bdat','data/formats.sas7bcat')

names(dat) <- tolower(names(dat))


subst <- list(
    smoke = select(dat,q31:q33),
    vape = select(dat,q34:q36),
    drink = select(dat,q40:q45),
    pot = select(dat,q46:q48)
)

for(nm in names(subst)){
    names(subst[[nm]]) <- paste0(nm,seq(ncol(subst[[nm]])))
    subst[[nm]] <- as_factor(subst[[nm]])
    subst[[nm]][subst[[nm]]=='Missing'] <- NA
    subst[[nm]] <- droplevels(subst[[nm]])
}

### look at alcohol
drink <- subst$drink
## drink1: During your life, on how many days have you had at least one drink of alcohol?
## drink2: How old were you when you had your first drink of alcohol other than a few sips?
## drink3: During the past 30 days, on how many days did you have at least one drink of alcohol?
## drink4: During the past 30 days, how did you usually get the alcohol you drank?
## drink5: During the past 30 days, on how many days did you have 4 or more drinks of alcohol in a row (if you are female) or 5 or more drinks of alcohol in a row (if you are male)?
## drink6: During the past 30 days, what is the largest number of alcoholic drinks you had in a row?

## checking depencencies:
## distribution of other variables when drink1=="0 days" (i.e. never drank)
drink%>%filter(drink1=='0 days')%>%map(table,useNA='ifany') ## checks out
## distribution of other variables when drink2=="Never drank alcohol"
drink%>%filter(drink2=='Never drank alcohol')%>%map(table,useNA='ifany') ## some answers for drink 1 (I guess that means a couple sips?)
## dist of drink4:6 if drink3=="0 days"
drink%>%filter(drink3=='0 days')%>%select(drink4:drink6)%>%map(table,useNA='ifany') ## checks out
## dist of drink3:drink6 if drink4 or drink6=="Did not drink in past 30 days"
drink%>%filter(drink4=='Did not drink in past30 days'|drink6=='Did not drink in past 30 days')%>%
    select(drink3:drink6)%>%map(table,useNA='ifany') ## checks out



drinkBin <- drink%>%
    mutate(sex=as.numeric(as.character(dat$q2)))%>%
    transmute(
        everDrink=ifelse(
            is.na(drink1),
                  ifelse(drink2=='Never drank alcohol',1,2),
                  ifelse(drink1=='0 days',1,2)
          ), ## 1= never drank 2= drank some
        preteen=ifelse(is.na(drink2),ifelse(everDrink==1,1,NA),
                ifelse(drink2%in%c('8 years old or younger','9 or 10 years old','11 or 12 years old'),2,1)), ## 2=drank pre-teen
        drink30days=ifelse(is.na(drink3),
                    ifelse(everDrink==1,1,NA),
                    ifelse(drink3=='0 days',1,2)), ## 1=no drinks past 30 days, 2=drank past 30 days
        bought=ifelse(startsWith(as.character(drink4),'Bought'),2,1), ## 2= bought alc in past 30 days
        drinkHeavy=ifelse(
            is.na(drink5),ifelse(drink30days==1,1,NA),
                   ifelse(drink5=='0 days',1,2)), ## 2=drank heavily in past 30 days
        drinkVheavy=ifelse(is.na(drink6),ifelse(drinkHeavy==1,1,NA),
                    ifelse(drink6%in%c('6 or 7 drinks','8 or 9 drinks','10 or more drinks'),2,1)),
### fill in some more NAs
        drinkHeavy=ifelse(is.na(drinkHeavy),
                   ifelse(is.na(drink6),as.numeric(NA),
                   ifelse(drink6=='4 drinks'&is.na(sex),as.numeric(NA),
                   ifelse(drinkVheavy==2|drink6=='5 drinks',2,
                   ifelse(drink6=='4 drinks',ifelse(sex==2,1,2),1)))),
                   drinkHeavy),
        drink30days=ifelse(is.na(drink30days),
                    ifelse(!is.na(drinkHeavy)&drinkHeavy==2,2,
                    ifelse(!is.na(drink4),
                    ifelse(drink4=='Did not drink in past 30 days',1,2),
                    ifelse(!is.na(drink6),
                    ifelse(drink6=='Did not drink in past 30 days',1,2),NA))),
                    drink30days),
        bought=ifelse(is.na(bought),
               ifelse((!is.na(drink30days)&drink30days==1)|(!is.na(everDrink)&everDrink==1),1,NA),
               bought),
        everDrink=ifelse(
            is.na(everDrink),
                  ifelse(drink30days==2|drinkHeavy==2|drinkVheavy==2,2,NA),
            everDrink)
    )



for(i in 1:(ncol(drinkBin)-1))
    for(j in (i+1):ncol(drinkBin)){
        print(paste(names(drinkBin)[i],names(drinkBin)[j]))
        print(table(drinkBin[[i]],drinkBin[[j]],useNA='always'))
    }



#### for conditioning model: set NAs
drinkBin2 <- drinkBin
drinkBin2[!is.na(drinkBin$everDrink)&drinkBin$everDrink==1,c('preteen','drink30days','bought','drinkHeavy','drinkVheavy')] <- NA
drinkBin2[!is.na(drinkBin$drink30days)&drinkBin$drink30days==1,c('bought','drinkHeavy','drinkVheavy')] <- NA
drinkBin2[!is.na(drinkBin$drinkHeavy)&drinkBin$drinkHeavy==1,'drinkVheavy'] <- NA


## look at pot
## pot 1: During your life, how many times have you used marijuana?
## pot 2: How old were you when you tried marijuana for the first time?
## pot 3: During the past 30 days, how many times did you use marijuana?

pot <- subst$pot
### complex structural 0s between pot1 and pot3

table(pot[[2]][pot[[1]]=='0 times'],useNA='always') ## checks out
table(pot[[3]][pot[[1]]=='0 times'],useNA='always') ## checks out
table(pot[[3]][pot[[2]]=='Never tried marijuana'],useNA='always') ## checks out


potBin <- pot%>%
    transmute(
        everPot=ifelse(
            is.na(pot1),
                  ifelse(pot2=='Never tried marijuana',1,2),
                  ifelse(pot1=='0 times',1,2)
          ), ## 1= never pot 2= smoked some
        preteenPot=ifelse(is.na(pot2),NA,
                   ifelse(pot2%in%c('8 years old or younger','9 or 10 years old','11 or 12 years old'),2,1)), ## 2=pot pre-teen
        pot30days=ifelse(pot3=='0 times',1,2), ## 1=no pot past 30 days, 2=pot past 30 days
        pot20plus=ifelse(is.na(pot3),NA,ifelse(pot3%in%c('20 to 39 times', '40 or more times'),2,1))
    )


for(i in 1:3) for(j in (i+1):4){
                  print(paste(names(potBin)[i],names(potBin)[j]))
                  print(table(potBin[[i]],potBin[[j]],useNA='always'))
                  }

potBin2 <- potBin
potBin2[potBin$evePot


pot2 <- pot%>%
    transmute(
        ever=factor(ifelse(pot1=='0 times','never smoked','smoked'),
                    levels=('never smoked','smoked')),
        ageFirst=pot2,
        timesEarlier=factor(
            ifelse(ever=='never smoked','0',
                   ifelse(pot1



for(nm in grep('q[1-9]',names(dat),value=TRUE)){
    x <- dat[[nm]]
    x[x==''] <- NA
    dat[[nm]] <- as.numeric







newVarb <- function(oldVarb,cats){
    new <- rep(NA,length(oldVarb))
    for(i in 1:length(cats))
        new[oldVarb%in%cats[[i]]] <- i
    new
}

tx17Days <- list(1,2:3,4:5,6:7)
tx17Times <- list(1,2,3:4,5:6)

sub17 <- with(dat17,data.frame(
  school.id=as.character(psu),
  year=rep(17,nrow(dat17)),
  type='Trad',
  smoke=newVarb(q32,tx17Days),
  chew=newVarb(q37,tx17Days),
  cigar=newVarb(q38,tx17Days),
  vape=newVarb(q35,tx17Days),
  drink=newVarb(q42,tx17Days),
  pot=newVarb(q48,tx17Times),
  perscription=newVarb(q56,tx17Times),
  coke=newVarb(q49,tx17Times),
  glue=newVarb(q50,tx17Times),
  heroin=newVarb(q51,tx17Times),
  meth=newVarb(q52,tx17Times),
  ecstasy=newVarb(q53,tx17Times),
  syPot=newVarb(q54,tx17Times)))

sub17full <- with(dat17,data.frame(
  year=rep(17,nrow(dat17)),
  type='Trad',
  smoke=q32,
  chew=q37,
  cigar=q38,
  vape=q35,
  drink=q42,
  pot=q48,
  perscription=q56,
  coke=q49,
  glue=q50,
  heroin=q51,
  meth=q52,
  ecstasy=q53,
  syPot=q54))
sub17full[sub17full>10] <- NA

### fill in some NAs
sub17 <- within(sub17,{
    smoke[is.na(smoke)] <-
        with(dat17[is.na(smoke),],
             ifelse(q31==1|q33==1|q39==1,1,NA))
    vape[is.na(vape)] <-
        with(dat17[is.na(vape),],
             ifelse(q34==1|q36==1|q39==1,1,NA))
    chew[is.na(chew)] <- ifelse(dat17$q39[is.na(chew)]==1,1,NA)
    cigar[is.na(cigar)] <- ifelse(dat17$q39[is.na(cigar)]==1,1,NA)
    drink[is.na(drink)] <-
        with(dat17[is.na(drink),],
             ifelse(q40==1|q41==1|q43==1|q45==1,1,NA))
    pot[is.na(pot)] <-
        with(dat17[is.na(pot),],
             ifelse(q46==1|q47==1,1,NA))
})

sub17full <- within(sub17full,{
    smoke[is.na(smoke)] <-
        with(dat17[is.na(smoke),],
             ifelse(q31==1|q33==1|q39==1,1,NA))
    vape[is.na(vape)] <-
        with(dat17[is.na(vape),],
             ifelse(q34==1|q36==1|q39==1,1,NA))
    chew[is.na(chew)] <- ifelse(dat17$q39[is.na(chew)]==1,1,NA)
    cigar[is.na(cigar)] <- ifelse(dat17$q39[is.na(cigar)]==1,1,NA)
    drink[is.na(drink)] <-
        with(dat17[is.na(drink),],
             ifelse(q40==1|q41==1|q43==1|q45==1,1,NA))
    pot[is.na(pot)] <-
        with(dat17[is.na(pot),],
             ifelse(q46==1|q47==1,1,NA))
})




