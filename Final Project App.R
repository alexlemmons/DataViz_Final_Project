
library(tidyverse)
library(dplyr)
library(stringr)



library(readr)
QB_data <- read_csv("../data/qb_combine.csv")

View(QB_data)

QB_data$Ht[is.na(QB_data$Ht)]="6-4"
QB_data$Wt[is.na(QB_data$Wt)]= "205"
# Brady Davis https://gotigersgo.com/sports/football/roster/brady-davis/5145
heights<-str_split(QB_data$Ht, "-", simplify=FALSE) 
n.heights<-length(heights)
ft<-unlist(heights)[1+2*0:(n.heights-1)]
inch<-unlist(heights)[2*1:n.heights]
QB_data$height_in_inches=12*as.numeric(ft)+as.numeric(inch)

QB_data[!(QB_data$year== 2017:2021),] 

QB_data[!duplicated(QB_data$Player), ]

QB_data$NormalizedCAV = QB_data$nfl_cav / (2021-QB_data$year)

view(QB_data)

library(dplyr)
library(tidyr)

#QB_data %>%
#  separate(Ht,c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
#  mutate(feet = 12*feet + inches) %>%
#  select(-inches)

# AdjPassPerAtt = (Yds+20*TDs-45*Ints)/Att
# NormalizedCAV = CAV/(2021-DraftYear)
# only include draft years before 2016
# lm or lasso or ridge or kNN or ... (NormalizedCAV ~   )


View(QB_data)
#Exceptions: Giovanni Carmazzi, Travis Brown
#Carson Wentz (https://www.sports-reference.com/cfb/players/carson-wentz-1.html)
which.CarsonW=which(QB_data$Player=="Carson Wentz")
QB_data$cfb_G[which.CarsonW]=35
QB_data$cfb_Completions[which.CarsonW]=392
QB_data$cfb_Attempts[which.CarsonW]=612
QB_data$cfb_CompPct[which.CarsonW]=64.1
QB_data$cfb_PassingYds[which.CarsonW]=5115
QB_data$cfb_YdsPerAtt[which.CarsonW]=8.4
QB_data$cfb_AdjPassPerAtt[which.CarsonW]=8.8
QB_data$cfb_Tds[which.CarsonW]=45
QB_data$cfb_Int[which.CarsonW]=14
QB_data$cfb_rating[which.CarsonW]=153.9

#Spergon Wynn (*Texas State Only*) (https://en.wikipedia.org/wiki/Spergon_Wynn) 
which.SpergonW=which(QB_data$Player=="Spergon Wynn")
QB_data$cfb_G[which.SpergonW]=NA
QB_data$cfb_Completions[which.SpergonW]=334
QB_data$cfb_Attempts[which.SpergonW]=607
QB_data$cfb_CompPct[which.SpergonW]=55.1
QB_data$cfb_PassingYds[which.SpergonW]=3497
QB_data$cfb_YdsPerAtt[which.SpergonW]=5.8
QB_data$cfb_AdjPassPerAtt[which.SpergonW]=(QB_data$cfb_PassingYds[which.SpergonW]+20*QB_data$cfb_Tds[which.SpergonW]-45*QB_data$cfb_Int[which.SpergonW])/QB_data$cfb_Attempts[which.SpergonW]
QB_data$cfb_Tds[which.SpergonW]=24
QB_data$cfb_Int[which.SpergonW]=20
QB_data$cfb_rating[which.SpergonW]=((8.4*QB_data$cfb_PassingYds[which.SpergonW])+(330*QB_data$cfb_Tds[which.SpergonW])+(100*QB_data$cfb_Completions[which.SpergonW])-(200*QB_data$cfb_Int[which.SpergonW]))/QB_data$cfb_Attempts[which.SpergonW]


#Justin Coleman (https://kearneyhub.com/sports/then-now-justin-coleman-had-record-setting-career-with-unk-football/article_93caabd4-a797-11ea-9a30-a7e624de5935.html)
which.JustinC=which(QB_data$Player=="Justin Coleman")
QB_data$cfb_G[which.JustinC]=NA
QB_data$cfb_Completions[which.JustinC]=706
QB_data$cfb_Attempts[which.JustinC]=1193
QB_data$cfb_CompPct[which.JustinC]=59.2
QB_data$cfb_PassingYds[which.JustinC]=11213
QB_data$cfb_YdsPerAtt[which.JustinC]=9.4
QB_data$cfb_AdjPassPerAtt[which.JustinC]=(QB_data$cfb_PassingYds[which.JustinC]+20*QB_data$cfb_Tds[which.JustinC]-45*QB_data$cfb_Int[which.JustinC])/QB_data$cfb_Attempts[which.JustinC]
QB_data$cfb_Tds[which.JustinC]=99
QB_data$cfb_Int[which.JustinC]=NA
QB_data$cfb_rating[which.JustinC]=NA

#David Rivers (*Virginia only, spent last year at Western Carolina where he supposedly had a very good season*) (https://www.sports-reference.com/cfb/players/david-rivers-1.html)
# incomplete drop 
which.DavidR=which(QB_data$Player=="David Rivers")
QB_data$cfb_G[which.DavidR]=13
QB_data$cfb_Completions[which.DavidR]=24
QB_data$cfb_Attempts[which.DavidR]=45
QB_data$cfb_CompPct[which.DavidR]=53.3
QB_data$cfb_PassingYds[which.DavidR]=275
QB_data$cfb_YdsPerAtt[which.DavidR]=6.1
QB_data$cfb_AdjPassPerAtt[which.DavidR]=5.4
QB_data$cfb_Tds[which.DavidR]=3
QB_data$cfb_Int[which.DavidR]=2
QB_data$cfb_rating[which.DavidR]=117.8

#J.T. O'Sullivan (https://web.archive.org/web/20080626090505/http://www.detroitlions.com/bio.cfm?bio_id=466&season=9)
which.JTO=which(QB_data$Player=="J.T. O'Sullivan")
QB_data$cfb_G[which.JTO]=NA
QB_data$cfb_Completions[which.JTO]=669
QB_data$cfb_Attempts[which.JTO]=1070
QB_data$cfb_CompPct[which.JTO]=62.5
QB_data$cfb_PassingYds[which.JTO]=10745
QB_data$cfb_YdsPerAtt[which.JTO]=10.0
QB_data$cfb_AdjPassPerAtt[which.JTO]=(QB_data$cfb_PassingYds[which.JTO]+20*QB_data$cfb_Tds[which.JTO]-45*QB_data$cfb_Int[which.JTO])/QB_data$cfb_Attempts[which.JTO]
QB_data$cfb_Tds[which.JTO]=96
QB_data$cfb_Int[which.JTO]=41
QB_data$cfb_rating[which.JTO]=((8.4*QB_data$cfb_PassingYds[which.JTO])+(330*QB_data$cfb_Tds[which.JTO])+(100*QB_data$cfb_Completions[which.JTO])-(200*QB_data$cfb_Int[which.JTO]))/QB_data$cfb_Attempts[which.JTO]

#Tony Romo (https://en.wikipedia.org/wiki/Tony_Romo)
which.TonyR=which(QB_data$Player=="Tony Romo")
QB_data$cfb_G[which.TonyR]=33
QB_data$cfb_Completions[which.TonyR]=560
QB_data$cfb_Attempts[which.TonyR]=892
QB_data$cfb_CompPct[which.TonyR]=62.8
QB_data$cfb_PassingYds[which.TonyR]=7816
QB_data$cfb_YdsPerAtt[which.TonyR]=8.8
QB_data$cfb_AdjPassPerAtt[which.TonyR]=(QB_data$cfb_PassingYds[which.TonyR]+20*QB_data$cfb_Tds[which.TonyR]-45*QB_data$cfb_Int[which.TonyR])/QB_data$cfb_Attempts[which.TonyR]
QB_data$cfb_Tds[which.TonyR]=82
QB_data$cfb_Int[which.TonyR]=34
QB_data$cfb_rating[which.TonyR]=159.1   

#Ryan Fitzpatrick (https://en.wikipedia.org/wiki/Ryan_Fitzpatrick)
which.RyanF=which(QB_data$Player=="Ryan Fitzpatrick")
QB_data$cfb_G[which.RyanF]=30
QB_data$cfb_Completions[which.RyanF]=384
QB_data$cfb_Attempts[which.RyanF]=641
QB_data$cfb_CompPct[which.RyanF]=59.9
QB_data$cfb_PassingYds[which.RyanF]=5234
QB_data$cfb_YdsPerAtt[which.RyanF]=4.1
QB_data$cfb_AdjPassPerAtt[which.RyanF]=(QB_data$cfb_PassingYds[which.RyanF]+20*QB_data$cfb_Tds[which.RyanF]-45*QB_data$cfb_Int[which.RyanF])/QB_data$cfb_Attempts[which.RyanF]
QB_data$cfb_Tds[which.RyanF]=39
QB_data$cfb_Int[which.RyanF]=15
QB_data$cfb_rating[which.RyanF]=((8.4*QB_data$cfb_PassingYds[which.RyanF])+(330*QB_data$cfb_Tds[which.RyanF])+(100*QB_data$cfb_Completions[which.RyanF])-(200*QB_data$cfb_Int[which.RyanF]))/QB_data$cfb_Attempts[which.RyanF]

#Adrian McPherson (https://www.sports-reference.com/cfb/players/adrian-mcpherson-1.html)
which.AdrianM=which(QB_data$Player=="Adrian McPherson")
QB_data$cfb_G[which.AdrianM]=18
QB_data$cfb_Completions[which.AdrianM]=98
QB_data$cfb_Attempts[which.AdrianM]=192
QB_data$cfb_CompPct[which.AdrianM]=51
QB_data$cfb_PassingYds[which.AdrianM]=1215
QB_data$cfb_YdsPerAtt[which.AdrianM]=6.3
QB_data$cfb_AdjPassPerAtt[which.AdrianM]=7.6
QB_data$cfb_Tds[which.AdrianM]=14
QB_data$cfb_Int[which.AdrianM]=1
QB_data$cfb_rating[which.AdrianM]=127.2

#Ingle Martin (*Furman only, transferred from Gators but wasn't a starter there*) (https://en.wikipedia.org/wiki/Ingle_Martin)
which.IngleM=which(QB_data$Player=="Ingle Martin")
QB_data$cfb_G[which.IngleM]=43
QB_data$cfb_Completions[which.IngleM]=464
QB_data$cfb_Attempts[which.IngleM]=756
QB_data$cfb_CompPct[which.IngleM]=61.37661
QB_data$cfb_PassingYds[which.IngleM]=6511
QB_data$cfb_YdsPerAtt[which.IngleM]=QB_data$cfb_PassingYds[which.IngleM]/QB_data$cfb_Attempts[which.IngleM]
QB_data$cfb_AdjPassPerAtt[which.IngleM]=(QB_data$cfb_PassingYds[which.IngleM]+20*QB_data$cfb_Tds[which.IngleM]-45*QB_data$cfb_Int[which.IngleM])/QB_data$cfb_Attempts[which.IngleM]
QB_data$cfb_Tds[which.IngleM]=53
QB_data$cfb_Int[which.IngleM]=25
QB_data$cfb_rating[which.IngleM]=144.475

#Joe Flacco (*UDelaware Only*) (https://en.wikipedia.org/wiki/Joe_Flacco)
#https://en.wikipedia.org/wiki/Joe_Flacco#College_career
which.JoeF=which(QB_data$Player=="Joe Flacco")
QB_data$cfb_G[which.JoeF]=NA
QB_data$cfb_Completions[which.JoeF]=596
QB_data$cfb_Attempts[which.JoeF]=942
QB_data$cfb_CompPct[which.JoeF]=63.3
QB_data$cfb_PassingYds[which.JoeF]=7057
QB_data$cfb_YdsPerAtt[which.JoeF]=NA
QB_data$cfb_AdjPassPerAtt[which.JoeF]=(QB_data$cfb_PassingYds[which.JoeF]+20*QB_data$cfb_Tds[which.JoeF]-45*QB_data$cfb_Int[which.JoeF])/QB_data$cfb_Attempts[which.JoeF]
QB_data$cfb_Tds[which.JoeF]=41
QB_data$cfb_Int[which.JoeF]=15
QB_data$cfb_rating[which.JoeF]=((8.4*QB_data$cfb_PassingYds[which.JoeF])+(330*QB_data$cfb_Tds[which.JoeF])+(100*QB_data$cfb_Completions[which.JoeF])-(200*QB_data$cfb_Int[which.JoeF]))/QB_data$cfb_Attempts[which.JoeF]

#Josh Johnson (https://en.wikipedia.org/wiki/Josh_Johnson_(quarterback))
which.JoshJ=which(QB_data$Player=="Josh Johnson")
QB_data$cfb_G[which.JoshJ]=NA
QB_data$cfb_Completions[which.JoshJ]=724
QB_data$cfb_Attempts[which.JoshJ]=1065
QB_data$cfb_CompPct[which.JoshJ]=68
QB_data$cfb_PassingYds[which.JoshJ]=9699
QB_data$cfb_YdsPerAtt[which.JoshJ]=9.1
QB_data$cfb_AdjPassPerAtt[which.JoshJ]=(QB_data$cfb_PassingYds[which.JoshJ]+20*QB_data$cfb_Tds[which.JoshJ]-45*QB_data$cfb_Int[which.JoshJ])/QB_data$cfb_Attempts[which.JoshJ]
QB_data$cfb_Tds[which.JoshJ]=113
QB_data$cfb_Int[which.JoshJ]=15
QB_data$cfb_rating[which.JoshJ]=176.7

#Rhett Bomar (https://gobearkats.com/sports/football/roster/rhett-bomar/606)
which.RhettB=which(QB_data$Player=="Rhett Bomar")
QB_data$cfb_G[which.RhettB]=31
QB_data$cfb_Completions[which.RhettB]=584
QB_data$cfb_Attempts[which.RhettB]=1035
QB_data$cfb_CompPct[which.RhettB]=56.42
QB_data$cfb_PassingYds[which.RhettB]=7582
QB_data$cfb_YdsPerAtt[which.RhettB]=7.3256
QB_data$cfb_AdjPassPerAtt[which.RhettB]=(QB_data$cfb_PassingYds[which.RhettB]+20*QB_data$cfb_Tds[which.RhettB]-45*QB_data$cfb_Int[which.RhettB])/QB_data$cfb_Attempts[which.RhettB]
QB_data$cfb_Tds[which.RhettB]=47
QB_data$cfb_Int[which.RhettB]=29
QB_data$cfb_rating[which.RhettB]=((8.4*QB_data$cfb_PassingYds[which.RhettB])+(330*QB_data$cfb_Tds[which.RhettB])+(100*QB_data$cfb_Completions[which.RhettB])-(200*QB_data$cfb_Int[which.RhettB]))/QB_data$cfb_Attempts[which.RhettB]

#John Skelton (https://en.wikipedia.org/wiki/John_Skelton_(American_football))
which.JohnS=which(QB_data$Player=="John Skelton")
QB_data$cfb_G[which.JohnS]=43
QB_data$cfb_Completions[which.JohnS]=802
QB_data$cfb_Attempts[which.JohnS]=1363
QB_data$cfb_CompPct[which.JohnS]=58.8
QB_data$cfb_PassingYds[which.JohnS]=9923
QB_data$cfb_YdsPerAtt[which.JohnS]=7.8
QB_data$cfb_AdjPassPerAtt[which.JohnS]=(QB_data$cfb_PassingYds[which.JohnS]+20*QB_data$cfb_Tds[which.JohnS]-45*QB_data$cfb_Int[which.JohnS])/QB_data$cfb_Attempts[which.JohnS]
QB_data$cfb_Tds[which.JohnS]=69
QB_data$cfb_Int[which.JohnS]=36
QB_data$cfb_rating[which.JohnS]=131.41

#Pat Delvin (https://en.wikipedia.org/wiki/Pat_Devlin_(American_football))
which.PatD=which(QB_data$Player=="Pat Devlin")
QB_data$cfb_G[which.PatD]=NA
QB_data$cfb_Completions[which.PatD]=506
QB_data$cfb_Attempts[which.PatD]=776
QB_data$cfb_CompPct[which.PatD]=65.2
QB_data$cfb_PassingYds[which.PatD]=6155
QB_data$cfb_YdsPerAtt[which.PatD]=NA
QB_data$cfb_AdjPassPerAtt[which.PatD]=(QB_data$cfb_PassingYds[which.PatD]+20*QB_data$cfb_Tds[which.PatD]-45*QB_data$cfb_Int[which.PatD])/QB_data$cfb_Attempts[which.PatD]
QB_data$cfb_Tds[which.PatD]=42
QB_data$cfb_Int[which.PatD]=12
QB_data$cfb_rating[which.PatD]=((8.4*QB_data$cfb_PassingYds[which.PatD])+(330*QB_data$cfb_Tds[which.PatD])+(100*QB_data$cfb_Completions[which.PatD])-(200*QB_data$cfb_Int[which.PatD]))/QB_data$cfb_Attempts[which.PatD]

#B.J. Coleman (*Chattanooga Only*) (https://gomocs.com/sports/football/roster/b-j--coleman/3723)
which.BJC=which(QB_data$Player=="B.J. Coleman")
QB_data$cfb_G[which.BJC]=21
QB_data$cfb_Completions[which.BJC]=356
QB_data$cfb_Attempts[which.BJC]=615
QB_data$cfb_CompPct[which.BJC]=57.9
QB_data$cfb_PassingYds[which.BJC]=4544
QB_data$cfb_YdsPerAtt[which.BJC]=7.4
QB_data$cfb_AdjPassPerAtt[which.BJC]=(QB_data$cfb_PassingYds[which.BJC]+20*QB_data$cfb_Tds[which.BJC]-45*QB_data$cfb_Int[which.BJC])/QB_data$cfb_Attempts[which.BJC]
QB_data$cfb_Tds[which.BJC]=35 #5 rushing
QB_data$cfb_Int[which.BJC]=23
QB_data$cfb_rating[which.BJC]=((8.4*QB_data$cfb_PassingYds[which.BJC])+(330*QB_data$cfb_Tds[which.BJC])+(100*QB_data$cfb_Completions[which.BJC])-(200*QB_data$cfb_Int[which.BJC]))/QB_data$cfb_Attempts[which.BJC]

#MarQueis Grey (https://www.sports-reference.com/cfb/players/marqueis-gray-1.html)
which.MarQueisG=which(QB_data$Player=="MarQueis Grey")
QB_data$cfb_G[which.MarQueisG]=47
QB_data$cfb_Completions[which.MarQueisG]=150
QB_data$cfb_Attempts[which.MarQueisG]=295
QB_data$cfb_CompPct[which.MarQueisG]=50.8
QB_data$cfb_PassingYds[which.MarQueisG]=2053
QB_data$cfb_YdsPerAtt[which.MarQueisG]=7
QB_data$cfb_AdjPassPerAtt[which.MarQueisG]=6.2
QB_data$cfb_Tds[which.MarQueisG]=14 #5 rushing and 1 receiving as a qb
QB_data$cfb_Int[which.MarQueisG]=11
QB_data$cfb_rating[which.MarQueisG]=117.5

#Brad Sorenson (*2011 Only*) (https://suutbirds.com/sports/football/roster/brad-sorensen/3764)
which.BradS=which(QB_data$Player=="Brad Sorenson")
QB_data$cfb_G[which.BradS]=NA
QB_data$cfb_Completions[which.BradS]=939
QB_data$cfb_Attempts[which.BradS]=1550
QB_data$cfb_CompPct[which.BradS]=60.6
QB_data$cfb_PassingYds[which.BradS]=11725
QB_data$cfb_YdsPerAtt[which.BradS]=7.6
QB_data$cfb_AdjPassPerAtt[which.BradS]=(QB_data$cfb_PassingYds[which.BradS]+20*QB_data$cfb_Tds[which.BradS]-45*QB_data$cfb_Int[which.BradS])/QB_data$cfb_Attempts[which.BradS]
QB_data$cfb_Tds[which.BradS]=78
QB_data$cfb_Int[which.BradS]=NA
QB_data$cfb_rating[which.BradS]=((8.4*QB_data$cfb_PassingYds[which.BradS])+(330*QB_data$cfb_Tds[which.BradS])+(100*QB_data$cfb_Completions[which.BradS])-(200*QB_data$cfb_Int[which.BradS]))/QB_data$cfb_Attempts[which.BradS]

#Jimmy Garoppolo (https://web.archive.org/web/20161003063910/https://sports.yahoo.com/ncaaf/players/193289/)
which.JimmyG=which(QB_data$Player=="Jimmy Garoppolo")
QB_data$cfb_G[which.JimmyG]=45
QB_data$cfb_Completions[which.JimmyG]=1047
QB_data$cfb_Attempts[which.JimmyG]=1668
QB_data$cfb_CompPct[which.JimmyG]=62.8
QB_data$cfb_PassingYds[which.JimmyG]=13156
QB_data$cfb_YdsPerAtt[which.JimmyG]=7.9
QB_data$cfb_AdjPassPerAtt[which.JimmyG]=(QB_data$cfb_PassingYds[which.JimmyG]+20*QB_data$cfb_Tds[which.JimmyG]-45*QB_data$cfb_Int[which.JimmyG])/QB_data$cfb_Attempts[which.JimmyG]
QB_data$cfb_Tds[which.JimmyG]=118
QB_data$cfb_Int[which.JimmyG]=51
QB_data$cfb_rating[which.JimmyG]=146.3

#Kyle Lauletta (https://sports.yahoo.com/ncaaf/players/231871/)
which.KyleL=which(QB_data$Player=="Kyle Lauletta")
QB_data$cfb_G[which.KyleL]=39
QB_data$cfb_Completions[which.KyleL]=758
QB_data$cfb_Attempts[which.KyleL]=1194
QB_data$cfb_CompPct[which.KyleL]=63.5
QB_data$cfb_PassingYds[which.KyleL]=10465
QB_data$cfb_YdsPerAtt[which.KyleL]=8.8
QB_data$cfb_AdjPassPerAtt[which.KyleL]=(QB_data$cfb_PassingYds[which.KyleL]+20*QB_data$cfb_Tds[which.KyleL]-45*QB_data$cfb_Int[which.KyleL])/QB_data$cfb_Attempts[which.KyleL]
QB_data$cfb_Tds[which.KyleL]=73
QB_data$cfb_Int[which.KyleL]=35
QB_data$cfb_rating[which.KyleL]=151.4

#Mike White (*Both South Florida and Western Kentucky*) (https://www.sports-reference.com/cfb/players/mike-white-6.html)
which.MikeW=which(QB_data$Player=="Mike White")
QB_data$cfb_G[which.MikeW]=44
QB_data$cfb_Completions[which.MikeW]=863
QB_data$cfb_Attempts[which.MikeW]=1393
QB_data$cfb_CompPct[which.MikeW]=62
QB_data$cfb_PassingYds[which.MikeW]=11262
QB_data$cfb_YdsPerAtt[which.MikeW]=8.1
QB_data$cfb_AdjPassPerAtt[which.MikeW]=8.1
QB_data$cfb_Tds[which.MikeW]=74
QB_data$cfb_Int[which.MikeW]=31
QB_data$cfb_rating[which.MikeW]=142.9

#Easton Stick (https://gobison.com/sports/football/roster/easton-stick/8038)
which.EastonS=which(QB_data$Player=="Easton Stick")
QB_data$cfb_G[which.EastonS]=55
QB_data$cfb_Completions[which.EastonS]=598
QB_data$cfb_Attempts[which.EastonS]=980
QB_data$cfb_CompPct[which.EastonS]=61
QB_data$cfb_PassingYds[which.EastonS]=8693
QB_data$cfb_YdsPerAtt[which.EastonS]=8.9
QB_data$cfb_AdjPassPerAtt[which.EastonS]=(QB_data$cfb_PassingYds[which.EastonS]+20*QB_data$cfb_Tds[which.EastonS]-45*QB_data$cfb_Int[which.EastonS])/QB_data$cfb_Attempts[which.EastonS]
QB_data$cfb_Tds[which.EastonS]=88
QB_data$cfb_Int[which.EastonS]=28
QB_data$cfb_rating[which.EastonS]=161


##Filtering Dataset
library(stringr)


QB_combine <- QB_data %>%
  select(-(Player : Ht))

QB_combine <- QB_combine %>%
  select(-(drafted : cfb_Conf))

view(QB_combine)

##Scatterplots of Each Variable
#Height
plot(QB_Combine$Ht)

#Weight
plot(QB_Combine$Wt)

#40 Yard
plot(QB_Combine$`40yd`)

#Vertical Jump
plot(QB_Combine$Vertical)

#Bench Reps
plot(QB_Combine$Bench)

#Broad Jump
plot(QB_Combine$`Broad Jump`)

#3 Cone Drill
plot(QB_Combine$`3Cone`)

#Shuttle Drill
plot(QB_Combine$Shuttle)



##Pairwise w/o Mean
```{r}
pairs(QB_Combine, pch = ".")
```

##Substituting Average for NA
```{r}
QB_combine_impute_mean <- QB_combine

mean40 = mean(QB_combine_impute_mean$`40yd`, na.rm=TRUE)
QB_combine_impute_mean$`40yd`[is.na(QB_combine_impute_mean$`40yd`)] = mean40

meanVertical = mean(QB_combine_impute_mean$`Vertical`, na.rm=TRUE)
QB_combine_impute_mean$`Vertical`[is.na(QB_combine_impute_mean$`Vertical`)] = meanVertical

meanbroad = mean(QB_combine_impute_mean$`Broad Jump`, na.rm=TRUE)
QB_combine_impute_mean$`Broad Jump`[is.na(QB_combine_impute_mean$`Broad Jump`)] = meanbroad

meanshuttle = mean(QB_combine_impute_mean$Shuttle, na.rm=TRUE)
QB_combine_impute_mean$Shuttle[is.na(QB_combine_impute_mean$Shuttle)] = meanshuttle

mean3cone= mean(QB_combine_impute_mean$`3Cone`, na.rm=TRUE)
QB_combine_impute_mean$`3Cone`[is.na(QB_combine_impute_mean$`3Cone`)] = mean3cone

meanBench = mean(QB_combine_impute_mean$Bench, na.rm=TRUE)
QB_combine_impute_mean$Bench[is.na(QB_combine_impute_mean$Bench)] = meanBench

meanGames = mean(QB_combine_impute_mean$cfb_G, na.rm=TRUE)
QB_combine_impute_mean$cfb_G[is.na(QB_combine_impute_mean$cfb_G)] = meanGames

meanCompletions = mean(QB_combine_impute_mean$cfb_Completions, na.rm=TRUE)
QB_combine_impute_mean$cfb_Completions[is.na(QB_combine_impute_mean$cfb_Completions)] = meanCompletions

meanAttempts = mean(QB_combine_impute_mean$cfb_Attempts, na.rm=TRUE)
QB_combine_impute_mean$cfb_Attempts[is.na(QB_combine_impute_mean$cfb_Attempts)] = meanAttempts

meanCompPct = mean(QB_combine_impute_mean$cfb_CompPct, na.rm=TRUE)
QB_combine_impute_mean$cfb_CompPct[is.na(QB_combine_impute_mean$cfb_CompPct)] = meanCompPct

meanPassingYds = mean(QB_combine_impute_mean$cfb_PassingYds, na.rm=TRUE)
QB_combine_impute_mean$cfb_PassingYds[is.na(QB_combine_impute_mean$cfb_PassingYds)] = meanPassingYds

meanYdsPerAtt = mean(QB_combine_impute_mean$cfb_YdsPerAtt, na.rm=TRUE)
QB_combine_impute_mean$cfb_YdsPerAtt[is.na(QB_combine_impute_mean$cfb_YdsPerAtt)] = meanYdsPerAtt

meanAdjYdsPerAtt = mean(QB_combine_impute_mean$cfb_AdjPassPerAtt, na.rm=TRUE)
QB_combine_impute_mean$cfb_AdjPassPerAtt[is.na(QB_combine_impute_mean$cfb_AdjPassPerAtt)] = meanAdjYdsPerAtt

meanTds = mean(QB_combine_impute_mean$cfb_Tds, na.rm=TRUE)
QB_combine_impute_mean$cfb_Tds[is.na(QB_combine_impute_mean$cfb_Tds)] = meanTds

meanInt = mean(QB_combine_impute_mean$cfb_Int, na.rm=TRUE)
QB_combine_impute_mean$cfb_Int[is.na(QB_combine_impute_mean$cfb_Int)] = meanInt

meanRating = mean(QB_combine_impute_mean$cfb_rating, na.rm=TRUE)
QB_combine_impute_mean$cfb_rating[is.na(QB_combine_impute_mean$cfb_rating)] = meanRating

meanCAV = mean(QB_combine_impute_mean$nfl_cav, na.rm=TRUE)
QB_combine_impute_mean$nfl_cav[is.na(QB_combine_impute_mean$nfl_cav)] = meanCAV

meanNormalizedCAV = mean(QB_combine_impute_mean$NormalizedCAV, na.rm=TRUE)
QB_combine_impute_mean$NormalizedCAV[is.na(QB_combine_impute_mean$NormalizedCAV)] = meanNormalizedCAV

view(QB_combine_impute_mean)nan(df_QB$`Out Route Right Average`)] = NA




