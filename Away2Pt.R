data <- NBA_PBP_2018.19
sum = 0
sum2 = 0
high = 0
fourthEasyW = c(rep(0, 30))
fourthEasyL = c(rep(0, 30))
fqy = c(rep(0, 30))
fqn = c(rep(0, 30))
FTPt = c(rep(0, 120))
#secondQ = c(rep(0, 120))
#DRebPt = c(rep(0, 120))
#ATAT = c(rep(0, 120))
#secondQ = FTPT
#Home3Pt = FTPT
curAwayTeam = 0
test3Q = 0
test4Q = 0
curHomeTeam = 0
indicator = 0
t = 1
while(t<614517){
  if(data$Quarter[t]==4 && data$Quarter[t-1]==3 && data$Quarter[t-2]==3){
    if(data$HomeScore[t]-data$AwayScore[t]>4 && data$HomeScore[t]-data$AwayScore[t]<10){
      test3Q = test3Q + 1
      high = 1
      indicator = 1
    }
    else if(data$AwayScore[t]-data$HomeScore[t]<10 && data$AwayScore[t]-data$HomeScore[t]>4){
      test3Q = test3Q + 1
      high = 1
      indicator = 0
    }
    else{
      high = 0
    }
    if(high==1){
      for(team in 1:30){
        if(data$HomeTeam[t]==vector[team]){
          curHomeTeam = team
        }
        if(data$AwayTeam[t]==vector[team]){
          curAwayTeam = team
        }
      }
    }
  }
  else if(high==1 && indicator==1 && data$AwayPlay[t]=="End of Game"){
    test4Q = test4Q + 1
    if(data$HomeScore[t]>data$AwayScore[t]){
      sum = sum + 1
      for(team in 1:30){
        if(data$HomeTeam[t]==vector[team]){
          fourthEasyW[team] = fourthEasyW[team] + 1
        }
      }
    }
    else{
      for(team in 1:30){
        if(data$HomeTeam[t]==vector[team]){
          fourthEasyL[team] = fourthEasyL[team] + 1
        }
      }
    }
    sum2 = sum2 + 1
  }
  else if(high==1 && indicator==0 && data$AwayPlay[t]=="End of Game"){
    test4Q = test4Q + 1
    if(data$HomeScore[t]<data$AwayScore[t]){
      sum = sum + 1
      for(team in 1:30){
        if(data$AwayTeam[t]==vector[team]){
          fourthEasyW[team] = fourthEasyW[team] + 1
        }
      }
    }
    else{
      for(team in 1:30){
        if(data$AwayTeam[t]==vector[team]){
          fourthEasyL[team] = fourthEasyL[team] + 1
        }
      }
    }
    sum2 = sum2 + 1
    
  }
  else if(high==1 && indicator==1 && data$Quarter[t]==4){
    if(grepl("2-pt", data$AwayPlay[t], fixed=TRUE)){
      if(grepl("makes", data$AwayPlay[t], fixed=TRUE)){
        fqy[curHomeTeam] = fqy[curHomeTeam] + 1
      }
      else{
        fqn[curHomeTeam] = fqn[curHomeTeam] + 1
      }
    }
  }
  else if(high==1 && indicator==0 && data$Quarter[t]==4){
    if(grepl("2-pt", data$HomePlay[t], fixed=TRUE)){
      if(grepl("makes", data$HomePlay[t], fixed=TRUE)){
        fqy[curAwayTeam] = fqy[curAwayTeam] + 1
      }
      else{
        fqn[curAwayTeam] = fqn[curAwayTeam] + 1
      }
    }
  }
  t = t + 1
}
print(sum/sum2)
for(team in 1:30){
  print(vector[team])
  print(fourthEasyW[team]/(fourthEasyL[team]+fourthEasyW[team]))
  print(fourthEasyW[team])
  print(fqy[team]/(fqn[team]+fqy[team]))
  PPP[team+90] = (fqy[team]/(fqn[team]+fqy[team]))
  secondQ[team+90] = fourthEasyW[team]/(fourthEasyL[team]+fourthEasyW[team])
}

Away2Pt = PPP
summary(lm(secondQ ~ Away2Pt))


#summary(lm(secondQ ~ HomePt + Home2Pt +  Home3Pt + Away2Pt + Away3Pt + AwayPt + ORebPt + DRebPt + AssistPt + Assist2Pt + Assist3Pt + FTPt + ATAT ))
