#calculate weights for AHP Example, formulas as provided in https://mi.boku.ac.at/ahp/ahptutorial.pdf

#load the data
domains <<- read.csv("~/IWI Forschung/PAPERS/WIP/Digital Readiness Assessment Model/IEEEEng&Mgmt/AHP Case/domains.csv", sep=";")
capabilities1 <<- read.csv("~/IWI Forschung/PAPERS/WIP/Digital Readiness Assessment Model/IEEEEng&Mgmt/AHP Case/capabilities.csv", sep=";")
practices <<- read.csv("~/IWI Forschung/PAPERS/WIP/Digital Readiness Assessment Model/IEEEEng&Mgmt/AHP Case/practices.csv", sep=";")

#function define function for eigenvector calculation
eigenv <- function(arg1){
  arg1mult<-list()
  for (i in 1:nrow(arg1)){
    arg1row<-1
    for (j in 1:ncol(arg1)){
      arg1row<-arg1row*arg1[i,j]
    }
    arg1mult<-unlist(c(arg1mult,arg1row^(1/nrow(arg1))))
  }
  return (arg1mult/sum(arg1mult))
}

#function return ratingmatrix for any given vector
ratingmatrix <- function(arg2){
  helpmatrix=matrix(data=NA,nrow=length(arg2),ncol=length(arg2))
  for (i in 1:length(arg2)){
    for (j in 1:length(arg2)){
      if (is.na(arg2[i]) | is.na(arg2[j])){}
      else if ((arg2[i]-arg2[j])<0){
        helpmatrix[i,j]=1/((arg2[j]-arg2[i])*2+1) 
      } else {
        helpmatrix[i,j]=(arg2[i]-arg2[j])*2+1 
      }
    }
  }
  return(helpmatrix)
}

#function to aggregate weights for a given matrix with the individual weights as columns
aggregateweights <- function(weightmatrix){
  aggregates <- c()
  for (i in 1:nrow(weightmatrix)){
    rowweight<-1
    for (j in 1:ncol(weightmatrix)){
      rowweight<-rowweight*(weightmatrix[i,j]^(1/nrow(weightmatrix)))
    }
    aggregates<-c(aggregates,rowweight)
  }
  return(aggregates/sum(aggregates))
}

#function for any given matrix of SME assessments return mean weights
meanweights <- function(argmatrix){
  matrixresults<-matrix(data=NA, nrow=nrow(argmatrix),ncol=ncol(argmatrix))
  scoreCRList<-list()
  scoregciList<-list()
  for (z in 1:ncol(argmatrix)){
    scorelist<-argmatrix[,z]
    if (is.na(mean(na.omit(scorelist)))){
      scorelist[is.na(scorelist)]<-1
    } else{
    scorelist[is.na(scorelist)]<-mean(na.omit(scorelist)) #replace nas with mean value here
    }
    scorem<-ratingmatrix(scorelist)
    scoreCR<-consistency(scorem)
    scoreCRList<-unlist(c(scoreCRList,scoreCR))
    scoreeigen<-eigenv(scorem)
    scoregci<-gci(scorem,scoreeigen)
    scoregciList<-unlist(c(scoregciList,scoregci))
    matrixresults[,z]<-scoreeigen
    
  }

  scoreweights<-aggregateweights(matrixresults)  
  scoreconsensusList<-list()
  for (z in 1:ncol(argmatrix)){
    scorelist<-argmatrix[,z]
    if (is.na(mean(na.omit(scorelist)))){
      scorelist[is.na(scorelist)]<-1
    } else{
      scorelist[is.na(scorelist)]<-mean(na.omit(scorelist)) #replace nas with mean value here
    }
    scorem<-ratingmatrix(scorelist)
    scoreeigen<-eigenv(scorem)
    scoreconsensus<-goci(scoreeigen,scoreweights)
    scoreconsensusList<-unlist(c(scoreconsensusList,scoreconsensus))
 
    
  }
  
  scoredeviation<-apply(na.omit(matrixresults),1,sd)
  print("Consistency :")
  print(max(scoreCRList,na.rm=TRUE))
  print("Standard Deviations :")
  print(scoredeviation)
  print("Score Weights :")
  print(scoreweights)
  for (y in 1:length(scoreweights)){
    collect<-list()
    collect[1]<-scoreweights[y]
    collect[3]<-max(scoreCRList,na.rm=TRUE)
    collect[4]<-scoredeviation[y]
    collect[5]<-max(scoregciList,na.rm=TRUE)
    collect[6]<-max(scoreconsensusList,na.rm=TRUE)
    rescol<<- rbind(rescol,collect)
  }
  
  return(scoreweights)
}

#function for any given matrix of SME assessments return weights for individual SMEs (same format...)
allweights <- function(argmatrix){
  matrixresults<-matrix(data=NA, nrow=nrow(argmatrix),ncol=ncol(argmatrix))
  scoreCRList<-list()
  scoregciList<-list()
  for (z in 1:ncol(argmatrix)){
    scorelist<-argmatrix[,z]
    if (is.na(mean(na.omit(scorelist)))){
      scorelist[is.na(scorelist)]<-1
    } else{
      scorelist[is.na(scorelist)]<-mean(na.omit(scorelist)) #replace nas with mean value here
    }
    scorem<-ratingmatrix(scorelist)
    scoreCR<-consistency(scorem)
    scoreCRList<-unlist(c(scoreCRList,scoreCR))
    scoreeigen<-eigenv(scorem)
    scoregci<-gci(scorem,scoreeigen)
    scoregciList<-unlist(c(scoregciList,scoregci))
    matrixresults[,z]<-scoreeigen
    
  }
  # scoreweights<-rowMeans(matrixresults,na.rm=TRUE)
  scoreweights<-aggregateweights(matrixresults)
  
  scoreconsensusList<-list()
  for (z in 1:ncol(argmatrix)){
    scorelist<-argmatrix[,z]
    if (is.na(mean(na.omit(scorelist)))){
      scorelist[is.na(scorelist)]<-1
    } else{
      scorelist[is.na(scorelist)]<-mean(na.omit(scorelist)) #replace nas with mean value here
    }
    scorem<-ratingmatrix(scorelist)
    scoreeigen<-eigenv(scorem)
    scoreconsensus<-goci(scoreeigen,scoreweights)
    scoreconsensusList<-unlist(c(scoreconsensusList,scoreconsensus))
    
    
  }
  
  scoredeviation<-apply(na.omit(matrixresults),1,sd)
  print("Consistency :")
  print(max(scoreCRList,na.rm=TRUE))
  print("Standard Deviations :")
  print(scoredeviation)
  print("Score Weights :")
  print(scoreweights)
  for (y in 1:length(scoreweights)){
    collect<-list()
    collect[1]<-scoreweights[y]
    collect[3]<-max(scoreCRList,na.rm=TRUE)
    collect[4]<-scoredeviation[y]
    collect[5]<-max(scoregciList,na.rm=TRUE)
    collect[6]<-max(scoreconsensusList,na.rm=TRUE)
    rescol<<- rbind(rescol,collect)
  }
  
  return(scoreweights)
}

#provide consistency index and consisteny ratio of the matrix
consistency <- function(argmatrix1){
  argn<-nrow(argmatrix1)
  argeigenvalues<-eigen(argmatrix1)$values
  argeigenv<-max(as.numeric(unlist(as.list(argeigenvalues))))
  argCI<-(argeigenv-argn)/(argn-1)
  #random indexes from  here https://www.semanticscholar.org/paper/Consistency-in-the-Analytic-Hierarchy-Process-a-Alonso-Lamata/13f1b74fb9cb5764e399cf213a01274ade280d06/pdf
  rilist<-c( 0,0,0.5247,0.8816,1.1086,1.2479,1.3417,1.4057,1.4499,1.4854,1.514,1.5365,1.5551,1.5713,1.5838) 
  argCR<-argCI/rilist[argn]
  return (argCR)
}

#function to calculate GCI (see Consensus models for AHP group decision making under row geometric mean prioritization method)
gci <- function(scorematrix,weightlist){
  n=nrow(scorematrix)
  diffcol<-0
  for (i in 1:nrow(scorematrix)){
    for (j in 1:ncol(scorematrix)){
      if (i<j){
        diffcol<-diffcol+(log(scorematrix[i,j])-log(weightlist[i])+log(weightlist[j]))^2
      }
    }
  }
  return((1/((n-1)*(n-2)))*diffcol)
}

#function to calculate GOCI (see Consensus models for AHP group decision making under row geometric mean prioritization method)
goci <- function(indlist,alllist){
  sumdiff<-0
  for (i in 1:length(indlist)){
    sumdiff<-sumdiff+abs(rank(indlist)[i]-rank(alllist)[i])
  }
  return(sumdiff/length(indlist))
}


#function to calculate GOCI with tolerant mode (see Consensus models for AHP group decision making under row geometric mean prioritization method)
goci <- function(indlist,alllist){
  sumdiff<-0
  for (i in 1:length(indlist)){
    rankindmin<-rank(indlist,ties.method="min")[i]
    rankindavg<-rank(indlist,ties.method="average")[i]
    rankindmax<-rank(indlist,ties.method="max")[i]
    rankallmin<-rank(alllist,ties.method="min")[i]
    rankallavg<-rank(alllist,ties.method="average")[i]
    rankallmax<-rank(alllist,ties.method="max")[i]
    if (rankindmin<=rankallmin & rankindmax>=rankallmin){rankdiff<-0}
    else if (rankallmin<=rankindmin & rankallmax>=rankindmin){rankdiff<-0}
    else{rankdiff<-min(abs(rankindmin-rankallmax),abs(rankindmax-rankallmin),abs(rankindavg-rankallavg))}
    sumdiff<-sumdiff+rankdiff
  }
  return(sumdiff/length(indlist))
}

###################################################
#Main Program
###################################################

#define resultscollector
rescol<<-matrix(data=NA,nrow=0,ncol=6)

#get mean domain weights and report consistency
domainweights<-meanweights(domains[,-c(1)])

#get capability weights for the individual domains
domain1weights<-meanweights(capabilities1[c(1,2),-c(1)])
domain2weights<-meanweights(capabilities1[c(3,4),-c(1)])
domain3weights<-meanweights(capabilities1[c(5,6),-c(1)])
domain4weights<-meanweights(capabilities1[c(7,8),-c(1)])
domain5weights<-meanweights(capabilities1[c(9,10,11),-c(1)])
domain6weights<-meanweights(capabilities1[c(12,13),-c(1)])
domain7weights<-meanweights(capabilities1[c(14,15),-c(1)])
domain8weights<-meanweights(capabilities1[c(16,17),-c(1)])



#get the practiceweights
capability1Aweights<-meanweights(practices[c(1,2),-c(1)])
capability1Bweights<-meanweights(practices[c(3,4,5),-c(1)])
capability2Aweights<-meanweights(practices[c(6,7,8,9),-c(1)])
capability2Bweights<-meanweights(practices[c(10,11),-c(1)])
capability3Aweights<-meanweights(practices[c(12,13),-c(1)])
capability3Bweights<-meanweights(practices[c(14,15,16),-c(1)])
capability4Aweights<-meanweights(practices[c(17,18,19),-c(1)])
capability4Bweights<-meanweights(practices[c(20,21,22),-c(1)])
capability5Aweights<-meanweights(practices[c(23,24,25),-c(1)])
capability5Bweights<-meanweights(practices[c(26,27,28),-c(1)])
capability5Cweights<-meanweights(practices[c(29,30,31),-c(1)])
capability6Aweights<-meanweights(practices[c(32,33,34),-c(1)])
capability6Bweights<-meanweights(practices[c(35,36),-c(1)])
capability7Aweights<-meanweights(practices[c(37,38),-c(1)])
capability7Bweights<-meanweights(practices[c(39,40,41),-c(1)])
capability8Aweights<-meanweights(practices[c(42,43,44),-c(1)])
capability8Bweights<-meanweights(practices[c(45,46),-c(1)])


#append the weights to the practice dataframe to prepare for multiplikation
practicecweights<-c(capability1Aweights,capability1Bweights,capability2Aweights,capability2Bweights,capability3Aweights,capability3Bweights,capability4Aweights,capability4Bweights,capability5Aweights,capability5Bweights,capability5Cweights,capability6Aweights,capability6Bweights,capability7Aweights,capability7Bweights,capability8Aweights,capability8Bweights)
practices$practicecweights<-practicecweights


capabilitydweights<-c(domain1weights[1],domain1weights[1],domain1weights[2],domain1weights[2],domain1weights[2],domain2weights[1],domain2weights[1],domain2weights[1],domain2weights[1],domain2weights[2],domain2weights[2],domain3weights[1],domain3weights[1],domain3weights[2],domain3weights[2],domain3weights[2],domain4weights[1],domain4weights[1],domain4weights[1],domain4weights[2],domain4weights[2],domain4weights[2],domain5weights[1],domain5weights[1],domain5weights[1],domain5weights[2],domain5weights[2],domain5weights[2],domain5weights[3],domain5weights[3],domain5weights[3],domain6weights[1],domain6weights[1],domain6weights[1],domain6weights[2],domain6weights[2],domain7weights[1],domain7weights[1],domain7weights[2],domain7weights[2],domain7weights[2],domain8weights[1],domain8weights[1],domain8weights[1],domain8weights[2],domain8weights[2])
practices$capabilitydweights<-capabilitydweights

domainweightsprac<-c(rep(domainweights[1],5),rep(domainweights[2],6),rep(domainweights[3],5),rep(domainweights[4],6),rep(domainweights[5],9),rep(domainweights[6],5),rep(domainweights[7],5),rep(domainweights[8],5))
practices$domainweightsprac<-domainweightsprac

practices$practiceweights<-practices$practicecweights*capabilitydweights*domainweightsprac
practices$capabilityweights<-capabilitydweights*domainweightsprac
View(practices)

colweights<-c(domainweights,domainweights[1]*domain1weights,domainweights[2]*domain2weights,domainweights[3]*domain3weights,domainweights[4]*domain4weights,domainweights[5]*domain5weights,domainweights[6]*domain6weights,domainweights[7]*domain7weights,domainweights[8]*domain8weights,practices$practiceweights)
rescol[,2]<-colweights
View(rescol)