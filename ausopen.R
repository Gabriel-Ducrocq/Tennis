
setwd("C:/Users/Jackie/Documents/Projets/R/Tennis")


source("tennis.R")

tablematchs<-read.csv("ausopen2014.csv", header=TRUE, stringsAsFactors=FALSE)
tablenames<-read.csv2("listeAO2014.csv", header=TRUE, stringsAsFactors=FALSE)


resultats<-data.frame(Date=tablematchs$Date, Round=tablematchs$Round, Comment=tablematchs$Comment, Winner=tablematchs$Winner, Loser=tablematchs$Loser, MaxW=tablematchs$MaxW, MaxL=tablematchs$MaxL, ProbaWinner=vector("numeric", nrow(tablematchs)), ProbaLoser=vector("numeric", nrow(tablematchs)), PariW=vector("numeric", nrow(tablematchs)), PariL=vector("numeric", nrow(tablematchs)), Gains=vector("numeric", nrow(tablematchs)), NbOppCom=vector("numeric",nrow(tablematchs)), stringsAsFactors=FALSE)




final<-function(prob1, Max1, Max2 )
{
    proba<-1/(1+exp(-(0.7732080-1.2992789*prob1-0.1492623*Max1+0.3691173*Max2)))
	
   return(proba)

}




existe2<-function(nom)
{
   
   retour<-FALSE
   if(nom %in% dir("C:/Users/Jackie/Documents/Projets/R/Tennis/data"))
   {
    retour<-TRUE
   }
  return(retour)
}




rechercheNames<-function(nom)
{
  
  name<-nom
  
  for(i in 1:nrow(tablenames))
  {  
    if(tablenames[i, 3] == nom)
	{
	  name<-tablenames$Complets[i]
	  
	}	
  }
    
    return(name)
}












for(i in 1:nrow(tablematchs))
{
  
  nomWinner<-rechercheNames(resultats$Winner[i])
  nomLoser<-rechercheNames(resultats$Loser[i])
  
   
   
  
  if(tablematchs$Comment[i]== "Completed" && existe2(nomWinner) && existe2(nomLoser) && winmatch(nomWinner, nomLoser, "Hard")[2]>0)
  {
     
     resultats$NbOppCom[i]<-winmatch(nomWinner, nomLoser, "Hard")[2]
	 probawin<-winmatch(nomWinner, nomLoser, "Hard")[1]
     
	
	 resultats$ProbaWinner[i]<- probawin
	 resultats$ProbaLoser[i]<-1-probawin
	
	
	 
	 
	 EsperenceWin<-(resultats$MaxW[i]-1)*resultats$ProbaWinner[i] - resultats$ProbaLoser[i]
	 EsperenceLoser<-(resultats$MaxL[i]-1)*resultats$ProbaLoser[i]- resultats$ProbaWinner[i]
	 
	
	if(TRUE)
	 {
	 if(EsperenceWin>0 && EsperenceLoser>0)
	 {
	    if(resultats$ProbaWinner[i]>0.5)
		{
		  resultats$PariW[i]<-1
		  resultats$PariL[i]<-0
		  resultats$Gains[i]<-resultats$MaxW[i]-1
		}
		else
		{
		resultats$PariW[i]<-0
		resultats$PariL[i]<-1
		resultats$Gains[i]<--1
		}
	  
	 }
	 else if(EsperenceWin>0 && EsperenceLoser<0 && resultats$ProbaWinner[i]>0.5)
	 {
	   resultats$PariW[i]<-1
	   resultats$PariL[i]<-0
	   resultats$Gains[i]<-resultats$MaxW[i]-1
	 }
	 else if(EsperenceWin<0 && EsperenceLoser>0 && resultats$ProbaLoser[i]>0.5)
	 {
	    resultats$PariW[i]<-0
		resultats$PariL[i]<-1 
		resultats$Gains[i]<--1
	 }
	 else
	 {
	    resultats$PariW[i]<-0
		resultats$PariL[i]<-0
		resultats$Gains[i]<-0
		
	 
	 }
	
	}
	else if(FALSE)
	{
	 if(resultats$MaxL[i] >(1/(1-probawin)) && (1-probawin)>0.5)
	 {
	   resultats$PariW[i]<-0
	   resultats$PariL[i]<-1
	   resultats$Gains[i]<--1
	 
	 }
	 else if(resultats$MaxW[i] >(1/probawin) && probawin>0.5)
	{
	    resultats$PariW[i]<-1
		resultats$PariL[i]<-0 
		resultats$Gains[i]<-resultats$MaxW[i]-1
	
	}
	else
	{
	 resultats$PariW[i]<-0
     resultats$PariL[i]<-0
	 resultats$Gains[i]<-0
		
	}
	
	
	}
	else
	{
	  if(resultats$MaxW[i] >= 2 && resultats$ProbaWinner[i]>0.5)
	  {
	   resultats$PariW[i]<-1
	   resultats$PariL[i]<-0
	   resultats$Gains[i]<-resultats$MaxW[i]-1
	  }
	  else if(resultats$MaxL[i]>=2 && resultats$ProbaLoser[i]>0.5)
	  {
	  resultats$PariW[i]<-0
	   resultats$PariL[i]<-1
	   resultats$Gains[i]<--1
	  }
	  else
	  {
	 resultats$PariW[i]<-0
     resultats$PariL[i]<-0
	 resultats$Gains[i]<-0
		
	  
	  }
	 
  }
  }
  else
  {
    resultats$ProbaWinner[i]<-NA
	resultats$ProbaLoser[i]<-NA
    resultats$Gains[i]<-0
  
  }
    
}	

setwd("C:/Users/Jackie/Documents/Projets/R/Tennis")
 