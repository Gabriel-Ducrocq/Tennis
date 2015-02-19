



setwd("C:/Users/Jackie/Documents/Projets/R/Tennis")
source("parsing.R")


#On récupère les tableaux de O'Malley

table1<-read.csv("tableA.csv", header= FALSE)
table2<-read.csv("tableB.csv", header= FALSE)


#On les met sous formes de matrices pour faciliter le calcul

A<-matrix(nrow = 28, ncol = 6)
B<-matrix(nrow = 21, ncol = 6)

for(i in 1:28)
{
   for(j in 1:6)
   {
      A[i, j]<-table1[i, j]
   }
}


for(i in 1:21)
{
   for(j in 1:6)
   {
     B[i, j]<-table2[i, j]
   }
}

#Probabilité de gagner un jeu selon O'Malley
G<-function(p)
{
   return((p^4)*(15-4*p-(10*(p^2)/(1-2*p*(1-p)))))
}



D<-function(p, q)
{
  return((p*q)/(1-(p*(1-q)+(1-p)*q)))
}





#Probabilité de gagner un tie-break

T<-function(p, q)
{
   resultat<-0
   for(i in 1:28)
   {
      resultat<-resultat+ A[i, 1]*(p^A[i, 2])*((1-p)^A[i, 3])*(q^A[i, 4])*((1-q)^A[i, 5])*(D(p, q)^A[i, 6])
   }
  
 return(resultat)
}



#Probabilité de gagner un set de tie-break

S<-function(p, q)
{
  resultat<-0
  for(i in 1:21)
  {
     resultat<-resultat+ B[i , 1]*(G(p)^B[i, 2]) * ((1-G(p))^B[i, 3]) * (G(q)^B[i, 4]) * ((1-G(q))^B[i, 5]) * ((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)
}








#probabilité de gagner un match de 5 sets

M5<-function(p, q)
{

   return((S(p,q)^3)*(1+3*(1-S(p,q))+6*((1-S(p,q))^2)))
}






septsix<-function(p, q)
{
  resultat<-0
  
  for(i in 16:21)
  {
     resultat<-resultat+(B[i,1]* (G(p)^B[i,2]) *((1-G(p))^B[i,3]) *(G(q)^B[i,4]) *((1-G(q))^B[i,5])) 
  }
   return(resultat*((G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q)))
}



sixzero<-function(p,q)
{
  resultat<-0
  for(i in 1:1)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)


}



sixun<-function(p,q)
{

  resultat<-0
  for(i in 2:3)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)

}


sixdeux<-function(p,q)
{
  
  resultat<-0
  for(i in 4:6)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)


}



sixtrois<-function(p,q)
{

  resultat<-0
  for(i in 7:10)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)





}



sixquatre<-function(p,q)
{
   
  resultat<-0
  for(i in 11:15)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)+(G(p)*(1-G(q))+(1-G(p))*G(q))*T(p,q))^B[i, 6])
  }
  return(resultat)




}




septcinq<-function(p,q)
{

     resultat<-0
  for(i in 16:21)
  {
     resultat<-resultat+B[i , 1]*(G(p)^B[i, 2])*((1-G(p))^B[i, 3])*(G(q)^B[i, 4])*((1-G(q))^B[i, 5])*((G(p)*G(q)))
  }
  return(resultat)




}







 
 
 
 #Fonction qui met tout en minuscule et met un tiret à la place des espaces.
traitementNom<-function(nom)
{
nom<-tolower(nom)
i<-1

  while(i<=nchar(nom))
  {
     if(substr(nom, i, i) == " ")
     {
       
       nom<-paste(substr(nom, 1, i-1), traitementNom(substr(nom, i+1, nchar(nom))), sep="-")
   
      i<-nchar(nom)
     }
  i<-i+1
 
  } 
  return(nom)

}
 
 
 
 
 
 #Fonction qui transforme le score en différence.
 translation<-function(resultat, issue)
{
   traduction<-data.frame(Scores=c("6-0","6-1", "6-2", "6-3", "6-4", "7-5", "7-6","0-6", "1-6", "2-6", "3-6", "4-6", "5-7", "6-7"), difference=c(0.48, 0.379, 0.254, 0.144, 0.095, 0.08, 0.043, -0.48, -0.379, -0.254, -0.144, -0.095, -0.08, -0.043))

   retour<-0
   for(i in 1:14)
   {
        if(resultat == traduction[i, "Scores"])
        {
        retour<-traduction[i, "difference"]
        }
   }
   
  return(retour)
}
 
 
 
 
 
 
 #Fonction qui va retourner le score du premier set.
 traitementScore2<-function(score, issue)
{
   
    i<-1
    k<-1
    while(i<=nchar(score))
    {
        if(substr(score, k, k)== " ")
        {
           score<-paste(substr(score, 1, k-1), substr(score, k+1, nchar(score)), sep="")
        } 
        else
        {
          k<-k+1
        }
       
      i<-i+1

    }
    firstset<-substr(score, 1, 3)
    
    

    if(issue == "D")
    {
     firstset<-paste(substr(firstset, 3, 3), substr(firstset, 1, 1), sep="-")
  
    }
   
    return(translation(firstset))
}
 
 
 
 existe<-function(nom)
{
   
   retour<-FALSE
   if(nom %in% dir("C:/Users/Jackie/Documents/Projets/R/Tennis/data"))
   {
    retour<-TRUE
   }
   
  return(retour)
}
 
 
 







 
 diffNextSet<-function(difference, tabletransition, issue)
 {
    
    if(issue == "V")
	{
    a<-0.6+(difference/2)
	b<-1-(0.6-(difference/2))
    tableoutcomes<-data.frame(Scores=c("6-0", "6-1", "6-2", "6-3", "6-4", "7-5", "7-6"), probabilites=round(c(sixzero(a, b), sixun(a,b), sixdeux(a,b), sixtrois(a,b), sixquatre(a,b), septcinq(a,b), septsix(a,b)), 3))
   
    retour<-sum(tableoutcomes$probabilites*tabletransition$Expected)
    }
	else if(issue == "D")
	{
      a<-0.6-(difference/2)
	  b<-1-(0.6+(difference/2))
    tableoutcomes<-data.frame(Scores=c("0-6", "1-6", "2-6", "3-6", "4-6", "5-7", "6-7"), probabilites=round(c(sixzero(a,b), sixun(a,b), sixdeux(a,b), sixtrois(a,b), sixquatre(a,b), septcinq(a,b), septsix(a,b)), 3))
 
    retour<-sum(tableoutcomes$probabilites*tabletransition$Expected)
 
    } 
  return(retour)
 
 }

 
 
 
 
 
 
 #Probabilité de gagner un match de 3 sets

M3<-function(d1, d2, d2prime, d3, d3prime)
{
   p_S<-S(0.6+(d1/2), 1-(0.6-(d1/2)))
	p_S2<-S(0.6+(d2/2), 1-(0.6-(d2/2)))
	q_S2<-S(0.6+(d2prime/2), 1-(0.6-(d2prime/2)))
	p_S3<-S(0.6+(d3/2), 1-(0.6-(d3/2)))
	q_S3<-S(0.6+(d3prime/2), 1-(0.6-(d3prime/2)))
	
   probawin<-p_S*p_S2+p_S*(1-p_S2)*q_S3+(1-p_S)*q_S2*p_S3
   return(probawin)
}
 
 
 
 
 
 
 
 
 
#Fonction qui déduit la différence initiale d'après les matchs contre des opposants communs.
winmatch<-function(player1, player2, terrain)
{
 
  name1<-traitementNom(player1)
  name2<-traitementNom(player2)
  
  #On charge les fichiers csv contenant les match de chaque joueurs ainsi que leurs tables de transition sur une surface pertinente.
  setwd(paste("C:/Users/Jackie/Documents/Projets/R/Tennis/data/", player1, sep=""))
  table1<-read.csv(paste(name1, ".csv", sep=""), stringsAsFactors=FALSE)
  tabletransition1<-read.csv(paste(paste(player1, terrain, sep= " "), ".csv", sep=""), stringsAsFactors=FALSE)
  
  setwd(paste("C:/Users/Jackie/Documents/Projets/R/Tennis/data/", player2, sep=""))
  table2<-read.csv(paste(name2, ".csv", sep=""), stringsAsFactors=FALSE)
  tabletransition2<-read.csv(paste(paste(player2, terrain, sep= " "), ".csv", sep=""), stringsAsFactors=FALSE)
  
  setwd("C:/Users/Jackie/Documents/Projets/R/Tennis")
  
 
 #On ne s'occupe que des matchs qui se sont déroulés sur une surface pertinente.
  table1<-subset(table1, Terrain==terrain)
  table2<-subset(table2, Terrain==terrain)
  
  tabletransition1win<-data.frame(Score=c("6-0", "6-1", "6-2", "6-3", "6-4", "7-5", "7-6"), Expected=tabletransition1$Difference[1:7])
  tabletransition1lose<-data.frame(Score=c("0-6", "1-6", "2-6", "3-6", "4-6", "5-7", "6-7"), Expected=tabletransition1$Difference[8:14])
  
  
  tabletransition2win<-data.frame(Score=c("6-0", "6-1", "6-2", "6-3", "6-4", "7-5", "7-6"), Expected=tabletransition2$Difference[1:7])
  tabletransition2lose<-data.frame(Score=c("0-6", "1-6", "2-6", "3-6", "4-6", "5-7", "6-7"), Expected=tabletransition2$Difference[8:14])
  
  
  
  diffFirstSet1<-mean(traitementScore2(table1$Service, table1$Issue))
  diffFirstSet2<-mean(traitementScore2(table2$Service, table2$Issue))
  
 #On déterminer le nom des opposants communs. 
  oppCom<-intersect(table1$Opposants, table2$Opposants)
 
 
  
  retour<-NA
  
  
  probatotale<-0
  nombre<-0 
  
  if(length(oppCom)>0)
  {
    retour<-0
  
 #On ne garde que les matchs qui se sont déroulés contre un opposant commun.
  table1<-subset(table1, Opposants %in% oppCom)
  table2<-subset(table2, Opposants %in% oppCom)
  
  
 
  for(i in 1:length(oppCom))
  {
    
     if(existe(oppCom[i]))
     {
	  
      nombre<-nombre+1
     #On ne garde que les matchs contre l'opposant commun.
      tableIntermediaire1<-subset(table1, Opposants == oppCom[i])
      tableIntermediaire2<-subset(table2, Opposants == oppCom[i])
	
	
	
	  #On calcule la différence entre les deux joueurs via leur opposant commun. Note: on prend les moyennes au cas où ils auraient joué plusieurs fois contre cet opposant commun.
	  diffe<-(mean(tableIntermediaire1$Service)-(1-mean(tableIntermediaire1$Retour))) - (mean(tableIntermediaire2$Service) - (1-mean(tableIntermediaire2$Retour)))
      
     
	  diff1<-0.5*diffe+0.5*(diffFirstSet1-diffFirstSet2)
	 
	
	

       
	  d2<-diffNextSet(diff1, tabletransition1win, "V")-diffNextSet(-diff1, tabletransition2lose, "D")
	  d2prime<-diffNextSet(diff1, tabletransition1lose, "D")-diffNextSet(-diff1, tabletransition2win, "V")
	
	
	  d3<-diffNextSet(d2prime, tabletransition1win, "V")-diffNextSet(-d2prime, tabletransition2lose, "D")
	  d3prime<-diffNextSet(d2, tabletransition1lose, "D")-diffNextSet(-d2, tabletransition2win, "V")
	

	
	  probabilite<-M3(diff1, d2, d2prime, d3, d3prime)
	
	  probatotale<-probatotale+probabilite
     
    }

	
  }

    retour<-probatotale/nombre
  
  
  }
  setwd("C:/Users/Jackie/Documents/Projets/R/Tennis")
  return(c(retour, nombre))
}





















