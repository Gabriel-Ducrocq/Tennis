

library("XML")

isVS<-function(chaine)
{
  
  vs<-FALSE
  for(i in 1:nchar(chaine))
  {
     if(substr(chaine, i, i+3) == "-VS-")
     {
       vs<-TRUE
     }
  }
  return(vs)
}
  

#Fonction qui indique si deux chiffres se suivent dans le score.
successifsIntRemove<-function(chaine)
{  
   i<-1
   nb<-nchar(chaine)
   while(i<nb)
   {
      if(!is.na(as.numeric(substr(chaine, i, i))) && !is.na(as.numeric(substr(chaine, i+1, i+1))))
      {
        chaine<-paste(substr(chaine, 1, i), substr(chaine, i+2, nchar(chaine)), sep="")
      }
      else
      {
        i<-i+1
      }
   }
 return(chaine)
}









eraseBlank<-function(phrase)
{
   k<-1
   nb<-nchar(phrase)
   while(k<nb)
   {
    
	if(substr(phrase, k, k)==" ")
	{
	   phrase<-paste(substr(phrase, 1, k-1), substr(phrase, k+1, nb) ,sep="")
	 
	
	}
   else
   {
    
     k<-k+1
   }
  
  
  }
  return(phrase)
}








#Fonction nous permettant d'enlever les pourcentages inutiles entre parenthèses, de séparer le numérateur et le dénominateur.
traitementStat<-function(stat)
{
     
     i<-0
     k<-1
    
     while(i<1 && k<nchar(stat))
     {
         if(substr(stat, k, k) == " ")
         {
              i<-1
              stat<-substr(stat, 1, k-1)
         }
       k<-k+1
     }
 
     i<-0
     k<-1
	 
    
     while(i<1 && k<nchar(stat))
     {
        if(substr(stat, k, k) == "/")
        {
           
           chiffre1<-as.numeric(substr(stat, 1, k-1))
		   
           chiffre2<-as.numeric(substr(stat, k+1, nchar(stat)))
        }
       k<-k+1
     }
 
    return(chiffre1/chiffre2)    



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




#Fonction qui met les score sous forme adéquate.
traitementScore<-function(score, issue)
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
    secondset<-substr(score, 5, 7)
    

    if(issue == "D")
    {
     firstset<-paste(substr(firstset, 3, 3), substr(firstset, 1, 1), sep="-")
     secondset<-paste(substr(secondset, 3, 3), substr(secondset, 1, 1), sep="-")

    }
    
    return(c(firstset, secondset))
}






#Fonction qui change les résultats en différence.
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







#Fonction qui va nous permettre de calculer les pourcentages de points gagnés sur le service et le retour séparément.
stats<-function(url, issue)
{
  numRow<-NA
  service<-NA
  retour<-NA
  Sys.sleep(2)
  error<-1
  while(error != 0)
  {
  error<-download.file(url, "stat.txt")
  }
  doc<-htmlParse("stat.txt")
  racine<-xmlRoot(doc)




if(xmlValue(racine[[1]][[1]]) == "404 - adresa neexistuje")
{
   print("Pas de données disponibles pour ce match")
}
else
{
  tableStats<-readHTMLTable("stat.txt", header=FALSE, stringsAsFactors=FALSE)[[2]]
   
  
  if(!is.na(tableStats[5, 1]) && substr(eraseBlank(tableStats[5, 1]), 1, 10) == "Pointsgagn")
  {
    numRow<-5
  }
  else if(!is.na(tableStats[6, 1]) && substr(eraseBlank(tableStats[6, 1]), 1, 10) == "Pointsgagn")
  {
    numRow<-6
  }
  else
  {
    print("Problème pour les points gagnés sur retour")
  }

  
  if(!is.null(tableStats) && !is.na(numRow))
  {

     if(issue == "V")
     {
          retour<-traitementStat(tableStats$V2[numRow])
          service<-1-traitementStat(tableStats$V3[numRow])
     }
     else if(issue == "D")
     {
          retour<-traitementStat(tableStats$V3[numRow])
          service<-1-traitementStat(tableStats$V2[numRow])
     }
     else
     {
       print("Pas d'issue correcte")
     }    

  }

}  
   return(c(service, retour))


}







#Fonction qui calcule les tableaux de transition d'un set à l'autre par terrain pour un joueur donné.
statistiques<-function(nom)
{
   name<-traitementNom(nom)
   
   path<-paste("/home/gabriel/R/Tennis/data/", nom, sep="")
   setwd(path)
   table<-read.csv(paste(name, ".csv", sep=""))
   
   terrain<-c("Hard", "Clay", "Indoor", "Carpet", "Grass")

   #A partir d'ici on calcule les différences moyennes espérées du second set sachant le résultat du premier set par terrain.
  
for(l in 1:5)
{

tableTerrain<-data.frame(Scores=c("6-0","6-1", "6-2", "6-3", "6-4", "7-5", "7-6", "0-6", "1-6", "2-6", "3-6", "4-6", "5-7", "6-7"), Difference=vector("numeric", 14), nombre=vector("numeric", 14))

   for(i in 1:nrow(table))
   {
     
      if(table[i, "Terrain"]==terrain[l])
      {

         
          scores<-traitementScore(as.character(table[i, "Scores"]), table[i, "Issue"])
          scores[2]<-translation(scores[2], table[i, "Issue"])
         
          for(j in 1:14)
          {
             if(scores[1] == tableTerrain[j, "Scores"])
             {
                  tableTerrain[j, "Difference"]<-tableTerrain[j, "Difference"]+as.numeric(scores[2])
                  tableTerrain[j, "nombre"]<-tableTerrain[j, "nombre"]+1
             }
          }
      }

   }


   for(k in 1:14)
   {
      if(tableTerrain[k, "nombre"]== 0)
	  {
	    tableTerrain[k, "Difference"]<-translation(tableTerrain[k, "Scores"])
	  }
	  else
	  {
      tableTerrain[k, "Difference"]<-tableTerrain[k, "Difference"]/tableTerrain[k, "nombre"]
	  }
   }
   
   
   write.csv(tableTerrain, paste(paste(nom, terrain[l],sep= " "), ".csv", sep=""))

}
setwd("/home/gabriel/R/Tennis")
 

}



























#Fonction qui va nous sortir la fiche d'une joueuse.
ficheFemme<-function(nom, terrain)
{

   name<-traitementNom(nom)

   url<-paste("http://www.tennisendirect.net/wta/", name, "/?su=", sep="")
   urlverif<-paste("http://www.tennisendirect.net/wta/", name, "/", sep="")


error<-1
while(error != 0)
{
error<-download.file(urlverif, "match.txt")
}
doc<-htmlParse("match.txt")
racine<-xmlRoot(doc)




if(xmlValue(racine[[1]][[1]]) == "404 - adresa neexistuje")
{
   print("Pas de données disponibles pour ce joueur")
}
else
{
numeroSurface<-data.frame(Surface=c("Hard", "Clay", "Indoor", "Carpet", "Grass"), Numero=c(1, 2, 3, 4, 5))
total<-data.frame(Dates=rep(NA, 0), Opposants=rep(NA, 0), Scores=rep(NA, 0), Issue=rep(NA, 0), Service=rep(NA, 0), Retour=rep(NA, 0), Terrain=rep(NA, 0))

numeroSurface2<-subset(numeroSurface, Surface == terrain)
i<-numeroSurface2$Numero[1]

if(is.na(i))
{
   print("La surface entrée n'est pas prise en charge par le modèle")
}


url2<-paste(url, as.character(i), sep="")
error<-1
while(error != 0)
{

error<-download.file(url2, "match.txt")
}

doc<-htmlParse("match.txt")

general<-readHTMLTable("match.txt", header=FALSE, stringsAsFactors=FALSE)
matchs<-general[[3]]
listeLinks<-getHTMLLinks("match.txt")

if(is.null(matchs) || is.null(listeLinks))
{
  print("Problème lors du chargement des matchs")
}


#On met la liste sous forme de vecteur.
listeLinks<-unlist(listeLinks)

listeDesLiens<-data.frame(Liens=listeLinks, Statuts=vector("logical", length(listeLinks)), stringsAsFactors=FALSE)

#On regarde quels liens mènent effectivement vers les statistiques d'un match.
for(k in 1:nrow(listeDesLiens))
{
   listeDesLiens$Statuts[k]<-isVS(listeDesLiens$Liens[k])
}



#On ne garde que ceux qui y mènent.
listeDesLiens<-subset(listeDesLiens, Statuts==TRUE)


excedent<-general[[2]]
if(substr(as.character(excedent$V1[1]), 1, 11) == "aucun match")
{
     
}
else
{
  for(m in 1:nrow(excedent))
  {
    listeDesLiens<-listeDesLiens[-1,]
  }

} 

#Si le nombre ne matche pas, on s'arrête.
if(nrow(listeDesLiens) != nrow(matchs))
{
  print("Le nombre de lien ne correspond pas au nombre de matchs")
}
#Sinon, on parcourt tous les liens.
else
{

   opposants<- data.frame(Dates=rep(NA, 50), Opposants=rep(NA, 50), Scores=rep(NA, 50), Issue=rep(NA, 50), Service= rep(NA, 50), Retour=rep(NA, 50), Terrain=rep(numeroSurface[i, "Surface"], 50))
   matchs<-cbind(matchs, listeDesLiens$Liens)
  
   j<-1
   l<-1
   while(j<=nrow(matchs) && l<=50)
   {
      
      #On ne garde que les matchs complets et qui se sont déroulés avant la date voulue.
      if(nchar(eraseBlank(matchs$V5[j]))<15 &&  as.Date(matchs$V1[j], format="%d.%m.%Y") < as.Date("13.01.14", format="%d.%m.%Y"))
      {
          opposant<-NA
          issue<-NA
          if(matchs$V3[j] == nom)
          {
              issue<-"V"
              opposant<-matchs$V4[j]
          }
          else if(matchs$V4[j] == nom)
          {
             issue<-"D"
             opposant<-matchs$V3[j]
          }
          else
          {
            print("Problème: pas d'issue trouvée pour le match.")
          }

         tableauPoints<-stats(as.character(matchs[j, 10]), issue)

         if(!is.na(tableauPoints[1]) && !is.na(tableauPoints[2]))
         {
            
            opposants$Dates[l]<-matchs$V1[j]
            opposants$Opposants[l]<-opposant
            opposants$Scores[l]<-successifsIntRemove(matchs$V5[j])
            opposants$Issue[l]<-issue
            opposants$Service[l]<-tableauPoints[1]
            opposants$Retour[l]<-tableauPoints[2]
            
            
            l<-l+1
          }   

      }
      j<-j+1
   }

}


  total<-rbind(total, opposants)


dir<-paste("/home/gabriel/R/Tennis/Data/", nom, sep="")
dir.create(dir)
setwd(dir)
nomFichier<-paste(name, ".csv", sep="")
write.csv(total[order(as.Date(total$Dates, format="%d.%m.%Y"), na.last= NA, decreasing=TRUE),], nomFichier )
statistiques(nom)


}
 
}

