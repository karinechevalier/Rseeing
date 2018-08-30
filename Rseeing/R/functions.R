library(zoo)
library(ggplot2)

CleanValues<-function(datas,seuilmax=5)
{
  if(class(datas)=="zoo")
  {
    datas=datas[which(coredata(datas)>0) ]
    datas=datas[which(coredata(datas)<seuilmax) ]
    return(datas)
  }
}

RemoveValues<-function(datas,seuilmax=5)
{
  if(class(datas)=="zoo")
  {
    return(datas[which(coredata(datas)<seuilmax)])
  }
}



# ReadZooFile<- function(file=NULL,sep=";")
# {
#   if(is.null(file)) stop("ReadZooFile : nom de fichier non défini")
#   if(!file.exists(file))stop("ReadZooFile : ",file," n'existe pas")
#   read.zoo(file=file,FUN=as.POSIXct,sep=";")->seeing
#   return(seeing)
# }
ReadSSMFile <- function(file=NULL, start=NULL,end=NULL,tz=Sys.timezone())
{
  #lecture et nettoyage des donnees
  if(is.null(file)) stop("ReadSSMFile : nom de fichier non défini")
  if(!file.exists(file))stop("ReadSSMFile : ",file," n'existe pas")
  
  serie <- read.table(file,
                      header = TRUE,
                      skip = 4,
                      sep = ";")
  as.vector(as.character(serie[,1]))->tmp
  all<-unlist(strsplit(tmp,split = "_",fixed = TRUE))
  ndata<-length(all)
  ndata<-ndata/3
  tindexes<-(1:ndata)*3
  dindexes<-(1:ndata)*3-2
  times<-gsub("-",":",all[tindexes])
  times<-gsub(",",".",times)
  dates<-all[dindexes]
  
  all<-unlist(strsplit(dates,split = "-",fixed = TRUE))
  ndata<-length(all)
  ndata<-ndata/3
  dindexes<-(1:ndata)*3-2
  mindexes<-(1:ndata)*3-1
  yindexes<-(1:ndata)*3
  
  thetimes<-paste(all[yindexes],"-",all[mindexes],"-",all[dindexes]," ",times,sep="")
  
  
  op <- options(digits.secs = 3)  
  
  as.POSIXct(thetimes)
  z<-difftime(as.POSIXct(thetimes),as.POSIXct(thetimes[1]))
  options(op) 
  
  
  thetimes<-as.POSIXct(thetimes[1],tz=tz)+z
  z1.index<-thetimes
  z1.data <- serie[,3]
  z1 <- zoo(z1.data, z1.index)
  fname<-unlist(strsplit(basename(file),split="[.]"))[1]
  fullname<-paste(dirname(file),"/",fname,"_zoo.csv",sep="")
  # cat("Ecriture au format zoo dans ",fullname)
  # write.zoo(x=z1,file=fullname,sep=";")
  if(!is.null(start) && !is.null(end))
  {
    z1<-window(z1,start=start,end=end)
  }
  return(z1)
}
CreateFile<-function(filename=NULL,directory=NULL,FUN,...)
{
  if(!is.null(filename))   
  {
    if(!is.null(directory))
      fname<-paste(directory,"/",filename,".png",sep="")
    else
      fname<-paste(getwd(),filename,sep="/")
    png(filename = fname)
  }
  
  FUN(...)
  if(!is.null(filename))    dev.off()
  
}
Histogram<-function(seeing=NULL,breaks,comment=NULL,titre=NULL,log=FALSE,filename=NULL,directory=NULL,...)
{
  if (is.null(seeing ))
    stop("Histogram :  seeing=  null")
  
  if(class(seeing)=="zoo")
  {
    seeingserie <- coredata(seeing)   
    mini<-floor(min(seeingserie))
    maxi<-ceiling(max(seeingserie))
    thelog=""
    if(log) thelog="x"
    if(missing(breaks) || is.null(breaks))
    {
      breaks<-(0:10)/10
      breaks<-breaks*(maxi-mini)+mini
      breaks[1]<-max(breaks[1],0.01)      
    }
    # if(!is.null(filename))   
    # {
    #   if(!is.null(directory))
    #     fname<-paste(directory,filename,sep="/")
    #   else
    #     fname<-paste(getwd(),filename,sep="/")
    #   png(filename = fname)
    # }
    # hist(seeing,breaks=breaks,sub=comment,xlab="Seeing",main=titre,log=thelog) -> hist  
    # if(!is.null(filename))    dev.off()
    return(CreateFile(filename,directory,FUN=hist,seeing,breaks=breaks,sub=comment,xlab="Seeing",main=titre,log=thelog,...))
  }
  
  return(NULL)
}

CalculerPlagesSeeing<-function(seeing,seuil)
{
  seeingserie <- coredata(seeing)  
  timeindexes<-index(seeing)
  times<-as.vector(as.numeric(difftime(timeindexes,timeindexes[1])))  
  indexes<-which(seeingserie<=seuil)
  sapply(split(indexes,cumsum(1-c(T,round(diff(indexes)-1,5)==0))),range)->matrice_plages
  plages<-times[matrice_plages[2,]]-times[matrice_plages[1,]]
  return(plages)
}

HistogrammesDureePasseSousValeursSeeing <-  function(seeing = NULL,  seeing_interet = NULL, comment = NULL,filename=NULL,directory=NULL,...)
{
  if (is.null(seeing_interet))
    stop("HistogrammesDureePasseSousValeursSeeing : seeing_interet nul")
  if (is.null(seeing ))
    stop("HistogrammesDureePasseSousValeursSeeing :  seeing=  null")
  
  if(class(seeing)=="zoo")
  {
    
    for (seuil in seeing_interet)
    {
      plages<-CalculerPlagesSeeing(seeing,seuil)
      plages<-plages[which(plages>0)]
      
      if (length(plages)>0 && !is.na(plages))
      {
        if(!is.null(filename)) 
          the_filename<-paste(filename,seuil,sep="_")
        else
          the_filename<-NULL
        CreateFile(filename = the_filename,
                   directory = directory ,
                   FUN=hist,
                   as.vector(plages),
                   breaks = 10,
                   main = paste("Seeing = ", seuil, sep = ""),
                   sub = comment,
                   xlab = paste("duree continue d'acquisition pour un seeing < ",seuil," arcs" ,sep="")
        )
        # hist( plages, breaks = 10,main = paste("Seeing = ", seuil, sep = ""), sub = comment,
        #       xlab = paste("duree continue d'acquisition pour un seeing < ",seuil," arcs" ,sep="")  )
        
      } else{
        cat("rien pour seeing = ", seuil, "\n")
      }
    }
  }
  
}



DureePasseSousValeursSeeing <-  function(seeing = NULL, seeing_interet = NULL, comment = NULL,filename=NULL,directory=NULL)
{
  if (is.null(seeing_interet))
    stop("DureePasseSousValeursSeeing : seeing_interet nul")
  if (is.null(seeing ))
    stop("DureePasseSousValeursSeeing :  seeing=  null")
  
  if(class(seeing)=="zoo")
  {
    durees<-NULL
    for (seuil in seeing_interet)
    {
      duree<-sum(CalculerPlagesSeeing(seeing,seuil))
      if(!is.na(duree))
        durees<-c(durees,duree)
      else
        durees<-c(durees,0)
    }
    tdiv<-1
    ylab<-"(secondes)"
    # print(durees)
    if(!is.null(durees))
    {
      if(max(durees) > 3600)
      {
        tdiv<-3600
        ylab<-"(heures)"        
      }
      the_frame<-cbind(seeing_interet,durees/tdiv)
      CreateFile(filename=filename,directory = directory
                 ,FUN=plot
                 ,the_frame
                 ,        type        = "p"
                 ,        main = "Duree passe sous une selection de valeurs de seeing"
                 ,        sub = comment
                 ,        xlab = "Seeing (arcsec)"
                 ,        ylab =ylab                 
      )
      # plot(
      #   seeing_interet
      #   ,       durees/tdiv
      #   ,        type        = "p"
      #   ,        main = "Duree passe sous une selection de valeurs de seeing"
      #   ,        sub = comment
      #   ,        xlab = "Seeing (arcsec)"
      #   ,        ylab =ylab
      # )     
    }
    
  }
}
myqqplot<-function(seeingserie1,seeingserie2,xlab="seeing_serie1",ylab="seeing_serie2",main="qqplot seeing",...)
{
  
  qqplot(seeingserie1
         ,seeingserie2
         ,xlab=xlab
         ,ylab=ylab
         , main = main,...)
  # la ligne isocentile sur le graphe
  abline(coef = c(0, 1),
         col = "red",
         lty = "dotted")
}
QQplot<-function(seeing1=NULL,seeing2=NULL,filename=NULL,directory=NULL,...)
{
  if (is.null(seeing1) )
    stop("QQplot :  seeing1=  null")
  if (is.null(seeing2))
    stop("QQplot :  seeing2=  null")
  
  
  if(class(seeing1)=="zoo" && class(seeing2)=="zoo")
  {
    
    seeingserie1 <- coredata(seeing1)
    seeingserie2 <-coredata(seeing2)
    #  comparaison des centiles pour les deux series
    CreateFile(filename=filename,directory = directory,FUN=myqqplot,seeingserie1,seeingserie2,...)
    
  }
}


CDplot<-function(seeing = NULL,
                 breaks = NULL,
                 comment = NULL)
{
  if (is.null(seeing ))   stop("CDplot :  seeing=  null")
  
  if(class(seeing)=="zoo")
  {
    seeingserie <- coredata(seeing)   
    mini<-floor(min(seeingserie))
    maxi<-ceiling(max(seeingserie))
    
    if(missing(breaks) || is.null(breaks))
    {
      breaks<-(0:10)/10
      breaks<-breaks*(maxi-mini)+mini
    }
    
    findInterval(seeingserie, breaks)->sint
    breaks[sint]->sint
    as.factor(sint) -> dseeingserie
    #----------------------------
    # 2 : instants, en colonne 2
    timeindexes <- index(seeing)
    times<-as.vector(as.numeric(difftime(timeindexes,timeindexes[1])))  
    nh<-ceiling(max(times)/3600)
    
    findInterval(times, (0:nh)*3600) -> times
    
    # graphes
    paste("conditionnal density plot\n seeing vs time\n",comment)->titre
    cdplot(dseeingserie ~ times
           ,xlab="duree depuis debut acquisition (heures)"
           ,ylab="seeing (arsec)"
           ,main =titre 
    )
  }
}



Quantiles<-function(seeing=NULL,probas=c(0, 0.05, 0.1,   0.5,  0.95))
{
  if (is.null(seeing ))
    stop("Quantiles :  seeing=  null")
  
  if(class(seeing)=="zoo")
  {
    seeingserie <- coredata(seeing)   
    quantile(seeingserie, probs = probas) -> q1
    cat("Quantiles de la serie\n")
    cat("proba  | seeing \n")
    cat(paste(probas, " : ", q1, "\n", sep = " "))
  }
}

mymatplot<-function(a_matrix,xlab,ylab,lty,col,thenames,main=NULL,sub=NULL)
{
  matplot(
    x = a_matrix[, 1],
    y = a_matrix[, 2:length(a_matrix[1, ])],
    type = "l",
    xlab = xlab,
    ylab =ylab,
    lty =  lty,
    col=col,
    main=main,
    sub=sub
  )
  
  legend("topleft", legend = thenames,  lty =lty,
         col=col)
}

mymatplot2<-function(a_matrix,xlab,ylab,lty,col,thenames,main=NULL,sub=NULL,times)
{

      matplot(
      x = times,
      y = a_matrix[, 1:length(a_matrix[1, ])],
      type = "l",
      xlab = xlab,
      ylab =ylab,
      lty =  lty,
      col=col,
      main=main,
      sub=sub
    )    
  legend("topleft", legend = thenames,  lty =lty, col=col)
}



QuantilesvsTime<-function(seeing=NULL
                          ,probas=c(0, 0.05, 0.1,   0.5,  0.95)
                          ,duree_plages=3600
                          ,comment=NULL
                          ,filename=NULL,directory=NULL)
{
  if (is.null(seeing) ) stop("QuantilesvsTime :  seeing=  null")
  if(require("ggplot2") && require("scales"))   autoplot(seeing)
  seeingserie <- coredata(seeing)  
  timeindexes<-index(seeing)

  times<-as.vector(as.numeric(difftime(timeindexes,timeindexes[1]))) 

  nplages<-ceiling(max(times)/duree_plages)
  plages<-(0:nplages)*duree_plages
  
  
  findInterval(times, plages) -> timesserie
  as.factor(timesserie) -> tlevels
  serie<-as.data.frame(cbind(seeingserie,timesserie))
  qplot1 <-   matrix(nrow = length(attr(tlevels, "levels")), ncol = length(probas))
  qplot1=as.data.frame(qplot1)
  names(qplot1)=probas
  thetimes=vector(mode="numeric",length=length(attr(tlevels, "levels")))
  i <- 0
  for (times in attr(tlevels, "levels"))
  {
    i <- i + 1
    sserie <- subset(serie, timesserie == times)
    t_seeingserie <- sserie[, "seeingserie"]
    
    if (length(t_seeingserie) > 0)
    {
      quantile(t_seeingserie, probs = probas) -> qplot1[i, 1:length(probas)]
      # qplot1[i, 1] <- sserie[1, "timesserie"]*duree_plages#+timeindexes[1]
      thetimes[i] <- sserie[1, "timesserie"]*duree_plages
    } else {
      stop("t_seeingserie = 0")
    }
  }
  thetimes=as.POSIXct(thetimes,tz=Sys.timezone(),origin=timeindexes[1])
  z=NULL
  z=zoo(x=qplot1,order.by=thetimes)
  # print(z)
  # for (i in 1: length(probas))
  # {
  #   tmp=zoo(x=qplot1[,i],order.by=thetimes)
  #   # z=cbind(z,tmp)
  # }
  # z=cbind(thetimes,qplot1[, 1:length(probas)])
  # autoplot(z, facets = NULL,log="y",main="Seeing quantiles",xlab="Time",ylab="Seeing (arcsec)")
  ggplot(aes(x = Index, y = Value, group = Series, colour = Series, linetype = Series),
         data = fortify(z, melt = TRUE)) + geom_line() + xlab("Time") + ylab(paste("Seeing ",comment,sep="")) 
  # qplot(x = Index, y = Value, group = Series, colour = Series,
  #       linetype = Series, facets = Series ~ ., data = fortify(z, melt = TRUE)) +
  #   geom_line() + xlab("Index") + ylab("")
  # ggplot(aes(x = Index, y = Value, group = Series, colour = Series, linetype = Series),
  #        data = fortify(z, melt = TRUE)) + geom_line() + xlab("Index") + ylab("")
  # stop("rrrrrrrr")
  # plot(zoo(qplot1))
  # print(thetimes)
  # z<-difftime(as.POSIXct(thetimes),as.POSIXct(thetimes[1]))
  # options(op) 
  # 
  # 
  # thetimes<-as.POSIXct(thetimes[1],tz=tz)+z
  # z1.index<-thetimes
  # z1.data <- serie[,3]
  # z1 <- zoo(z1.data, z1.index)  
  # 
  # 
  # CreateFile(filename=filename,directory = directory,
  #            FUN=mymatplot2,
  #            times=thetimes,
  #            a_matrix=qplot1,
  #            xlab = "Time",
  #            ylab = "Seeing",
  #            main="Seeing quantiles",
  #            lty =  1:length(probas),
  #            col=1:length(probas),
  #            thenames=probas,
  #            sub=comment
  # )
}





HistrogramsThroughTime<-function(seeing=NULL
                                 ,duree_plages=3600
                                 ,breaks=NULL
                                 ,comment=NULL
                                 ,filename=NULL,directory=NULL)
{
  if (is.null(seeing) ) stop("HistrogramsThroughTime :  seeing=  null")
  
  timeindexes<-index(seeing)
  times<-as.vector(as.numeric(difftime(timeindexes,timeindexes[1]))) 
  
  nplages<-ceiling(max(times)/duree_plages)
  plages<-(0:nplages)*duree_plages
  
  
  findInterval(times, plages) -> timesserie
  as.factor(timesserie) -> tlevels
  
  for (times in attr(tlevels, "levels"))
  {
    sserie <- seeing[which(timesserie == times)]
    tdiv<-1
    unit<-" s "
    if(as.numeric(times)*duree_plages > 60 )
    {
      tdiv<-60
      unit<-" min "      
    }
    if(as.numeric(times)*duree_plages > 3600 )
    {
      tdiv<-3600
      unit<-" heures "      
    }    
    titre<-paste("t = ", as.numeric(times)*duree_plages/tdiv,unit)
    fname<-NULL
    if(!is.null(filename))   fname<-paste(filename,"_",times,sep="")
    Histogram(seeing = sserie,breaks=breaks,comment=comment ,titre = titre,directory = directory,filename = fname)
  }
}



FractionsTemporellesSousSeeing_interet <-
  function(series =  NULL,
           seeing_interet = NULL,
           comment = NULL,
           noms,
           filename=NULL,directory=NULL)
  {
    if (is.null(series ))
      stop("FractionsTemporellesSousseeing_interet :  series=  null")
    
    if (is.null(seeing_interet))
      stop("FractionsTemporellesSousseeing_interet :  seeing_interet=  null")  
    if (is.null(noms))
      stop("FractionsTemporellesSousseeing_interet :  noms=  null") 
    fseuil <- NULL
    ns<-length(series)
    maxlen<-length(seeing_interet)
    matseries<-matrix(nrow=maxlen,ncol=ns+1)
    matseries[,1]<-seeing_interet
    is<-1
    for (seeing in series)
    {
      is<-is+1
      
      if(class(seeing)=="zoo")
      {
        fseuil <- NULL
        
        for (seuil in seeing_interet)
        {
          i <- 1
          CalculerPlagesSeeing(seeing,seuil)->deltat
          duree_totale<-max(index(seeing)) - min(index(seeing))
          duree_totale<-as.double(duree_totale,units="secs")
          fraction_sous_seuil <-sum(deltat) /  duree_totale
          fseuil <- c(fseuil, 100 * fraction_sous_seuil)
        }
        fseuil<-as.vector(fseuil) 
        
        matseries[,is]<-fseuil
      }
    }
    

    CreateFile(filename=filename,directory = directory,
               FUN=mymatplot,
               a_matrix=matseries,
               xlab = "Seuil de seeing (arcsec)",
               ylab = "%temps",
               main="Fraction du temps passe sous le seuil",
               lty =  1:ns,
               col=1:ns,
               thenames=noms,
               sub=comment
    )    
    
    
    
  }
