#always set working directory to the project.

rm(list=ls())

# install all necessary packages.
packages <- c("lattice", "graphics", "kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "stats", "foreach", "pvclust")
lapply(packages, library, character.only = TRUE)



#select export or import
flowdir <- "import"
#flowdir <- "export"

# Dictionary
wddir <- "./data"

# clean the data
drops <- c("X", "code")

# read the data
csvfile1 <- paste(wddir, "/", flowdir, "/tradematrix/", flowdir, "1900.csv", sep="")
dataDF <- read.csv(csvfile1, header=TRUE, sep=",", row.names="country", stringsAsFactors=F)
dataDF <- dataDF[,!(names(dataDF) %in% drops)]
dataDF[dataDF==-9]<-NA
diag(dataDF)<-NA


years<-seq(1901, 2014, by = 1)
allyears<-seq(1900, 2014, by = 1)

# assemble export or import files over the years
for (i in years){
  csvfile2 <- paste(wddir,"/", flowdir,"/tradematrix/", flowdir
                    , i, ".csv", sep="")
  dataDF2 <- read.csv(csvfile2, header=TRUE, sep=",", row.names="country")
  dataDF2 <- dataDF2[,!(names(dataDF2) %in% drops)]
  dataDF2[dataDF2==-9]<-NA
  diag(dataDF2)<-NA
  
  dataDF <- rbind(dataDF, dataDF2)
}

dataDF[is.na(dataDF)] <- 0
dataDF.sc <- scale(dataDF, scale = TRUE, center=FALSE)
dataDF.sc[is.na(dataDF.sc)] <- 0
dataMatrix <- data.matrix(dataDF.sc)
seed <- 50
somx <- 8
somy <- 6
set.seed(seed)

# Run SOM
dataSOM <- som(dataMatrix, grid=somgrid(somx, somy, "hexagonal"), rlen = 300, alpha=c(0.0005, 0.0001), maxNA.fraction = 0.6667)
#plot(dataSOM)


#Read country file
countryfile <- paste(wddir,"/country.csv", sep="")
countryDF <- read.csv(countryfile)
options(max.print=10000000)
countrylist <- countryDF$country

num_col <- length(dataSOM$unit.classif)/length(countrylist)

# Create matrix to store SOM position over time 
mdat<-matrix(, nrow = length(countrylist), ncol = num_col
             ,dimnames = list(countrylist,allyears
             ))

# Assign SOM positions over time to matrix 
for(col in 1:ncol(mdat)){
  
  id_first<- (col-1)*length(countrylist)+1
  id_last<- col*length(countrylist)
  
  mdat[,col ] <- dataSOM$unit.classif[id_first:id_last]
}


mdat<-mdat[rowSums(is.na(mdat)) != ncol(mdat),]

id_year_all <- data.frame(Ints=integer(),
                          Characters=character(),
                          Characters=character(),
                          stringsAsFactors = FALSE)

col_names <- c("SOM_ID", "Years","Countries")
colnames(id_year_all) <- col_names

# SOM trajectory drawing

# SOM outputs for exports or imports
SOMOutput <- paste("Output", flowdir, sep="") 
dir.create(SOMOutput)
# where to store the trajectory outputs
SOMOutputDir <- paste("./", SOMOutput, sep="")
setwd(SOMOutputDir)

for(i in 1:length(rownames(mdat))){
  
  
  # one country's name
  a<-rownames(mdat)[i]
  
  # year list
  yearlist <- c()
  id_list <- c()
  
  # first column is ids and second column is character about year range
  id_year <- data.frame(Ints=integer(),
                        Characters=character(),
                        Characters=character(),
                        stringsAsFactors = FALSE)
  
  col_names <- c("SOM_ID", "Years","Countries")
  colnames(id_year) <- col_names
  
  t<-1
  
  for(j in 1:length(mdat[a,])){
    
    id<-mdat[a,][[j]]
    if(is.na(id)) 
    {
      
      if(!is.null(id_list)){
        
        if(length(id_year$SOM_ID)!=0)
        {
          if(tail(id_year$SOM_ID,1)!=tail(id_list,1))
            
            if( length(yearlist)==1)
            {
              time_label<- yearlist[1]
              id_year[t,1]<-tail(id_list,1)
              id_year[t,2]<-as.character(time_label)
              id_year[t,3]<-a
              t<-t+1
            }
          else
          {
            time_label<- paste (yearlist[1],"-",tail(yearlist, n=1))
            id_year[t,1]<-tail(id_list,1)
            id_year[t,2]<-as.character(time_label)
            id_year[t,3]<-a
            t<-t+1
          } 
        }
        
      }
      
      next
      
    }
    yearnames<-colnames(mdat)
    
    
    
    
    if(is.null(yearlist))
    {
      yearlist <- c(yearnames[j])
      id_list <- c(id)
      
      
      
    }
    
    else{
      
      
      if(tail(id_list,1) != id || j== length(mdat[a,]))
        
      {

        
        # id changes
        last_id<-mdat[a,][[j-1]]
        if (tail(id_list,1) != id){ 
          if( length(yearlist)==1)
          {
            time_label<- yearlist[1]
            id_year[t,1]<-tail(id_list,1)
            id_year[t,2]<-as.character(time_label)
            id_year[t,3]<-a
            t<-t+1
          }
          else
          {
            time_label<- paste (yearlist[1],"-",tail(yearlist, n=1))
            id_year[t,1]<-tail(id_list,1)
            id_year[t,2]<-as.character(time_label)
            id_year[t,3]<-a
            t<-t+1
          }  
        }
        # last id
        if(j== length(mdat[a,])){
          
          
          if(
            tail(id_list, n=1) == id
          )
            
          {
            time_label<- paste (yearlist[1],"-",yearnames[j])
            id_year[t,1]<-tail(id_list,1)
            id_year[t,2]<-as.character(time_label)
            id_year[t,3]<-a
            t<-t+1
          }
          
          else
          {
            time_label<- yearnames[j]
            id_year[t,1]<-tail(id_list,1)
            id_year[t,2]<-as.character(time_label)
            id_year[t,3]<-a
            t<-t+1
          }
          
        }
        
        
        yearlist <- c(yearnames[j])
        id_list <- c(id)

        
      }
      
      else{
        
        yearlist <- c(yearlist,yearnames[j])
        id_list <- c (id_list,id)
        
      }
      
    } 
    

  }
  
  if(dim(id_year)[1]==0){
    time_label_last<- tail(yearlist,1)
    time_label<- paste (yearlist[1],"-",time_label_last)
    
    if(!is.null(id_list)){
      id_year[t,1]<-tail(id_list,1)
    }
    id_year[t,2]<-as.character(time_label)
    id_year[t,3]<-a
  }
  
  id_year_all<-rbind(id_year_all,id_year)
  
  
  filenames = paste(a,".png")
  png(filename=filenames,width = 2000, height = 1000)
  plot(dataSOM,main = "")
  
  
  # title name
  
  plottitle <- paste ("Multilateral interactions trajectory: ", flowdir, " of ", a)
  
  subtitle<-"The order of the countries on the x-axis in each cell is based on geographic regions: 
  north America, south America, Europe, Africa, Asia,Oceania "
  title (main = plottitle,cex.main = 3)
  mtext(side=3, line=-4, cex=2, subtitle)
  
  points(dataSOM$grid$pts[id_year$SOM_ID,1],dataSOM$grid$pts[id_year$SOM_ID,2],pch=20, col="yellow")
  lines(dataSOM$grid$pts[id_year$SOM_ID,1],dataSOM$grid$pts[id_year$SOM_ID,2],col="#311bf9",lty="solid")
  
  # Labeling Text
  id_year_order <- id_year[order(id_year$SOM_ID),] 
  change_ids<-which(duplicated(id_year_order$SOM_ID)==FALSE)
  difference<-diff(change_ids)
  
  fontsize = 1.2
  
  if(length(change_ids)==1)
  {
    part_id_year<-id_year_order[tail(change_ids,1):length(id_year_order$SOM_ID),]
    text(dataSOM$grid$pts[id_year_order$SOM_ID,1],dataSOM$grid$pts[id_year_order$SOM_ID,2], part_id_year$Years,cex = fontsize)
  }
  
  else
  {
    
    for(i in 1:length(difference))
    {
      if(difference[i]<=4){
        
        part_id_year<-id_year_order[change_ids[i]:(change_ids[i]+difference[i]-1),]
        text(dataSOM$grid$pts[part_id_year$SOM_ID,1],dataSOM$grid$pts[part_id_year$SOM_ID,2], part_id_year$Years, pos =c(1:difference[i]),cex = fontsize)
        
      }
      
      if(difference[i]>4)
      {
        part_id_year<-id_year_order[change_ids[i]:(change_ids[i]+difference[i]-1),]
        n<- ceiling(difference[i]/2)
        x_offset<- sort(rep(c(-0.3,0.3), n))
        y_offset<- rep(c(1:n), 2)/5
        text(dataSOM$grid$pts[part_id_year$SOM_ID,1]+x_offset,dataSOM$grid$pts[part_id_year$SOM_ID,2]-y_offset+0.25, part_id_year$Years, pos =3,cex = fontsize)
      }
      
    }
    
    # draw the last unique one 
    
    difference<-length(id_year_order$SOM_ID)-tail(change_ids,1)+1
    part_id_year<-id_year_order[tail(change_ids,1):length(id_year_order$SOM_ID),]
    if(difference==1)
    {
      text(dataSOM$grid$pts[part_id_year$SOM_ID,1],dataSOM$grid$pts[part_id_year$SOM_ID,2], part_id_year$Years, pos =3,cex = fontsize)
    }else
    {
      
      if(difference<=4){
        
        text(dataSOM$grid$pts[part_id_year$SOM_ID,1],dataSOM$grid$pts[part_id_year$SOM_ID,2], part_id_year$Years, pos =c(1:difference),cex = fontsize)
        
      }
      
      if(difference>4)
      {
        n<- ceiling(difference/2)
        x_offset<- sort(rep(c(-0.3,0.3), n))
        y_offset<- rep(c(1:n), 2)/5
        text(dataSOM$grid$pts[part_id_year$SOM_ID,1]+x_offset,dataSOM$grid$pts[part_id_year$SOM_ID,2]-y_offset+0.25, part_id_year$Years, pos =3,cex = fontsize)
      }
    }
  }
  
  dev.off() 
  
}

# dengrogram view with hcluster
# # caculate the sum of the euclidean distance between pairs of points with x,y locations in two trajectories
d <- function(n1,n2,max_dis){
  
  # find the ids for no-null ids
  
  a<-which(!is.na(n1[,1]))
  b<-which(!is.na(n2[,1]))
  c<-intersect(a,b)
  # 
  if(length(c)==0){
    d<-max_dis*115
  }
  else
  {
    x1_list<-n1[,1][c]
    y1_list<-n1[,2][c]
    x2_list<-n2[,1][c]
    y2_list<-n2[,2][c]
    
    x_value<-foreach(i=1:length(x1_list), .combine='c') %do% sqrt((y1_list[i]-y2_list[i])^2+(x1_list[i]-x2_list[i])^2)
    d<-sum(x_value)
    d<-d+(115-length(c))*max_dis
    
  }
  
  
  return(d)
}

mdat_dis_matrix<-matrix(, nrow = nrow(mdat), ncol = nrow(mdat)
                        ,dimnames = list(rownames(mdat),
                                         rownames(mdat))
)

lastsomid<-somx*somy
maxdistance_som<-sqrt((dataSOM$grid$pts[lastsomid,][1]-dataSOM$grid$pts[1,][1])^2+(dataSOM$grid$pts[lastsomid,][2]-dataSOM$grid$pts[1,][2])^2)
maxdistance_nooverlap<-ceiling(maxdistance_som)

for(row in 1:(nrow(mdat)-1)){
  
  for(col in (row+1):nrow(mdat)){
    mdat_dis_matrix[row,col]<-d(dataSOM$grid$pts[mdat[row,],],dataSOM$grid$pts[mdat[col,],],maxdistance_nooverlap)
  }
  
}


diag(mdat_dis_matrix) <- 0

mdat_dis_matrix[lower.tri(mdat_dis_matrix)] <- t(mdat_dis_matrix)[lower.tri(mdat_dis_matrix)]

options(max.print=10000000)


# hclust
cluster <- hclust(as.dist(mdat_dis_matrix), method="ward.D2")

clusterfig <- paste("./", "cluster", flowdir, ".png")
title <- paste(flowdir, " dendrogram")
png(filename = clusterfig, width = 2500, height = 1500, units = "px")
plot(cluster, main = title)
dev.off()
graphics.off()

