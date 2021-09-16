library(chirps)
library(sf)
library(sp)


#-------------------------------------------------------------------------
# sampledlocs is where you add your coordinates
# JUST THE LOCATIONS
#-------------------------------------------------------------------------

 sampled.locs <- data.frame(Name=NA,Longitude=NA,Latitude=NA)
 sampled.locs[1,] <- c("Wayanad",76.1320,11.6854)
 sampled.locs[2,] <- c("Kochi"  ,76.2673, 9.9312)
 sampled.locs[3,] <- c("Thrissur"  ,76.2144, 10.5276)



#-------------------------------------------------------------------------
# flood years is a list of the flood years that corresponds to the sampled locs
#-------------------------------------------------------------------------
 floodyears <- vector(mode="list",length=nrow(sampled.locs))
 floodyears[[1]] <- data.frame(year=c(2004,2011,2018,2013,2014,2019,2007),
                              col=rainbow(7))
                              #Wayanad
 floodyears[[2]] <- data.frame(year=c(2004,2011,2018,2013,2014,2019,2007),
                               col=rainbow(7))#Kochi
 floodyears[[3]] <- data.frame(year=c(2004,2011,2018,2013,2014,2019,2007),
                              col=rainbow(7))#Thrissur

#-------------------------------------------------------------------------
# making columns 2 and 3 of the locations table into numbers because it was originally saved as text
#-------------------------------------------------------------------------
 sampled.locs[,2] <- as.numeric(sampled.locs[,2])
 sampled.locs[,3] <- as.numeric(sampled.locs[,3])
 
 #-------------------------------------------------------------------------
 # making a copy of sampled.locs
 #-------------------------------------------------------------------------
 sampled.locs.sp <- sampled.locs
 
 
 #-------------------------------------------------------------------------
 # make it spatial by tellin R the NAME of The coordinate columns
 # also make an sf spatial
 #-------------------------------------------------------------------------
 coordinates(sampled.locs.sp) <- c("Longitude","Latitude")
 sampled.locs.sf <- st_as_sf(sampled.locs.sp)

 #-------------------------------------------------------------------------
 # download the CHIRPS data just for our 3 locations
 #-------------------------------------------------------------------------
 dt <- get_chirps(sampled.locs.sf, dates = c("1982-01-01","2019-12-31"))

 #Save data as txt. I also have it saved as csv
 #capture.output(dt, file = "My New File.txt")
 #txt_CHIRPS<- read.delim("My New File.txt")
 
 #-------------------------------------------------------------------------
 # make an empty space for outputs
 #-------------------------------------------------------------------------
  output <- vector(mode="list",length=nrow(sampled.locs))


 #-------------------------------------------------------------------------
 # create a loop, iterating a number from 1 : 3 or the number of rows in the 
 # location table
 #-------------------------------------------------------------------------
 for (L in 1:nrow(sampled.locs)){
    print(L)
  
   #-------------------------------------------------------------------------
   # just selecting the rows of the dt table with id == 1 or 2 or 3 or whatever L is
   # take a look at tidyverse filter and select
   #-------------------------------------------------------------------------
   rain <- dt[which(dt$id == L), ]

   #-------------------------------------------------------------------------
   # make a 3 day sum column that is empty. Then manually enter the statistical
   #calculations by including dates such as Jan 2 nd, Jan 2nd- 1day, and Jan 3
   #2nd + 1 day.
   #-------------------------------------------------------------------------
   rain$rain3day <- NA
  
   rain$rain3day[2:(nrow(rain)-1)]  <- rain$chirps[2:(nrow(rain)-1)] +
                                      rain$chirps[1:(nrow(rain)-2)] +
                                      rain$chirps[3:nrow(rain)]

   #-------------------------------------------------------------------------
   # make a 5 day sum column that is empty. Then manually enter the statistical
   #calculations by including dates such as Jan 3 rd, Jan 3rd- 1day,Jan 3rd- 2days,
   #Jan 3rd + 1 day,and Jan 3rd +2days
   #-------------------------------------------------------------------------
   rain$rain5day <- NA
   rain$rain5day[3:(nrow(rain)-2)]  <- rain$chirps[1:(nrow(rain)-4)] +
                                       rain$chirps[2:(nrow(rain)-3)] +
                                       rain$chirps[3:(nrow(rain)-2)] +
                                       rain$chirps[4:(nrow(rain)-1)] +
                                       rain$chirps[5:nrow(rain)]
   #-------------------------------------------------------------------------
   # make a cummulative rain sum column that is empty
   #-------------------------------------------------------------------------
   rain$cumulative <- NA

   #-------------------------------------------------------------------------
   #create a loop, iterating a number from 1 : the number of rows in the 
   # rain table. If decimal day of year is 001 then assign the cumulative column
   #in d to be 0 or else assign the value to be d-1 and then add the chirps column
   #-------------------------------------------------------------------------
   for(d in 1:nrow(rain)){
     if(format.Date(rain$date[d], "%j") %in% "001"){
       rain$cumulative[d] <- 0
     }else{
       rain$cumulative[d] <- rain$cumulative[d-1]+rain$chirps[d]
    }
   }

   #-------------------------------------------------------------------------
   # then get some custom dates. Create plotdate column in rain table that
   # counts and repeats from  2000-01-01 to 2000-12-30 through all rows. 
   #Create doy column that counts and repeats from 1 to 365 through all rows.
   #includes leap year number. Year 2000 is a leap year.
   #-------------------------------------------------------------------------
   rain$plotdate <- as.Date(paste("2000",format.Date(rain$date,"%j"),sep="-"),format="%Y-%j")
   rain$doy      <- format.Date(rain$date,"%j")
   
   #-------------------------------------------------------------------------
   # Create year column that draws data from the date column.
   # as.numeric function saves values that were originally text as numbers. 
   #-------------------------------------------------------------------------
   rain$year     <- as.numeric(format.Date(rain$date,"%Y"))
   
   #-------------------------------------------------------------------------
   # Create month column that draws data from the date column.
   # as.numeric function saves values that were originally text as numbers. 
   #-------------------------------------------------------------------------
   rain$month    <- as.numeric(format.Date(rain$date,"%m"))

   output[[L]] <- rain
}


 #-------------------------------------------------------------------------
 # August rainfall plot. Assign output to variable aug. create a loop, iterating 
 #a number from 1 : the number of rows in the sampled.locs table.
 #-------------------------------------------------------------------------
 totallocationscount<- nrow(sampled.locs)
 #-------------------------------------------------------------------------
 #dev.off() clears the plot window
 #-------------------------------------------------------------------------
 
 dev.off()
 layout(matrix(c(1,2,2,2,2,2,2,
                 1,3,3,3,3,3,3,
                 1,4,4,4,4,4,4),
                 nrow=3,
                 ncol=7,
               byrow = TRUE))
 #par(mar=c(0,4,0,0))
 #plot(1:10,1:10,col="white",axes=FALSE,xlab="",ylab="test")
 
 
 par(mar=c(2,2,0.5,0.5),
     oma=c(0,3,0,0),
     mfrow= c(3,1))
 
 aug <- output
 for (Q in 1:totallocationscount){
   print(Q)
   print(sampled.locs$Name[Q])
   
   #-------------------------------------------------------------------------
   # If L were 1 all id values one with month 8 are selected.
   #-------------------------------------------------------------------------
   aug[[Q]] <- output[[Q]][output[[Q]]$month %in% 8,]
   
   
   #-------------------------------------------------------------------------
   # To calculate aug5daymax create a data frame that includes years that are 
   #recognized as numeric values as a vector of the same length as the original
   #list calculating the maximum value of the 5-day sums for august of each year 
   #-------------------------------------------------------------------------
   aug5daymax <- data.frame(
     year=as.numeric(names(unlist(lapply(split(aug[[Q]]$rain5day,aug[[Q]]$year),max,na.rm=TRUE)))),
     max=unlist(lapply(split(aug[[Q]]$rain5day,aug[[Q]]$year),max,na.rm=TRUE)))
   
   #-------------------------------------------------------------------------
   # printing aug5daymax after calculating the quantile values of max
   # that fall in the 90th percentile.
   #-------------------------------------------------------------------------
   print(aug5daymax[which(aug5daymax$max>quantile(aug5daymax$max,0.9)),])
   #png(filename = paste("5_day_sum",sampled.locs$Name[L],".png",sep=""),width = 6,height = 4,units = "in",res = 300,pointsize = 10)
   
   
   #-------------------------------------------------------------------------
   #Plot aug5daymax year and max column
   #-------------------------------------------------------------------------
   
   plot(aug5daymax$year,
        aug5daymax$max,
        xlim= c(1980,2021),
        ylim= c(min(aug5daymax$max)*0.8,max(aug5daymax$max)*1.2),
        xlab="",
        ylab="",
        pch=16,
        tcl=-0.25,
        mgp=c(1.75,0.5,0),
        cex.axis=1)
   
   grid()
   lines(aug5daymax$year,
        aug5daymax$max,
        xlim= c(1980,2021),
        ylim= c(min(aug5daymax$max)*0.8,max(aug5daymax$max)*1.2),
        xlab="",
        ylab="",
        type="p",
        pch=16,
        tcl=-0.25,
        mgp=c(1.75,0.5,0),
        cex.axis=1)
   
   legend("topleft",
          legend = sampled.locs$Name[Q],
          xjust = 0,
          bg ="white")
          
  box()
   #-------------------------------------------------------------------------
   # identify function allows one to select points of graph that are 
   # labeled once the function is complete
   #-------------------------------------------------------------------------
   identify(aug5daymax$year,
            aug5daymax$max,
            labels=aug5daymax$year,
            cex=0.7)
 } 

 
#dev.off()

#-------------------------------------------------------------------------
# Cumulative Plot
#-------------------------------------------------------------------------
for (L in 1:nrow(sampled.locs)){
  maxrain=4000
  
    rain <- output[[L]]
    rain #
    unlist(lapply(split(output[[L]]$rain5day,output[[L]]$year),max,na.rm=TRUE))
    locationmaxrain<-unlist(lapply(split(output[[L]]$rain5day,output[[L]]$year),max,na.rm=TRUE))
    print(which(locationmaxrain>quantile(locationmaxrain,0.9)))
    plot(seq(from=as.Date("2000-01-01"),to=as.Date("2000-12-31"),by="d"),
         c(rep(1,length(seq(from=as.Date("2000-01-02"),to=as.Date("2000-12-31"),by="d"))),maxrain),
           cex=0,xlim=c(as.Date("2000-07-15"),as.Date("2000-09-15")),
         ylab="Cumulative rain",xlab="Date",axes=FALSE,main=paste("5 day sum",sampled.locs$Name[L]))
    grid();box()
    axis(side=2)
    axis(side=1,at=seq(from=as.Date("2000-01-01"),to=as.Date("2000-12-31"),by="m"),
         labels=format.Date(seq(from=as.Date("2000-01-01"),to=as.Date("2000-12-31"),by="m"),"%b"))

    for(y in unique(rain$year)){
        tmp <- rain[rain$year %in% y,]
        lines(tmp$plotdate,tmp$cumulative,col="grey")
    }
    for(y in 1:nrow(floodyears[[L]])){
      tmp <- rain[rain$year %in% floodyears[[L]]$year[y],]
      lines(tmp$plotdate,tmp$cumulative,col=floodyears[[L]]$col[y])
    }
    legend("bottomright",legend=floodyears[[L]]$year,lwd=1,col=floodyears[[L]]$col)
}








