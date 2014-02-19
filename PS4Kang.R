## Problem Set 4. 
## Myunghoon Kang

## Reading in data without a clean format
## making a function
netlogo <- function(x){
  
  # Both "file.csv" and "file" can be input because the following
  # codes make any input as the form of "file.csv"
  x <- gsub(".csv","",x)
  file.name <- paste(x, ".csv", sep="")
  
  # extracting file name and the date on which the simulation conducted
  file <- scan(file.name, what="", n=1, skip=1, sep=",")
  date <- scan(file.name, what="", n=1, skip=2, sep=",")
  
  # Because '\', ':' cannot be used as the name of directory in Windows system,
  # the following codes substitutes those strings to '.'. Also, the blank is
  # replaced with '_'.
  # 'toplevel' is the name of top level directory
  toplevel <- paste(file,date,sep=" ")
  toplevel <- gsub("\\/|\\:",".",toplevel)
  toplevel <- gsub(" ","_",toplevel)
  
  ## making a directory
  dir.create(toplevel)
  sapply(c("Globals", "Turtles", "Plots"), function(x) dir.create(paste(toplevel,"/", x ,sep="")))
  sapply(c("PositionPlot", "WinnersPlot","PolarizationPlot","IncumbentPercentagePlot"),
         function(x) dir.create(paste(toplevel,"/Plots/",x,sep="")))
  
  ## making a Global.R
  # The following codes extract the names and the values of Globals.
  # Given the structure of .NetLogo file, the name of values in Globals will begin at
  # 8th row in .csv file regardless of other parameters. 
  # ']','[', and quotation marks are removed. After this, each element of a vectors
  # is splited. 
  globals.begin <- 8
  glname <- scan(file.name, what="", n=-1, nlines=1, skip=globals.begin, sep=",")
  glvalues <- scan(file.name, what="", n=-1, nlines=1, skip=globals.begin+1, sep="," )
  glvalues <- gsub("\\]|\\[|^\"|\"$", "", glvalues)
  global <-  sapply(glvalues, strsplit, split=" ")
    
  # The following code will convert each element into an appropriate class such as
  # numeric, logical, and characters. The reason is that I need an information 
  # such as the number of candidates, voters, activists and so forth. Because this
  # function aims to work for generic .NetLogo file, we need to extract the relevant
  # information from the .NetLogo file rather than using specific numbers which
  # is only appropriate to the results of specific simulation.
  # To do this, I convert all elements in to numeric first. As a result, I get
  # NA from non-numeric values. Using this, I classify numeric values and character values.
  # So, Warning message is intentional. To avoid any confusion from the warning message,
  # I suppress the warning message. After this, I again classify elements such as 'TRUE' and 
  # "FALSE' as logical. 
  suppressWarnings(is.not.numeric <- is.na(sapply(global, as.numeric)))
  is.logical <- sapply(1:length(global), function(i) is.not.numeric[[i]]==TRUE & any(global[[i]] %in% c("TRUE","FALSE")))
  global <- sapply(1:length(global), function(i) if(is.not.numeric[i]==FALSE) as.numeric(global[[i]]) else global[[i]])
  global <- sapply(1:length(global), function(i) if(is.logical[i]==TRUE) as.logical(global[[i]]) else global[[i]])
  
  # Setting the names to the each sub-list in the list of 'global'
  names(global) <- glname
  
  # storing 'global' as a .R file in the designated directory.
  dump("global", file=paste(toplevel,"/Globals/Globals.R",sep=""))
  
  ## making a data for turltes directory
  # extrating a relevant information from the 'Global' list.
  # the number of districts, the number of activists, the number of voters, the number of parties
  # the number of candidates, and the number of total observations.
  n.districts <- unlist(global["num-districts"])
  n.activists <- n.districts*unlist(global["num-activists-per-district"])
  n.voters <- n.districts*unlist(global["num-voters-per-district"])
  n.party <- unlist(global["n.parties"])
  n.candi <- n.districts*n.party
  total.obs <- n.districts+n.activists+n.voters+n.party+n.candi
  
  # given the structure of .NetLogo file, the name of turtles begin 4 rows
  # after the row which contains the name of data in Globals.
  turtles.begin <- globals.begin+4
  
  # extracting the name of column for turtles
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=turtles.begin, sep=",")
  # drop meaningless elements
  col.names <- subset(col.names, col.names!="")
  
  # extracting the values of turtles and making it as a data frame. 
  turtles <- scan(file.name, what="", n=-1, nlines=total.obs, skip=turtles.begin+1, sep=",")
  turtles <- gsub("\\]|\\[|\\{breed |\\{turtles |\\}|^\"|\"$", "", turtles)
  turtles <- matrix(turtles, total.obs, byrow=TRUE)
  turtles <- data.frame(turtles[,1:length(col.names)], stringsAsFactors=FALSE )
  colnames(turtles) <- col.names
  
  # missing values in turtles data frame will be coded as NA
  turtles[turtles[,]==""] <- NA
  
  ## making separate .csv file cotaining the relevant information for districts
  # subsetting the values of districts from data frame 'turtles'
  districts <- subset(turtles, turtles$breed=="districts")
  
  # dropping the missing values and constant values for all subjects 
  is.na.all <- which(sapply(districts, function(x) sum(is.na(x))==n.districts))
  is.constant <- which(sapply(districts, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  districts <- districts[,-drop]
  
  # extracting the column names for districts after dropping irrelevant data.
  d.name <- colnames(districts)
  
  # Breaking down the vectors into separate columns. 
  districts <- unlist(sapply(districts, strsplit, split=" "))
  districts <- matrix(districts, nrow=n.districts, byrow=FALSE)
  
  # Re-arranging the appropriate column names for each column. 
  d.name[9:10] <- d.name[7]
  d.name[7:8] <- d.name[6]  
  colnames(districts) <- d.name
  
  # saving the data as .csv file in the designated directory
  write.csv(districts, paste(toplevel,"/Turtles/Districts.csv", sep=""), row.names=FALSE)
  
  ## making separate .csv file cotaining the relevant information for voters.
  # this codes are same with what used in districts. To avoid redundancy, 
  # I will omit documentation except when there is some different things. 
  voters <- subset(turtles, turtles$breed=="voters")
  
  is.na.all <- which(sapply(voters, function(x) sum(is.na(x))==n.voters))
  is.constant <- which(sapply(voters, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  voters <- voters[,-drop]
  v.name <- colnames(voters)
  voters <- unlist(sapply(voters, strsplit, split=" "))
  voters <- matrix(voters, nrow=n.voters, byrow=FALSE)
  v.name[11:13] <- v.name[9]
  v.name[10] <- v.name[8]  
  v.name[8:9] <- v.name[7]
  colnames(voters) <- v.name
  
  write.csv(voters, paste(toplevel,"/Turtles/Voters.csv", sep=""), row.names=FALSE)
  
  ## making separate .csv file cotaining the relevant information for activists.
  activists <- subset(turtles, turtles$breed=="activists")
  
  is.na.all <- which(sapply(activists, function(x) sum(is.na(x))==n.activists))
  is.constant <- which(sapply(activists, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  activists <- activists[,-drop]
  a.name <- colnames(activists)
  activists <- unlist(sapply(activists, strsplit, split=" "))
  activists <- matrix(activists, nrow=n.activists, byrow=FALSE)
  a.name[14] <- a.name[10]
  a.name[11:13] <- a.name[9]  
  a.name[9:10] <- a.name[8]
  colnames(activists) <- a.name
  
  write.csv(activists, paste(toplevel,"/Turtles/Activists.csv", sep=""), row.names=FALSE)  
  
  ## making separate .csv file cotaining the relevant information for parties.
  parties <- subset(turtles, turtles$breed=="parties")
  
  is.na.all <- which(sapply(parties, function(x) sum(is.na(x))==n.party))
  is.constant <- which(sapply(parties, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  parties <- parties[,-drop]
  p.name <- colnames(parties)
  parties <- unlist(sapply(parties, strsplit, split=" "))
  parties <- matrix(parties, nrow=n.party, byrow=FALSE)
  p.name[223] <- p.name[12]
  p.name[222] <- p.name[11]  
  p.name[219:221] <- p.name[10]
  p.name[11:218] <- p.name[9]
  p.name[10] <- p.name[8]
  p.name[8:9] <- p.name[7]
  colnames(parties) <- p.name
  
  write.csv(parties, paste(toplevel,"/Turtles/Parties.csv", sep=""), row.names=FALSE)  
  
  ## making separate .csv file cotaining the relevant information for candidates.
  candidates <- subset(turtles, turtles$breed=="cands")
  
  is.na.all <- which(sapply(candidates, function(x) sum(is.na(x))==n.candi))
  is.constant <- which(sapply(candidates, function(x) length(unique(x))==1))
  drop <- c(is.na.all, is.constant)
  candidates <- candidates[,-drop]
  c.name <- colnames(candidates)
  candidates <- unlist(sapply(candidates, strsplit, split=" "))
  candidates <- matrix(candidates, nrow=n.candi, byrow=FALSE)
  c.name[14:16] <- c.name[12]
  c.name[13] <- c.name[11]  
  c.name[12] <- c.name[10]
  c.name[11] <- c.name[9]
  c.name[10] <- c.name[8]
  c.name[8:9] <- c.name[7]
  colnames(candidates) <- c.name
  
  write.csv(candidates, paste(toplevel,"/Turtles/Candidates.csv", sep=""), row.names=FALSE) 
  
  ## extrating data for Plots directory
  # As I mentioned above, we need to know the number of row in which relevant values
  # for each sub-directory begins.
  # I assume that the basic structure is given. It means that the number of columns
  # is given. However, I assume that the length of row will vary according to
  # the number of subjects. 
  # So, it is necessary to find out the first row for each data section.
  
  # the number of row in which 'patches' section begin
  patches.begin <- turtles.begin+total.obs+2
  patches.length <- (abs(unlist(global['min-pxcor']))+abs(unlist(global['max-pxcor']))+1)*(abs(unlist(global['min-pycor']))+abs(unlist(global['max-pycor']))+1)+13
  
  ## data for D1
  # the number of row in which 'D1 overall information' section begin
  D1.info.begin <- patches.begin+patches.length 
  
  # the numer of pen
  n.of.pen <- scan(file.name, what="", n=8, nlines=1, skip=D1.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  
  # the number of x. For some reasons, the NetLogo file begins its count from
  # 1. So, if the number of x is larger than 0, we need to add 1 to it and 0 otherwise.
  n.of.x <- scan(file.name, what="", n=6, skip=D1.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  
  ## the number of row in which the values of position in D1 begin.
  position.begin <- D1.info.begin+n.of.pen+6
  
  # extrating the column names for D1 position.
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=position.begin-2, sep=",")
  col.names <- gsub("^\"|\"$","",col.names)
  col.names <- subset(col.names, col.names != "")
  col.names[1] <- "RedIncumbents"
  col.names[2] <- "BlueIncumbents"
  
  # I guess that this simulation can have at best six dimensions. 
  # So, the dimension can vary according to the setting of the simulation.
  # Therefore, if the dimension is used in the simulation(i.e. x>0),
  # I will extract the data for this dimension and writing as .csv file, skip otherwise.
  if(n.of.x !=0){
   plot.d1 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin, sep=",")
   plot.d1 <- subset(plot.d1, plot.d1!="") # drop meaningless elements
   plot.d1 <- matrix(plot.d1, n.of.x, 4*n.of.pen, byrow=TRUE) # making a matrix
   plot.d1 <- plot.d1[,seq(2,(4*n.of.pen-2), by=4)] #extracting Y values for each.
   colnames(plot.d1) <- col.names # setting the column names
   period <- 0:(n.of.x-1) # period(x) 
   plot.d1 <- cbind(period, plot.d1) # combine the data with period(x)
  
   
   write.csv(plot.d1, paste(toplevel,"/Plots/PositionPlot/D1.csv", sep=""), row.names=FALSE) 
  } 
    
  ## data for D2.
  # the following codes are identical with what used in D1.
  interval <- n.of.x+3 # This is the number of rows between the end of the data and
                       # beginning of the data for new dimension.
                       # Also, the number of x (n.of.x) will be updated 
                       # every section.
    
  D2.info.begin <- position.begin+interval # We can specify row number in which
                                           # new dataset begins by doing this.
  
  n.of.pen <- scan(file.name, what="", n=8, skip=D2.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D2.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.2 <- D2.info.begin + n.of.pen+6
  
  if(n.of.x !=0){
  plot.d2 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.2, sep=",")
  plot.d2 <- subset(plot.d2, plot.d2!="")
  plot.d2 <- matrix(plot.d2, n.of.x, 4*n.of.pen, byrow=TRUE)
  plot.d2 <- plot.d2[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(plot.d2) <- col.names
  period <- 0:(n.of.x-1)
  plot.d2 <- cbind(period, plot.d2)
  write.csv(plot.d2, paste(toplevel,"/Plots/PositionPlot/D2.csv", sep=""), row.names=FALSE) 
  }
  
  ## data for D3.
  interval <- n.of.x+3
  D3.info.begin <- position.begin.2+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D3.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D3.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.3 <- D3.info.begin + n.of.pen+6
  
  if(n.of.x !=0){
  plot.d3 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.3, sep=",")
  plot.d3 <- subset(plot.d3, plot.d3!="")
  plot.d3 <- matrix(plot.d3, n.of.x, 4*n.of.pen, byrow=TRUE)
  plot.d3 <- plot.d3[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(plot.d3) <- col.names
  period <- 0:(n.of.x-1)
  plot.d3 <- cbind(period, plot.d3)
  write.csv(plot.d3, paste(toplevel,"/Plots/PositionPlot/D3.csv", sep=""), row.names=FALSE) 
  }
  
  ## data for D4.
  interval <- n.of.x+3
  D4.info.begin <- position.begin.3+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D4.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D4.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.4 <- D4.info.begin + n.of.pen+6
  
  if(n.of.x !=0){
    plot.d4 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.4, sep=",")
    plot.d4 <- subset(plot.d4, plot.d4!="")
    plot.d4 <- matrix(plot.d4, n.of.x, 4*n.of.pen, byrow=TRUE)
    plot.d4 <- plot.d4[,seq(2,(4*n.of.pen-2), by=4)]
    colnames(plot.d4) <- col.names
    period <- 0:(n.of.x-1)
    plot.d4 <- cbind(period, plot.d4)
    write.csv(plot.d4, paste(toplevel,"/Plots/PositionPlot/D4.csv", sep=""), row.names=FALSE) 
  }
  
  ## data for D5.
  interval <- n.of.x+3
  D5.info.begin <- position.begin.4+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D5.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D5.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.5 <- D5.info.begin + n.of.pen+6

  if(n.of.x !=0){
    plot.d5 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.5, sep=",")
    plot.d5 <- subset(plot.d5, plot.d5!="")
    plot.d5 <- matrix(plot.d5, n.of.x, 4*n.of.pen, byrow=TRUE)
    plot.d5 <- plot.d5[,seq(2,(4*n.of.pen-2), by=4)]
    colnames(plot.d5) <- col.names
    period <- 0:(n.of.x-1)
    plot.d5 <- cbind(period, plot.d5)
    write.csv(plot.d5, paste(toplevel,"/Plots/PositionPlot/D5.csv", sep=""), row.names=FALSE) 
  }
  
  ## data for D6.
  interval <- n.of.x+3
  D6.info.begin <- position.begin.5+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=D6.info.begin, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=D6.info.begin+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  position.begin.6 <- D6.info.begin + n.of.pen+6
  
  if(n.of.x !=0){
    plot.d6 <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=position.begin.6, sep=",")
    plot.d6 <- subset(plot.d6, plot.d6!="")
    plot.d6 <- matrix(plot.d6, n.of.x, 4*n.of.pen, byrow=TRUE)
    plot.d6 <- plot.d6[,seq(2,(4*n.of.pen-2), by=4)]
    colnames(plot.d6) <- col.names
    period <- 0:(n.of.x-1)
    plot.d6 <- cbind(period, plot.d6)
    write.csv(plot.d6, paste(toplevel,"/Plots/PositionPlot/D6.csv", sep=""), row.names=FALSE) 
  }
  
  ## data for winner.
  interval <- n.of.x+3
  winner.info <- position.begin.6+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=winner.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=winner.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  winner.begin <- winner.info+n.of.pen+6
    
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=winner.begin-2, sep=",")
  col.names <- subset(col.names, col.names!="")
  winner <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=winner.begin, sep=",")
  winner <- subset(winner, winner!="")
  winner <- matrix(winner, n.of.x, 4*n.of.pen, byrow=TRUE)
  winner <- winner[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(winner) <- col.names
  period <- 0:(n.of.x-1)
  winner <- cbind(period, winner)
  write.csv(winner, paste(toplevel,"/Plots/WinnersPlot/Winner.csv", sep=""), row.names=FALSE) 
  
  # plot winner. 
  # To set the range of y axis, we need to use the maximum and minimum value
  # from the data as the boundary for the axis.
  # the number of x (n.of.x) can be used as a x axis. 
  min.blue <- as.numeric(min(winner[,2]))
  min.red <- as.numeric(min(winner[,4]))
  max.blue <- as.numeric(max(winner[,2]))
  max.red <- as.numeric(max(winner[,4]))
  min <- min(min.blue, min.red)
  max <- max(max.blue, max.red)
  
  # save plot as a pdf file. 
  pdf(paste(toplevel,"/Plots/WinnersPlot/Winner.pdf",sep=""), width=12)
  plot(winner[,1], winner[,2], col="blue", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n', xlab="period", ylab="percentage (%)", 
       main="Percentage of candidates from each party\n \"Won\" in each cycle", pch=19)
  par(new=TRUE)
  plot(winner[,1], winner[,4], col="red", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n',xlab="", ylab="", pch=19)
  axis(1, at=c(seq(0,n.of.x,by=1)))
  axis(2, at=c(seq(floor(min), ceiling(max), by=5)))
  legend("topleft",legend=c("BLUE Party", "RED Party"),
          cex=1, lty=c(1,1), col=c("blue","red"), bty='n')
  dev.off()
  
  ## data for polarization.
  interval <- n.of.x+3
  polar.info <- winner.begin+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=polar.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=polar.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  polar.begin <- polar.info+n.of.pen+6
  
  col.names <- scan(file.name, what="", n=-1, nlines=1, skip=polar.begin-2, sep=",")
  col.names <- subset(col.names, col.names!="")
  polar <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=polar.begin, sep=",")
  polar <- subset(polar, polar!="")
  polar <- matrix(polar, n.of.x, 4*n.of.pen, byrow=TRUE)
  polar <- polar[,seq(2,(4*n.of.pen-2), by=4)]
  colnames(polar) <- col.names
  period <- 0:(n.of.x-1)
  polar <- cbind(period, polar)
  write.csv(polar, paste(toplevel,"/Plots/PolarizationPlot/Polarization.csv", sep=""), row.names=FALSE) 
  
  # plot and save polarization
  min.total <- as.numeric(min(polar[,2]))
  min.voter <- as.numeric(min(polar[,3]))
  min.activ <- as.numeric(min(polar[,4]))
  max.total <- as.numeric(max(polar[,2]))
  max.voter <- as.numeric(max(polar[,3]))
  max.activ <- as.numeric(max(polar[,4]))
  min <- min(min.total, min.voter, min.activ)
  max <- max(max.total, max.voter, max.activ)
  pdf(paste(toplevel,"/Plots/PolarizationPlot/PolarizationPlot.pdf",sep=""), width=12)
  plot(polar[,1], polar[,2], col="black", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n', xlab="period", ylab="", 
       main="Euclidean distance between the mean position", pch=19)
  par(new=TRUE)
  plot(polar[,1], polar[,3], col="green", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n',xlab="", ylab="", pch=19)
  par(new=TRUE)
  plot(polar[,1], polar[,4], col="purple", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n',xlab="", ylab="", pch=19)
  axis(1, at=c(seq(0,n.of.x,by=1)))
  axis(2, at=c(seq(floor(min), ceiling(max), by=1)))
  legend("topleft",legend=c("Candidates", "Voters", "Activists"), 
          cex=1, lty=c(1,1,1), col=c("black","green","purple"), bty='n')
  dev.off()
  
  ## data for incumbents
  interval <- n.of.x+3
  Incum.info <- polar.begin+interval
  n.of.pen <- scan(file.name, what="", n=8, skip=Incum.info, sep=",")
  n.of.pen <- as.numeric(n.of.pen[8])
  n.of.x <- scan(file.name, what="", n=6, skip=Incum.info+3, sep=",")
  n.of.x <- ifelse(as.numeric(n.of.x[6])!=0,as.numeric(n.of.x[6])+1,0)
  incum.begin <- Incum.info+n.of.pen+6
  
  percent <- scan(file.name, what="", n=-1, nlines=n.of.x, skip=incum.begin, sep=",")
  percent <- subset(percent, percent!="")
  percent <- matrix(percent, n.of.x, 4*n.of.pen, byrow=TRUE)
  percent <- percent[,seq(2,(4*n.of.pen-2), by=4)]
  period <- 0:(n.of.x-1)
  percent <- cbind(period, percent)
  write.csv(percent, paste(toplevel,"/Plots/IncumbentPercentagePlot/IncumbentWins.csv", sep=""), row.names=FALSE) 

  # plot and save incumbents
  min <- as.numeric(min(percent[,2]))
  max <- as.numeric(max(percent[,2]))
  pdf(paste(toplevel,"/Plots/IncumbentPercentagePlot/IncumbentWins.pdf",sep=""), width=12)
  plot(percent[,1], percent[,2], col="black", type='l', xlim=c(0,n.of.x), 
       ylim=c(floor(min), ceiling(max)), bty='n', xaxt='n', yaxt='n', xlab="period", ylab="percentage (%)", 
       main="the percentage of incumbent candidates in each party\n that are \"winning\" in each time period", pch=19)
  axis(1, at=c(seq(0,n.of.x,by=1)))
  axis(2, at=c(seq(floor(min), ceiling(max), by=5)))
  dev.off()
}

x <- "NetLogo.csv"
netlogo(x)
