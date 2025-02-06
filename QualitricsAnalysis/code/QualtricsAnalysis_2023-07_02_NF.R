# Setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

# input information #####
DataFilename = "./01_input/01_QualticsData.csv"
QuestionText = "./01_input/02_QuestionText.csv"
QuestionNumber = "./01_input/03_QuestionNumberAndScales.csv"

plot.colors.7 = c("sienna", "salmon2", "lightsalmon" ,
                  "gray","paleturquoise","skyblue","dodgerblue4" )
plot.colors.5 = c("sienna", "salmon2", "gray", "skyblue","dodgerblue4")

plot.colors.4 = c("sienna", "salmon2", "skyblue","dodgerblue4")
plot.colors.6 = c("sienna", "salmon2", "gray", "skyblue","dodgerblue4", "gray41")

# Functions Section ####

# Likert mean function ####
# takes column headings as numeric and multiples by the number of responses
# to get the total.likert.scale score for a given likert scale 

likert_mean_function<-function(x){
  # x is a sub-question (row) within the  matrix of responses for a question
  likert_mean<-(sum(as.numeric(names(x))*x)/sum(x))
  return(likert_mean)
}

# Function to automatically fill the unanswered likert scales ####
blank_values_function<-function(x, total.likert.scale, Df){
  # x is the question number
  # total.likert.scale is the total.likert.scale number of options in the likert scale
  # Df is the dataframe of the qualtrics data file
  
  Q_counts <- tapply(Df[[x]][!(Df[[x]] == "")], 
                     Df[[x]][!(Df[[x]] == "")], length, default=0)
  
  sequence <- as.integer(names(Q_counts))
  len<-length(Q_counts)
  seq <- 1:total.likert.scale
  missing <- seq[!seq %in% sequence]
  Q_counts <- c(Q_counts, rep(0,(total.likert.scale-len)))
  names(Q_counts) <- c(names(Q_counts[1:len]),as.character(missing))
  Q_counts <- Q_counts[order(names(Q_counts))] 
  return(Q_counts)
}

# Function to read question titles from input question string ####

read_question_titles_from_String_function <-function(Questions){
  
  mat <- matrix(nrow = 0, ncol = 1)
  for(que in Questions){
    x <- strsplit(as.character(que)," - ")[[1]]
    y <- x[length(x)]
    if(nchar(y) >80){
      y1 <- substring(y,1,80)
      y2 <- substring(y,81,nchar(y))
      y <- paste(y1, "\n",y2,sep=" " )
    }
    mat <- rbind(mat,y)
  }
  return(mat)
}

# Create Graph function ####
make_plot <- function(Q_summary, question_number) {
  
  if(total.likert.scale == 7){
    color = plot.colors.7
  }
  else if(total.likert.scale == 5){
    color = plot.colors.5
  }
  else if(total.likert.scale == 4){
    color = plot.colors.4
  }
  else if(total.likert.scale == 6){
    color = plot.colors.6
  }
  
  Q_summary_plot<-Q_summary[order(Q_summary[,"rownum"], decreasing=T),]
  
  my.file.name <- paste("./03_output/",question_number,"_barplot.png", sep = "")
  
  # width/height here are for the actual barplot, NOT the space around barplot
  png(my.file.name, width=7.5, height=4.5, unit="in", res=1000) 
  
  #margins, top, left, bottom and right, its same for all graphs, and 
  
  par(mar=c(4,23,4,5)) 
  
  barplot_spacing<-
    barplot(t(Q_summary_plot[,(total.likert.scale+2):(2*total.likert.scale+1)]),
            horiz=T, 
            col= color,
            las=1, axes=F,names=NULL,axisnames=F,cex.main=1, 
            cex.names=.8,cex.axis=.8,border= NA,
            yaxt ="n",xaxt="n" )
  
  axis(1, line=-.1,at=seq(0,100,by=20), labels=seq(0,100,by=20), 
       cex.axis=0.7, padj=-2, hadj=0.5,tck=-0.02)
  
  text(51, par("usr")[3] - (par("usr")[4]*2/15),
       "Frequency (%)", cex=0.8, xpd=T)
  
  par(lheight=0.79)
  text(-1.5, barplot_spacing, rev(rownames(Q_summary)), xpd=T, cex=.8, adj=c(1,NA))
  x<-rev((round(Q_summary[,"likert_mn"],1)-1)*25)
  
  text(x, barplot_spacing, rev(paste(format(round(Q_summary[,"likert_mn"], 1),
                                            nsmall=1))),
       col="white", font=2, cex=.6)
  
  
  # Depending on the length of the scale values adjust the total space 
  x.start <- -30
  x.end <- 100
  total.legend.space <- x.end - x.start
  
  axis(3, line=1.5, col="white", 
       at=seq(x.start,x.end,by=(total.legend.space/(total.likert.scale-1))),
       labels=chartr(" ","\n",ScaleValues),
       tck=0.02,padj=1, col.ticks="white", xpd = TRUE,gap.axis = -1,
       cex.axis=0.5, col.axis="black", font=0.5)
  
  ycordinate <- par("usr")[4] + (par("usr")[4]/10)
  
  points(x = seq(-30,100,by=(130/(total.likert.scale-1))),
         y = rep(ycordinate,total.likert.scale),
         pch = 15,
         col=color,
         xpd = TRUE)
  
  axis(3, line=0.65, col="black", at=seq(0,100,by=(100/(total.likert.scale-1))),
       labels=seq(1:total.likert.scale),
       tck=0.02,padj=4.5, col.ticks="black", 
       cex.axis=0.7, col.axis="black", font=1)
  
  
  text(109,barplot_spacing,
       paste("n=",rev(Q_summary[,"likert_total.likert.scale_n"]), sep=""),
       xpd=T, cex=.8)
  
  dev.off() #close the plot
}

# Import Data File and question file ####

# header is the first row with Question numbers
# we will skip the second two rows, and import data from the third row
headers <- colnames(read.csv(DataFilename))
Df.Data <- read.csv(DataFilename, header = FALSE)[-(1:3),]
colnames(Df.Data) <- headers

# Import Questions
Df.Questions <- read.csv(QuestionText)
Df.QuestionNumbers <- read.csv(QuestionNumber)

# For loop on every question ####

for (questions in 
     Df.QuestionNumbers$Question[!(is.na(Df.QuestionNumbers$Question))]){
  
  #for every question, read related info from DF ####
  total.likert.scale <- 
    Df.QuestionNumbers$TotalScale[Df.QuestionNumbers$Question == questions &
                                    !(is.na(Df.QuestionNumbers$TotalScale))]
  ScaleValues <- 
    strsplit(Df.QuestionNumbers$Scale[Df.QuestionNumbers$Question == questions &
                                !(is.na(Df.QuestionNumbers$Scale))], ",")[[1]]
  
  PlotWidthAdjust <- 
    Df.QuestionNumbers$PlotWidthAdjust[Df.QuestionNumbers$Question == questions &
                                    !(is.na(Df.QuestionNumbers$PlotWidthAdjust))]
  PlotWidthAdjust
  #reading other details from the second file
  SubQuestionNumbers<-Df.Questions$SubQuestion[
    grep(pattern=paste(questions,"_",sep=""),Df.Questions$SubQuestion)]
  
  
  QuestionTitles <- ifelse(is.na(Df.Questions$AlternateText[
    grep(pattern=paste(questions,"_",sep=""),Df.Questions$SubQuestion)]),
    
    Df.Questions$QuestionTitle[
      grep(pattern=paste(questions,"_",sep=""),Df.Questions$SubQuestion)],
    
    Df.Questions$AlternateText[
      grep(pattern=paste(questions,"_",sep=""),Df.Questions$SubQuestion)]
  )
  
  tites <-read_question_titles_from_String_function(QuestionTitles)
  rownames <- c(as.character(tites[1:length(tites)]))
  
  # Replacing the unanswered scales with 0 of likert questions ####
  
  #create data frame with 0 rows and 5 / total.likert.scale scale columns
  mat = matrix(nrow = 0, ncol = total.likert.scale)
  
  #provide column names
  colnames(mat) <- seq <- 1:total.likert.scale
  
  for (q in SubQuestionNumbers) {
    answers <- blank_values_function(q, total.likert.scale, Df.Data)
    mat <- rbind(mat,answers)  
  }
  
  # Calculate the mean and percent values ####
  Q_likert_means<-t(t(apply(mat, 1,likert_mean_function)))
  
  Q_likert_total.likert.scale_n<-t(t(apply(mat, 1,sum)))
  
  Q_likert_pct<-round(prop.table(mat,1)*100, digits=1)
  
  # Create the summary of the question ####
  Q_summary <- cbind(mat, Q_likert_total.likert.scale_n, Q_likert_pct,
                     round(Q_likert_means, digits=1), 
                     seq(1,length(SubQuestionNumbers),1)) 
  
  colnames(Q_summary)<-c(colnames(mat), "likert_total.likert.scale_n", 
                         colnames(Q_likert_pct),"likert_mn", "rownum") 
  
  rownames(Q_summary)<-rownames
  
  # plot the graph
  make_plot(Q_summary,questions)
  
}

# End ####




