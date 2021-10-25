## Problem 1

setwd("C:\\Users\\Username\\Desktop\\Mini_Project")
##setting up the working directory

unzip("rprog_data_specdata.zip")
##unzipping the zip file downloaded from the LMS

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  name<- c(list.files(directory))
  ## storing the name of the CSV files in the directory in a character vector 'name'
  
  useful_data<-{}
  
  ## initializing the vector useful_data as an empty vector
  ## vector useful_data will contain the data with values (ignoring NA values) in the 
  ## specified pollutant and monitor list
  
  
  for(i in id){
    fname<-name[i]
    loc<-gsub(" ", "", paste(directory, "/", fname))
    dataset<-read.csv(loc)
    interested_data<-dataset[pollutant]
    useful_data<- c(useful_data, interested_data[!is.na(interested_data)])
  }
  mean(useful_data)
  
  ## For the for loop
  ## The loop will iterate based on the number of monitor ID numbers indicated
  
  ## The 2nd line of code in the for loop indicates
  ## 'fname' as a character vector that will store 
  ## the name of CSV files in the directory based on their monitor ID numbers
  
  ## The 3rd line of code in the for loop indicates
  ## 'loc' as a character vector that will store the name of the directory of
  ## of the indicated Monitor ID numbers 
  ## the gsub() function will eliminate the the space in the outout of the
  ## paste function() that will be used in merging the directory
  ## name and the name of the CSV file
  
  ## The 4th line of code in the for loop will read the file 
  ## that contains the values of the indicated monitors
  ## and store them in the data frame 'dataset'
  
  ## The 5th line of code in the for loop indicates
  ## 'interested_data' as a numeric vector that will store the values 
  ## in the indicated pollutant from which we will calculate the mean
  
  ## After every iteration of the for loop, the values in the 
  ## numeric vector 'useful_data' will change. The 5th line of code will
  ## ensure that the values stored in previous iteration of 
  ## for loop will be included until the last iteration of the for loop
  
  ## the function mean() will calculate the values stored in the vector 'useful_data'
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

## Calling the  function pollutant mean
## that will return the mean of the specified pollutant across all monitors list
## in the specified 'id' vector (ignoring NA values)

## Problem 2

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  name<- c(list.files(directory))
  ## storing the name of the CSV files in the directory in a character vector 'name'
  
  complete_cases<- data.frame(id=numeric(0), nobs=numeric(0))
  
  ## initializing the 'complete_cases' as an empty data frame
  ## that will store the numeric values of the object 'id' and 'nobs' 
  

  for(i in id){
    fname<-name[i]
    loc<-gsub(" ", "", paste(directory, "/", fname))
    dataset<-read.csv(loc)
    
    count=0
    
    ##initializing the value of object count to 0 before the iteration of the second loop
    ## This is to ensure that the number of complete cases for the specified monitor
    ## will not add up to the number of complete cases on the next monitor
    
    for(k in 1:nrow(dataset))
    {
      if(!is.na(dataset$sulfate[k])&!is.na(dataset$nitrate)[k]){
        count=count+1
      }else{
        count=count+0
      }
    }
    complete_cases<- rbind(complete_cases, data.frame(id=i, nobs=count))
  }
  complete_cases
  
  ## For the first for loop
  ## The loop will iterate based on the number of indicated monitor ID
  
  ## The 2nd line of code in the for loop indicates
  ## 'fname' as a character vector that will store 
  ## the name of CSV files in the directory based on their monitor ID numbers
  
  ## The 3rd line of code in the for loop indicates
  ## 'loc' as a character vector that will store the name of the directory of
  ## of the indicated Monitor ID numbers 
  ## the gsub() function will eliminate the the space in the output of the
  ## paste function() that will be used in merging the directory
  ## name, '/', and the name of the CSV file
  
  ## The 4th line of code in the for loop will read the file 
  ## that contains the values of the indicated monitors
  ## and store them in the data frame 'dataset'
  
  ## The second for loop will determine the number of complete
  ## cases in the specifies monitor. It will iterate based on the number of rows
  ## or the number of data on the specified monitor
  
  ## The if function will test per row if there exist a NA value in the data of sulfate
  ## and nitrate. If both of them has no NA values, it will the 1 value to the object count.
  ## And if either one or both of them has an NA value, the value of count will remain.
    
  ## After every iteration of the for loop, the values in the 
  ## numeric vector 'complete_cases' will change (The value that the function
  ## complete will return the last values of the data.frame complete_cases).
  ## The line of code after the if function will ensure that the previous values of 
  ## 'complete_cases' will ramain. The rbind() function will will add the new value of complete_cases
  ## to it's previous value
}
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)

## Calling the  function complete()
## that will return a data frame that consist of the number of complete cases
## of the specified monitor

corr <- function(directory, threshold=0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all variables)
  ## required to compute the correlation between
  ## nitrate and sulfate; the defualt is 0
  
  name<- c(list.files(directory))
  ## storing the name of the CSV files in the directory in a character vector 'name'
  
  complete_case<-data.frame(id=numeric(0), nobs=numeric(0))
  
  ## initializing 'complete_cases' as an empty data frame
  ## that will store the numeric values of the object 'id' and
  ## 'nobs'(number of complete cases in specified monitor) 
  
  
  complete_case<-complete("specdata", 1:332)
  
  ## calling the function complete() to calculate the number of complete cases
  ## in every monitor and then store the value that it will return on a data frame 'complete_case'
  
  nobs<-complete_case$nobs
  
  ## storing the value nobs(complete cases) in the data frame 'complete_cases'
  ## to the vector nobs
  cor<-0
  results<-{}
  
  ## initializing the value of 'cor' or correlation of nitrate and sulfate to zero
  ## and 'results' as an empty vector that will store
  ## the value correlations between nitrate and sulfate
  ## on the monitor whose complete cases meet the value of threshold
  
  for(i in 1:length(nobs)){
    if(nobs[i]>threshold){ 
      fname<-name[i] 
      loc<-gsub(" ", "", paste(directory, "/", fname))
      dataset<-read.csv(loc)
      useful_sul<-dataset[(!is.na(dataset$sulfate)), ]
      useful_data<-useful_sul[(!is.na(useful_sul$nitrate)), ]
      x<-useful_data$sulfate
      y<-useful_data$nitrate
      cor<-cor(x,y)
    }else{
      next()
      ## this will iterate if the number of complete
      ## is less than the threshold
      ## what will it do is will not check the correlation 
      ## of sulfate and nitrate in the specified monitor
    }
    results<-c(results, cor[(!is.na(cor(x,y)))])
  }
  results
  
  ## For the for loop
  ## The for loop will iterate based on the number of nobs or complete cases
  
  ## The if function will determine if the value of nobs or complete cases
  ## meets the value of threshold
  ## if the value of threshold was meet.
  
  ## the first line of code after the if function indicates 
  ## 'fname' as a character vector that will store 
  ## the name of CSV files in the directory based on the ID number
  ## of the of the monitor whose complete cases meet the threshold
  
  ## The 2nd line of code indicates
  ## 'loc' as a character vector that will store the name of the directory of
  ## of the indicated Monitor ID numbers, 
  ## the gsub() function will eliminate the the space in the output of the
  ## paste function() that will be used in merging the directory
  ## name, '/', and the name of the CSV file
  
  ## The 3rd line of code will read the file 
  ## that contains the values of the indicated monitors 
  ## whose directory will be based on 'loc'
  ## and store them in the data frame 'dataset'
  
  ## 'useful_sul' is a data frame that will store the values in 'dataset'
  ## that has no NA values in sulfate
  
  ## 'useful_data' is a data frame that  will store the valye in 'useful_sul'
  # that has no NA values in both nitrate and sulfate
  
  ## 'x' is a numeric vector that will store the the value of sulfate
  ## in the data frame useful_data
  
  ## 'y' is a numeric vector that will store the values of nitrate
  ## in the data frame useful_data
  
  ## the vector 'cor' will store the value of the correlation
  ## between x and y or the correlation of nitrate and sulfate
  ## in monitor whose complete cases meet the threshold
  
  ## After every iteration of the for loop, the values in the 
  ## numeric vector 'results' will change (The value that the function
  ## corr will return the last values of the vector 'results'.
  ## the last line of code in the for loop will ensure that the previous value of 'results'
  ## or correlation between nitrate and sulfate will remain. What the will do concatenate the 
  ## the value of the correlation between sulfate and nitrate in the monitor whose 
  ## complete cases meet the threshold
}


cr<-corr("specdata", 150)
## Calling the  function corr
## that will return a vector that consist of the values of the 
## correlations of sulfate and nitrate in monitors
## with complete cases and store them in the vector 'cr'
head(cr);summary(cr)

## head will return the first values of the vector cr
## summary is a generic function used to produce result 
## summaries of the values in the vector 'cr' 


cr<-corr("specdata", 400)
head(cr);summary(cr)

cr<-corr("specdata", 5000)
head(cr);summary(cr);length(cr)
## head() will return the first values of the vector cr
## summary() is a generic function used to produce result 
## summaries of the values in the vector 'cr' 
## length() wil determine if how many values was stored in the
## vector 'cr'

cr<-corr("specdata")
head(cr);summary(cr);length(cr)




##problem 4
setwd("C:\\Users\\Username\\Desktop\\Mini_Project")
##setting up the working directory

unzip("rprog_data_specdata.zip")
##unzipping the zip file downloaded from the LMS

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
## storing the values in the CSV file "outcome-of-care-measures.csv" in the data frame 'outcome'
## that has character classess

head(outcome)
## head() will return the first values of the data frame 'outcome'
ncol(outcome)
## ncol() will determine the number of columns in the data frame 'outcome
nrow(outcome)
## ncol() will determine the number of rows in the data frame 'outcome

outcome[, 11] <- as.numeric(outcome[, 11])
## coercing the values in column 11 of the data frame 'outcome'
## as a numeric value since we set it up first as character

hist(outcome[, 11])
## hist() will computes a histogram of the given the values in the column 11 of 'outcome'

## the hist() function below will computes a histogram of the give
## the values in the column 11 of 'outcome'
hist(outcome[, 11],
     col="sky blue",
     ## will fill the bars with the color sky blue
     main= "Hospital 30-Day Death(Mortality) Rates from Heart Attack", 
     ## will put the title of the histogram 
     ## ("Hospital 30-Day Death(Mortality) Rates from Heart Attack
     xlab= "Deaths", 
     ## will label the x axis as 'Death'
     ylab = "Frequency"
     ## will label the y axis as 'Death'
     )


