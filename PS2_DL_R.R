#####################
#Leemis' Statistic

#Create some toy data to test the loop
a<-c(1:100)
b<-NULL
xi<-NULL

###Leemis
leemis<-function(a){
  #argument a will be a vector of total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This extracts the first number of whatever values are fed into b.
  proportions<-NULL
  for (i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){ #This loop calculates the inside of the max function for Leemis m
    element<-(xi[i]-log((1+1/i), base=10))
    argument<-c(argument,element)
  }
  argmax<-max(argument) #This is m
  return(argmax)
}
leemis(a)


############################
#Cho-Gains' D


chosum<-NULL
cho<-function(c){
  #We are creating a vector to hold total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This extracts the first number of whatever values I input into b
  proportions<-NULL
  for (i in 1:9){ #This  creates a proportion of occurrences for each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){ #This loop calculates the inside of the max function for chogains d
    element<-((xi[i]-log((1+1/i), base=10))^2)
    chosum<-c(chosum,element)
    finvec<-sum(chosum)
    d<-sqrt(finvec)
  }
  return(d)
}

  return(chogains)
}
cho(a)


########################
#Now I shall use the functions I created to create an output which spits
# both of the statistics or just one of them depending on the user input


final_func<-function(a,m,d){
  #we feed in these values, a being the values, m being leemis() and d being cho()
  #the parameters will be such that unless otherwise specified both functions will be the output
  #if you, however, input false for the m or the d you will receive an NA and the requested value
if(m==T){
   m<-leemis(a)
  } else{
    l<-NULL
    }
if(d==T){
   d<-cho(a)
  } else{
    d<-NULL
  }
  dig_distr<-table(as.numeric(substr(a, start=1, stop=1))) #This extracts the first number of whatever values I input into b

  answer<-list(d,m,dig_distr) # this is not right HOWTF do i get Digitdistr
  names(answer)<-c("Leemis' M", "Cho-Gains' D", "Digit Distribution")
  return(answer)
  }

final_func(a,T, T)

#yay!

###############################
#2:Critical Values


