#####################
#Leemis' Statistic

#Create some toy data to test the loop
a<-as.matrix(c(rep(9,10000)))#a vector that will send back significant values
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
    element<-(xi[i]-log((1+1/i), base=10))#The calculation for Leemis' M
    argument<-c(argument,element) #Constructioin of a vector which holds all the calculations
  }
  argmax<-max(argument) #This is max of the max of the above calculation
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
  for(i in 1:9){ #This loop calculates the  function for chogains d
    element<-((xi[i]-log((1+1/i), base=10))^2)
    chosum<-c(chosum,element)
    finvec<-sum(chosum)
    d<-sqrt(finvec)
  }
  return(d)
}
cho(a)


########################
#Now I shall use the functions I created to create an output which spits
#both of the statistics or just one of them depending on the user input


final_func<-function(a,m,d){
  #we feed in these values, a being the values of interest, m being leemis() and d being cho()
  #the parameters will be such that unless otherwise specified both functions will be the output
  #if you, however, input false for the m or the d you will receive an NULL and the requested value
  if(m==T){
    m<-leemis(a)
  } else{
    m<-NULL
  }
  if(d==T){
    d<-cho(a)
  } else{
    d<-NULL
  }
  dig_distr<-table(as.numeric(substr(a, start=1, stop=1))) #This extracts the first number of whatever values I input into b
  
  answer<-list(d,m,dig_distr) # This is the list of the calculation of the Leemis Cho and the Digit Distribution
  names(answer)<-c("Leemis_M", 'Cho-Gains_D', "Digit Distribution")
  return(answer)
}

final_func(a,T, T)#Examples of how user input creates the requested output
final_func(a,F, T)
final_func(a,T, F)


#yay!

###############################
#2:Critical Values

print.benfords<-function(a){#creating the requested function which includes significance stars
  fin<-as.data.frame(final_func(a,T,T)) #create a dataframe so that the numbers dont become characters when add the star
  k<-final_func(a,T,T)#a call variable to easily type the specifications
  if (0.851 < k[1] & k[1] < 0.967){#this and the next two lines say set parameters on first column  between the requested values
    fin$Leemis_M[1]<-paste(k[1],"*" )}
  if (0.967 <= k[1] & k[1] < 1.212){
    fin$Leemis_M[1]<-paste(k[1],"**" )}
  if (1.212<= k[1] ){
    fin$Leemis_M[1]<-paste(k[1],"***" )}
  if (1.212 < k[2] & k[2] <1.330){#setting parameters for the second column
    fin$Cho_gains[1]<-paste(k[2],"*" )}
  if (1.330 <= k[2] & k[2] < 1.569){
    fin$Cho_gains[1]<-paste(k[2],"**" )}
  if (1.569 <= k[2]){
    fin$Cho_gains[1]<-paste(k[2],"***" )}
  print(fin)
  cat("Note: P-value <0.01 ***, <0.05 **, <0.1 *")
}
print.benfords(a)# Look at that fresh output

save<-function(a){#save that stuff
  sink(file="dl_bed.csv")
  print.benfords(a)
  sink()
}
save(a)

