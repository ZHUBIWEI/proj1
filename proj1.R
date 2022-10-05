#XXXXXXXXXXXXXXXXXXXXXX PATH XXXXXXXXXXXXXXXXXXXXXXXXX
setwd("C:/Statistical programming/coursework 1")
# setwd("put/your/local/repo/location/here")
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

a <- scan("10-0.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX THE SPLIT-PUNCT XXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


is.integer0 <- function(x)
{
  is.integer(x) & (length(x) == 0L)
}


split_punct <- function(x){
  #1.The function should search for each word containing the punctuation mark.
  #2.remove it from the word, and add it as a new entry in the vector of words #after the word it came from. 
  
  ls <- c(",", ".", ";", "!", ":", "?", "’")
  lenl <- length(ls)
  lenA <- 0 
  lenx <- length(x)
  
  for(i in 1:lenl) {
    #find the location of the words that contains punctuation.
    # if fixed=TRUE, return value will become the corresponding repo of pattern.
    #position ls[i] in x
    location_has_punct <- grep(ls[i],x,fixed=TRUE)
    
    #clear the punctuation from the words have that punctuation
    x[location_has_punct] <- gsub(ls[i],"",x[location_has_punct], fixed=TRUE) 
    
    #The length of spilted punctuation
    lenA <- lenA + length(location_has_punct)
    
    #The total length for each loop
    Total_len <- lenA + lenx
    
    #xs is a vector to store the single digits
    xs <- rep(0,Total_len) 
    
    #A is the location for a punctuation to insert in this loop
    A <- location_has_punct + 1:length(location_has_punct)
    xs[A] <- ls[i]
    
    #avoid the case that there is no punctuation in each loop
    if(is.integer0(location_has_punct)){
      xs <-x
    } else {
      xs[-A] <- x
    }
    x <- xs
  }
  return(x)
}


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX SPLIT-PUNCT END XXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXX M FREQUENT WORDS XXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

splited_text <- split_punct(a)

len_splited <- length(splited_text)

lower_splited_text <- tolower(splited_text)

# find the unique element of the splited text
unique_words <- unique(lower_splited_text,incomparables = NULL)

# match each word to the unique words
vector_match <- match(lower_splited_text,unique_words,incomparables = NULL)

#cat(length(vector_match) == length(splited_text))#whether there is any blank,可以删掉

# count the number of times of occuring words
count_unique_words <- tabulate(vector_match)


# choosing the top 500 words
m <- 500

Order <- order(count_unique_words)#rearrange count_unique_words in upgrading order

most_common_word <- Order[(length(Order)- m + 1):length(Order)]#出现最多的500个单词在unique_words的下标

b <- unique_words[most_common_word]


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXX M FREQUENT WORDS END  XXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 7  XXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#XXXXXXXXXXXXXXXX  How to generate T  XXXXXXXXXXXXXXXX
match_T <- match(lower_splited_text,b)

shifted_pair_T <- cbind(match_T[1:(length(match_T)-2)],match_T[2:(length(match_T)-1)],match_T[3:length(match_T)])

sum_shifted_pair_T <- rowSums(shifted_pair_T)

#find the location of NA.
location_NA_T <- is.na(sum_shifted_pair_T)

location_common_pair_T <- grep(FALSE,location_NA_T)

#define a M by M matrix with elements 0

T <- array(0, dim = c(m,m,m))

# if i,k,j is a common_pair, T[i,k,j] += 1

for (i in 1:length(location_common_pair_T)){
  T[shifted_pair_T[location_common_pair_T[i],1], shifted_pair_T[location_common_pair_T[i],2], shifted_pair_T[location_common_pair_T[i],3]] <- T[shifted_pair_T[location_common_pair_T[i],1], shifted_pair_T[location_common_pair_T[i],2], shifted_pair_T[location_common_pair_T[i],3]] + 1
}

#Let the sum of each row equal to one

for (i in 1:m){
  for (k in 1:m){
    if (sum(T[i,k,])!=0){
      T[i,k,] <- T[i,k,]/sum(T[i,k,])
    }
  }
}

#XXXXXXXXXXXXXXXX  How to generate A  XXXXXXXXXXXXXXXX

match_A <- match(lower_splited_text,b)

shifted_pair <- cbind(match_A[1:length(match_A)-1],match_A[2:length(match_A)])

sum_shifted_pair <- rowSums(shifted_pair)

#find the location of NA.
location_NA <- is.na(sum_shifted_pair)

location_common_pair<- grep(FALSE,location_NA)

#define a M by M matrix with elements 0

A<-matrix(0, nrow= m, ncol=m)

# if i,j is a common_pair, A[i,j] += 1

for (i in 1:length(location_common_pair)){
  A[shifted_pair[location_common_pair[i],1], shifted_pair[location_common_pair[i],2]] <- A[shifted_pair[location_common_pair[i],1], shifted_pair[location_common_pair[i],2]] + 1
}

#Let the sum of each row equal to one

for (i in 1:m){
  A[i,] <- A[i,]/sum(A[i,])
}

#XXXXXXXXXXXXXXXX  How to generate S  XXXXXXXXXXXXXXXX
match_S <- match(lower_splited_text,b)

shifted_pair_S <- cbind(match_S)

sum_shifted_pair_S <- rowSums(shifted_pair_S)

location_NA_S <- is.na(sum_shifted_pair_S)

location_common_pair_S<- grep(FALSE,location_NA_S)

S<-array(0, dim = c(m))

for (i in 1:length(location_common_pair_S)){
  S[shifted_pair_S[location_common_pair_S[i]]] <- S[shifted_pair_S[location_common_pair_S[i]]] + 1
}

for (i in 1:m){
  S[i] <- S[i]/sum(S)
}


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 7  END XXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 8  XXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


indexes_b_chosen <- 1:length(b)

#random selection from b

rand_initial <- sample(indexes_b_chosen,size = 2,replace = TRUE)

#generate a vector to store the printed index
random_sample_index <- rep(0,50)

random_sample_index[1] <-  rand_initial[1]
random_sample_index[2] <-  rand_initial[2]
t1=0
t2=0
t3=0

for (i in 3:50)
{
  if (sum(T[random_sample_index[i-2],random_sample_index[i-1],])!=0){
    rand_initial <- sample(1:length(b),size = 1,prob=T[random_sample_index[i-2],random_sample_index[i-1],])
    random_sample_index[i] <- rand_initial
    t1=t1+1
  }
  else if(sum(A[random_sample_index[i-1],]) != 0){
    rand_initial <- sample(1:length(b),size = 1,prob=A[random_sample_index[i-1],])
    random_sample_index[i] <- rand_initial
    t2=t2+1
  }
  else{
    rand_initial <- sample(1:length(b),size = 1,prob=S[random_sample_index[i]])
    random_sample_index[i] <- rand_initial
    t3=t3+1
  }
}
t1
t2
t3

cat(b[random_sample_index])

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 8  END XXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 9  XXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

indexes_b_chosen_S <- 1:length(b)

rand_initial_S <- sample(indexes_b_chosen_S,size = 1, prob = S)

#generate a vector to store the printed index
random_sample_index_S <- rep(0,50)

random_sample_index_S[1] <-  rand_initial_S

for (i in 2:50)
{
  rand_initial_S <- sample(1:length(b), size = 1, prob = S)
  random_sample_index_S[i] <- rand_initial_S
}


cat(b[random_sample_index_S])

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 9  END XXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 10  XXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Try to find the number of first capitalized common words in the splited text

capitalized_count <- rep(0,m)

require(stringr)
for(i in 1:m)
{
  capitalized_count[i] <- length(which(splited_text == str_to_title(b[i])))
}

# count_unique_words[most_common_word] counts the number of each common word

sum_common <- count_unique_words[most_common_word]

# runif(n, min = 0, max = 1)

#The probability of the capitalization of a common word

capitalized_prob <- rep(0,m)
for(i in 1:m)
{
  capitalized_prob[i] <- capitalized_count[i]/sum_common[i]
}

#print the capitalized word with probability capitalized_prob
print_b <- rep("",50)
for (i in 1:50)
{
  if (runif(1, min = 0, max = 1) < capitalized_prob[i])
  {
    print_b[i] <- str_to_title(b[random_sample_index[i]])
  }else
  {
    print_b[i] <- b[random_sample_index[i]]
  }
}

cat(print_b)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 10  END XXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
