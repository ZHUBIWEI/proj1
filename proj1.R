#XXXXXXXXXXXXXXXXXXXXXXXX proj1.R XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Biwei Zhu, s2325784
# Guanhao Su, s2301705
# Shuying Liu, s2436365

#XXXXXXXXXXXXXXXXXXXXXXXX Contribution XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Shuying Liu completed the function split_punct and separate the punctuation marks, modified the A and S in question 7, completed question 10.
# Biwei Zhu completed the question 6
# Guanhao Su completed the question 7 (a),(b),(c),(d),(e)
# Biwei Zhu and Guanhao Su completed the question 8 and 9 together

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Please change the below to your local repo directory
setwd("D:/Edinburgh/Courses_study/Statistical programming/Project1/proj1")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

############################ Q 4-5 ##########################################

# Pre-process a, remove special punctuations
a <- gsub("[()*]","",a)

#separate required punctuations
split_punct <- function(old,punct){
  rep_i <- rep(1,length(old))
  punct_i <- grep(punct,old,fixed=TRUE)
  rep_i[punct_i] <- 2
  old <- gsub(punct,"",old,fixed=TRUE)
  new <- rep(old,rep_i)
  new[punct_i+1:length(punct_i)] <- punct
  new
}

for (punct in c(",",".",";","!",":","?")) a <- split_punct(a,punct)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Question 6 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# a <- split_punct(a)
# a>a, lowercase>lower_a
len_splited <- length(a)
lower_a <- tolower(a)

uniq_w <- unique(lower_a) # Find the unique element

uniq_match <- match(lower_a,uniq_w) # Match bible to the unique words

freq_uniq <- tabulate(uniq_match) # Count the frequeny of times of occuring words

# Choosing the top 500 the most common words
m <- 500

up_order <- order(freq_uniq) # Sort the freq_uniq in ascending order

most_500 <- up_order[(length(up_order)- m + 1):length(up_order)] # The index of the top 500 most common words

b <- uniq_w[most_500]

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 7  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Generating T  XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
match_T <- match(lowercase,b)
shift_T <- cbind(match_T[1:(length(match_T)-2)],match_T[2:(length(match_T)-1)],match_T[3:length(match_T)])
pairT_index <- shift_T[!is.na(rowSums(shift_T)),] # Find the location of NA

T <- array(0, dim = c(m,m,m)) # Define an m by m by m matrix

# When i,k,j is a common_pair(j joins after i and k), the time increases by one
for (i in 1:length(pairT_index)){
  T[shift_T[pairT_index[i],1], shift_T[pairT_index[i],2], shift_T[pairT_index[i],3]] <- T[shift_T[pairT_index[i],1], shift_T[pairT_index[i],2], shift_T[pairT_index[i],3]] + 1
}

# Let the sum of each row's probability equal to one
for (i in 1:m){
  for (k in 1:m){
    if (sum(T[i,k,])!=0){
      T[i,k,] <- T[i,k,]/sum(T[i,k,])
    }
  }
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Generating A  XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

match_A <- match(lowercase,b)

shift_A <- cbind(match_A[1:length(match_A)-1],match_A[2:length(match_A)])

pairA_index <- grep(FALSE,is.na(rowSums(shift_A))) # Find the location of NA

A <- matrix(0, nrow= m, ncol=m) # Define an m by m matrix

# if i,j is a common_pair, the time plus one
for (i in 1:length(pairA_index)){
  A[shift_A[pairA_index[i],1], shift_A[pairA_index[i],2]] <- A[shift_A[pairA_index[i],1], shift_A[pairA_index[i],2]] + 1
}

# Let the sum of each row's probability equal to one
for (i in 1:m){
  A[i,] <- A[i,]/sum(A[i,])
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Generating S  XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
match_S <- match(lowercase,b)

shift_S <- cbind(match_S)

pairS_index<- grep(FALSE,is.na(rowSums(shift_S)))

S<-array(0, dim = c(m)) # Define an m rows' matrix

for (i in 1:length(pairS_index)){
  S[shift_S[pairS_index[i]]] <- S[shift_S[pairS_index[i]]] + 1
}

# Let the sum of each row's probability equal to one
for (i in 1:m){
  S[i] <- S[i]/sum(S)
}
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 8  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

rand_b <- 1:length(b)
rand_index <- sample(rand_b,size = 2,replace = TRUE) # Two random words from b are used as the premises of the quadratic Markov chain

# Complete the composition of sentences in the Bible by Markov chain
sample_index <- rep(0,50)
sample_index[1] <- rand_index[1]
sample_index[2] <- rand_index[2]

for (i in 3:50)
{
  if (sum(T[sample_index[i-2],sample_index[i-1],])!=0){
    rand_index <- sample(1:length(b),size = 1,prob=T[sample_index[i-2],sample_index[i-1],])
  }
  else if(sum(A[sample_index[i-1],]) != 0){
    rand_index <- sample(1:length(b),size = 1,prob=A[sample_index[i-1],])
  }
  else{
    rand_index <- sample(1:length(b),size = 1,prob=S[sample_index[i]])
    
  }
  sample_index[i] <- rand_index
}

cat(b[sample_index]) # Print out biblical sentences formed by quadratic Markov chains

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 9  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

b_form_S <- 1:length(b)
rand_index_S <- sample(b_form_S,size = 1, prob = S)

# Create Biblical sentences only through S
# Since Markov chain is not used, it does not need to pick the first word,
# Just randomly pick the first word by probability
sample_S_index <- rep(0,50)
sample_S_index[1] <-  rand_index_S

for (i in 2:50)
{
  rand_index_S <- sample(1:length(b), size = 1, prob = S)
  sample_S_index[i] <- rand_index_S
}

cat(b[sample_S_index]) # Print out biblical sentences just by S

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX