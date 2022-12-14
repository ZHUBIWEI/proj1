#XXXXXXXXXXXXXXXXXXXXXXXX proj1.r XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# This project aims to generate text that matches word patterns seen in the Bible, using a 2nd order Markov model.

# Biwei Zhu, s2325784
# Guanhao Su, s2301705
# Shuying Liu, s2436365

#XXXXXXXXXXXXXXXXXXXXXXXX Contribution XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Shuying Liu completed the function split_punct and separate the punctuation marks, modified the A and S in question 7, completed question 10
# Biwei Zhu found the 500 most commonly occurring words in Bible(question 6)
# Guanhao Su completed the T,A and S which are used to generate Bible sentences(question 7)
# Biwei Zhu and Guanhao Su generated the Bible sentences by using T,A and S together(question 8 and 9)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Please change the below to your local repo directory
setwd("C:/Statistical programming/coursework 1/proj1")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#XXXXXXXXXXXXXXXXXXXXXXXXXXXX Question 4-5 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Pre-process a, remove special punctuations
a <- gsub("[()*]","",a)

# Separate required punctuations
split_punct <- function(old,punct){
  rep_i <- rep(1,length(old))
  punct_i <- grep(punct,old,fixed=TRUE)
  rep_i[punct_i] <- 2
  old <- gsub(punct,"",old,fixed=TRUE)
  new <- rep(old,rep_i)
  #insert punctuations back
  new[punct_i+1:length(punct_i)] <- punct
  new
}

for (punct in c(",",".",";","!",":","?")) a <- split_punct(a,punct)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Question 6, 10 XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate b containing most common words
len_splited <- length(a)
lower_a <- tolower(a)

uniq_w <- unique(lower_a) # Find the unique element

uniq_match <- match(lower_a,uniq_w) # Match bible to the unique words

freq_uniq <- tabulate(uniq_match) # Count the frequency of each occuring words

# Choosing the top 500 most common words and storing them in b
m <- 500

up_order <- order(freq_uniq) # Sort the freq_uniq in ascending order

most_500 <- up_order[(length(up_order)- m + 1):length(up_order)] # The index of the top 500 most common words

b <- uniq_w[most_500]

# Identify words that most often start with a capital letter, then modify b(question 10)
b_new <- b
for (i in 1:m){
  a_word <- a[lower_a==b_new[i]]
  if (mean(a_word!=b[i])>0.5) substr(b_new[i],1,1) <- toupper(substr(b_new[i],1,1))
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 7  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXX  Generating T, A, S  XXXXXXXXXXXXXXXXXXXXXXXXXXX

# c1, c2, c3 store the adjacent common words
c1 <- match(lower_a,b)
c2 <- cbind(c1[1:n-1],c1[2:n])
c3 <- cbind(c1[1:(n-2)],c1[2:(n-1)],c1[3:n])

# Remove the rows with NA in c1,c2 and c3
cw1 <- c1[!is.na(c1)]
cw2 <- c2[!is.na(rowSums(c2)),]
cw3 <- c3[!is.na(rowSums(c3)),]

# Initialize T, A to matrices of zeros
T <- array(0,c(m,m,m))
A <- array(0,c(m,m))

# Generate T, A, S
# When i,k,j is a common_pair(j joins after i and k), the time increases by one
for (rnum in 1:nrow(cw3)){
  T[cw3[rnum,1],cw3[rnum,2],cw3[rnum,3]] <- T[cw3[rnum,1],cw3[rnum,2],cw3[rnum,3]]+1
}
for (rnum in 1:nrow(cw2)){
  A[cw2[rnum,1],cw2[rnum,2]] <- A[cw2[rnum,1],cw2[rnum,2]]+1
}
S <- tabulate(cw1)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 8  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate a 50-word text from T, A, S
# Randomly pick two words to start generating text
word_i <- sample(1:m,size=1,prob=S)#prob means we select a word according to the value in S 
if (sum(A[word_i,])){
  word_k <- sample(1:m,size=1,prob=A[word_i,])
}else{
  word_k <- sample(1:m,size=1,prob=S)
}

# Complete the rest of sentences in the Bible by Markov chain
sample_index <- array(0,50)
sample_index[1:2] <- c(word_i,word_k)

for (i in 3:50)
{
  if (sum(T[sample_index[i-2],sample_index[i-1],])){
    rand_index <- sample(1:m,size = 1,prob=T[sample_index[i-2],sample_index[i-1],])
  }
  else if(sum(A[sample_index[i-1],])){
    rand_index <- sample(1:m,size = 1,prob=A[sample_index[i-1],])
  }
  else{
    rand_index <- sample(1:m,size = 1,prob=S)
  }
  sample_index[i] <- rand_index
}

# Print out biblical sentences formed by quadratic Markov chains
cat("Text generated from T, A and S:\n")
cat(b[sample_index],"\n\n")
#Text generated with some capital words ,easy to compare with sentences in lower case.(question 10)
cat(b_new[sample_index],"\n\n")

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 9  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate a 50-word text from just S
rand_index_S <- sample(1:m,size=50,replace=TRUE,prob=S)

cat("Text generated from just S:\n")
cat(b[rand_index_S])

