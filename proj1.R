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
setwd("D:\\Edinburgh\\Statistical Programming\\proj1")

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
  new[punct_i+1:length(punct_i)] <- punct
  new
}

for (punct in c(",",".",";","!",":","?")) a <- split_punct(a,punct)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Question 6, 10 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate b containing most common words
len_splited <- length(a)
lower_a <- tolower(a)

uniq_w <- unique(lower_a) # Find the unique element

uniq_match <- match(lower_a,uniq_w) # Match bible to the unique words

freq_uniq <- tabulate(uniq_match) # Count the frequeny of times of occuring words

# Choosing the top 500 most common words and storing them in b
m <- 500

up_order <- order(freq_uniq) # Sort the freq_uniq in ascending order

most_500 <- up_order[(length(up_order)- m + 1):length(up_order)] # The index of the top 500 most common words

b <- uniq_w[most_500]

# Identify words that most often start with a capital letter, then modify b
b_new <- b
for (i in 1:m){
  a_word <- a[lower_a==b_new[i]]
  if (mean(a_word!=b[i])>0.5) substr(b_new[i],1,1) <- toupper(substr(b_new[i],1,1))
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 7  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXX  Generating T, A, S  XXXXXXXXXXXXXXXXXXXXXXXXXXX

match_T <- match(lower_a,b)
# c1, c2, c3 store the adjacent common words
c3 <- cbind(match_T[1:(length(match_T)-2)],match_T[2:(length(match_T)-1)],match_T[3:length(match_T)])
c2 <- c3[,c(1,2)]
c1 <- c3[,1]

# Remove the rows with NA
cw3 <- c3[!is.na(rowSums(c3)),]
cw2 <- c2[!is.na(rowSums(c2)),]
cw1 <- c1[!is.na(c1)]

# Initialize T, A, S to matrices of zeros
T <- array(0,c(m,m,m))
A <- array(0,c(m,m))
S <- array(0,m)

# Generate T, A, S
# When i,k,j is a common_pair(j joins after i and k), the time increases by one
for (rnum in 1:nrow(cw3)){
  T[cw3[rnum,1],cw3[rnum,2],cw3[rnum,3]] <- T[cw3[rnum,1],cw3[rnum,2],cw3[rnum,3]]+1
}
for (rnum in 1:nrow(cw2)){
  A[cw2[rnum,1],cw2[rnum,2]] <- A[cw2[rnum,1],cw2[rnum,2]]+1
}
for (i in 1:length(cw1)){
  S[cw1[i]] <- S[cw1[i]]+1
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 8  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate a 50-word text from T, A, S
# Randomly pick two words to start generating text
word_i <- sample(1:m,size=1,prob=S)
if (sum(A[word_i,])){
  word_k <- sample(1:m,size=1,prob=A[word_i,])
}else{
  word_k <- sample(1:m,size=1,prob=S)
}

# Complete the composition of sentences in the Bible by Markov chain
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
cat("Text generated from T, A and S:\n\n")
cat(b[sample_index],"\n\n(q10)\n")
cat(b_new[sample_index],"\n\n")

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXX  Question 9  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Generate a 50-word text from just S
rand_index_S <- sample(1:m,size=50,replace=TRUE,prob=S)

cat("Text generated from just S:\n\n")
cat(b[rand_index_S])

