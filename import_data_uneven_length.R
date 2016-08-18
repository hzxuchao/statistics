#################################################################
#
# import non-rectangular data into R
#
# 8/18/2016 by Chao Xu
#
################################################################


#read in data as one long vector
V = scan("testdata2.txt", sep=",")

#creat a list to stoge records with different lengths
X = list()

#the number of records that have absolute values larger than 3
N = sum(abs(V)>3)
M = 0

for(i in 1:N){
   #find the next value larger than 3
   w = min(which(abs(V)>3))

   #extract subject n data and save it into X
   X[[i]] = V[1:w]

   #throw out the data just recorded
   V = V[-c(1:w)]
    
   #see if this is the longest record yet 
   if(w > M){
      M = w 
   }
}

#record data into a matrix with NAs
D = matrix(NA, N, M)
for(i in 1:N) D[i, 1:length(X[[i]])] = X[[i]]

