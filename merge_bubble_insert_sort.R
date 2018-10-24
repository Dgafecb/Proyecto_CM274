bubble_sort <- function(arr){
    n <- length(arr)
    for(i in 1:n){
        for(j in 1:(n-1)){
            if(arr[j] > arr[j+1]){
                temp <- arr[j]
                arr[j] <- arr[j+1]
                arr[j+1]<- temp
            }
        }   
    }
    return(arr)
} 
insertion_sort <- function(arr){
    n <- length(arr)
    for(i in 2:n){
        key <- arr[i]
        j <- i-1
        while(j>0 && key < arr[j]){
            arr[j+1] <- arr[j]
            j <- j-1
        } 
        arr[j+1] <- key
    } 
    return(arr)
} 
merge<-function(a,b) { #Une los 2 vectores a y b
    r<-numeric(length(a)+length(b))
    ai<-1; bi<-1; j<-1;
    for(j in 1:length(r)) {
        if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
            r[j] <- a[ai]
            ai <- ai+1
        } else { 
            r[j] <- b[bi]
            bi <- bi+1          
        }
    }
    return(r)
} 
merge_sort<-function(A) {
    if(length(A)>1) {
        q <- ceiling(length(A)/2) # length(A)/2 si length(A) par o length(A)+1 /2 para length(A) impar
        a <- merge_sort(A[1:q])
        b <- merge_sort(A[(q+1):length(A)])
        merge(a,b)
    } else { 
        return(A)
    } 
}

test_arr1 <- sample(1:1000,10)#Genero un array de 10 elementos con valores aleatorios entre 1 y 1000
print(test_arr1)
result_bubble <- bubble_sort(test_arr1)
print(result_bubble)

test_arr2 <- sample(1:1000,10)#Genero un array de 10 elementos con valores aleatorios entre 1 y 1000
print(test_arr2)
result_insertion <- insertion_sort(test_arr2)
print(result_insertion)

test_arr3 <- sample(1:1000,10)#Genero un array de 10 elementos con valores aleatorios entre 1 y 1000
print(test_arr3)
result_merge <- merge_sort(test_arr3)
print(result_merge)
