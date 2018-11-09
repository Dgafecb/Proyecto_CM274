##############################################
## Codigo del Bubble sort
##############################################
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
##############################################
## Codigo del Insertion sort
##############################################
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
##############################################
## Codigo del Merge sort
##############################################
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
##############################################
## Codigo del quicksort
##############################################

partition = function(data, i, f){				# Se ordena el array respecto al ultimo elemento
    pivot = data[f-1]
    mid = i
    num = i
    
    while(num < f-1){
        if(data[num] < pivot){
            buffer = data[mid]
            data[mid] = data[num]
            data[num] = buffer
            mid = mid+1
        }
        num = num+1
    }
    
    data[f-1] = data[mid]
    data[mid] = pivot
    
    lista = list("d"=data,"n"=mid)				# Se retorna una lista con el array y el pivote
    return(lista)
}

quick = function(data, i, f){					# Funcion recursiva del quicksort
    if(f-i > 1){
        lista = partition(data, i, f)
        data = lista$d
        mid = lista$n
        
        data=quick(data, i, mid)
        data=quick(data, mid+1, f)
    }
    return(data)
}

quicksort = function(data){						# Funcion meramente estetica
    return(quick(data, 1, length(data)+1))
}

##############################################
## Codigo del bucketsort
##############################################

bucketsort = function(data){					# Se crea una lista de 10 canastas o buckets
    buckets = list(c(), c(), c(), c(), c(), c(), c(), c(), c(), c())
    i = 1
    while(i <= length(data)){					# Se añaden los elementos mediante hash
        index = ceiling(data[i]/10)
        buckets[[index]] = c(buckets[[index]], data[i])
        i = i+1
    }
    i = 1
    newData = c()
    while(i <= 10){								# Se llama a quicksort dentro de cada bucket
        adding = quicksort(buckets[[i]])
        newData = c(newData, adding)
        i = i+1
    }
    return(newData)								# Retorna la suma de todos los buckets
}

# 2da parte
# Test para el Bubble Sort
# 1)
# E(n) ~ ( tp(n) + to(n) ) / 2 .....(1)
# 
# donde:
#     
#     n		es el numero de elementos del array,
# tp(n)	es el peor tiempo (array en orden descendiente)
# to(n)	es el mejor tiempo (array en orden ascendente)
# 
# la aproximacion se vuelve igualdad cuando n y n-1 NO son
# multiplos de 4, en otro caso E es ligeramente menor.
# 
# La recomendacion seria usar 99 elementos en lugar de 100
# para una mejor aproximacion, midiendo tp y to varias veces
# para mayor exactitud y promediar ambos valores para tener
# un tp y to medios, luego reemplazar arriba.

#Para el peor caso generamos un array de 99 elementos ordenado de forma descendente
#Una vez generado el array de forma descendente
# Mediremos el tiempo de ordenamiento con el Bubble Sort para obtener el peor tiempo de ejecucion
test_times <- vector() #Este vector almacena el array de tiempos de ejecucion de cada test
for(i in 1:1000){
    test_1 <- sample(1:100,99)
    sort(test_1,decreasing = TRUE)
    t_inicio <- Sys.time()
    test_1 <- bubble_sort(test_1)
    t_final <- Sys.time()
    test_times[i] <- t_final-t_inicio
}
worst_time_bubble<- mean(test_times)
#Ahora generamos un array para el mejor caso, que seria un array ordenado
for(i in 1:1000){
    test_2 <- sample(1:100,99)
    sort(test_2)
    t_inicio <- Sys.time()
    test_2 <- bubble_sort(test_2)
    t_final <- Sys.time()
    test_times[i] <- t_final-t_inicio
}
best_time_bubble <- mean(test_times)
#Ahora con los valores promedio de cada uno de sus peores casos, procedemos a reemplazar la ecuacion (1)
#Para hallar la esperanza para n = 99
expected_time_bubble <- (best_time_bubble + worst_time_bubble)/2
sprintf("La esperanza para el Bubble sort para un array de 99 elementos es E(99) = %f", expected_time_bubble)

#Test para el Insertion Sort
# 2) Insertion sort
# 
# E(n) = ( tp(n) + to(n) ) / 2
# 
# la leyenda es igual al bubble sort, se resuelve de igual
# manera, aunque aqui si se puede usar 100 elementos.
for(i in 1:1000){
    test_3 <- sample(1:100,100)
    sort(test_3,decreasing = TRUE)
    t_inicio <- Sys.time()
    test_3 <- insertion_sort(test_3)
    t_final <- Sys.time()
    test_times[i] <- t_final-t_inicio
}
worst_time_insert <- mean(t)
#Ahora generamos un array para el mejor caso, que seria un array ordenado
for(i in 1:1000){
    test_4 <- sample(1:100,100)
    sort(test_4)
    t_inicio <- Sys.time()
    test_4 <- insertion_sort(test_4)
    t_final <- Sys.time()
    test_times[i] <- t_final-t_inicio
}
best_time_insert <- mean(test_times)
#Ahora con los valores promedio de cada uno de sus peores casos, procedemos a reemplazar la ecuacion (1)
#Para hallar la esperanza para n = 99
expected_time_insert <- (best_time_insert + worst_time_insert)/2
sprintf("La esperanza para el Insertion sort para un array de 100 elementos es E(100) = %f", expected_time_insert)

#Test para el Merge Sort
# 3) Merge sort
# 
# El tiempo de ejecucion no depende del ordenamiento de
# los numeros, es decir E(n) = k para cierto valor de n.
# En este caso, para hallar E simplemente se debe calcular
# el promedio de varios casos aleatorios del mismo tamaño.
for(i in 1:10000){
    test_5 <- sample(1:100,100)
    t_inicio <- Sys.time()
    test_5 <- merge_sort(test_5)
    t_final <- Sys.time()
    test_times[i] <- t_final - t_inicio
}
expected_time_merge <- mean(test_times)
sprintf("La esperanza para el Merge Sort para un array de 100 elementos es E(100) = %f", expected_time_merge)

# Test para el Quick Sort
#
# El algoritmo Quicksort usa un método llamado de “pivote” donde él ultimo termino generalmente se usa como pivote, 
# en este caso, se ha aplicado el algoritmo a un grupo de 100 datos numéricos entre el 1 y el 1000 que son generados
# aleatoriamente y luego son ordenados de manera ascendente mediante el algoritmo quicksort.
# Debido a que la eficiencia del algoritmo varia significativamente según la posición que ocupará el “pivote” al ser ordenado,
# se ha colocado la posición del “pivote” tras el ordenamiento y a su vez se ha colocado el tiempo del que tardó el algoritmo
# en realizar el ordenamiento.
# Se observará que el mejor tiempo se dará cuando el “pivote” sea la mediana de los términos ordenados. Y que el peor de los
# casos, será cuando este ocupe la primera o ultima posición de los términos ordenados
test_times1 <- vector()
for(i in 1:10000){
    test_6 <- sample(1:100,100)
    t_inicio <- Sys.time()
    quicksort(test_6)
    t_final <- Sys.time()
    test_times1[i] <- t_final - t_inicio
}
expected_time_quick <- mean(test_times1)
sprintf("La esperanza para el Quick Sort para un array de 100 elementos es E(100) = %f", expected_time_quick)

# Test para el Bucket Sort
#
# El algoritmo de Bucketsort es una “variante” del algoritmo de Quicksort ya explicado antes, lo que se hace es dividir los datos
# numéricos entre una potencia de 10, tal que todos tengan al menos un decimal, después de esto se tomará máximo entero a cada término,
# y los que tengan el mismo máximo entero se agruparán en un sub-array o también llamado “cubeta”(Bucket), finalmente se aplicará
# Quicksort dentro de cada cubeta y se procederá multiplicar por la misma potencia de 10 entre la cual se dividió. Cabe destacar que la
# esperanza de este método es n^2, y que la esperanza del quicksort para cantidades pequeñas de datos es casi lineal.
# Finalmente diremos que el Bucketsord adquiere su máxima efectividad cuando las cubetas está distribuidas de manera igual o muy parecida,
# o sea cuando cada cubeta tiene (casi) el mismo numero de elementos
test_times2 <- vector()
for(i in 1:10000){
    test_7 <- sample(1:100,100)
    t_inicio <- Sys.time()
    bucketsort(test_7)
    t_final <- Sys.time()
    test_times2[i] <- t_final - t_inicio
}
expected_time_bucket <- mean(test_times2)
sprintf("La esperanza para el Bucket Sort para un array de 100 elementos es E(100) = %f", expected_time_bucket)










