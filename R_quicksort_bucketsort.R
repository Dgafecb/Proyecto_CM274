
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

array1 = sample(1:100, 20)
print(array1)
quicksort(array1)

array2 = sample(1:100, 20)
print(array2)
bucketsort(array2)
