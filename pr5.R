#Ej.1
numArtefactos<- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
  #¿Cómo almacena los valores numéricos: integer o double?double
  #Transforma el tipo de dato a número entero llamando al objeto ‘numArtefactos_int’. 
numArtefactos_int <- as.integer(numArtefactos)
#Ej.2. Calcula la media del objeto ‘numArtefactos_int’. 
numArtefactos_int=as.integer(numArtefactos)
 mean(numArtefactos_int)
#Ej. 3.Calcula la mediana del objeto ‘numArtefactos_int’. Define brevemente la mediana: concepto y cálculo.
median(numArtefactos_int)
#Ej.4 Calcula la moda del objeto ‘numArtefactos_int’. 
unique(numArtefactos_int)
tabulate(numArtefactos_int,nbins = 102)
match(10,numArtefactos_int)
Mode<-function(x)
  {u<-unique(x)
  tab<-tabulate(match(x, u))
  u[tab==max(tab)]
}
#opcon 2
moda2<-function(x){return(x)
tab<-tabulate(match(x, u))
u[tab==max(tab)]
}
#Ej.5 Calcula el número de veces que se repite el valor correspondiente con la moda.
frecuencias<-table(numArtefactos_int)
frecuencias.ordenadas<-frecuencias[order(frecuencias,decreasing=TRUE)]
frecuencias.ordenadas
#Ej.6 Calcula los cuartiles del objeto ‘numArtefactos_int’.
    # Un cuartil es una medida de distribucion
quantile(numArtefactos_int)

#Ej.7Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. Interpreta elresultado
iqr_numArtefactos_int <- IQR(numArtefactos_int)

#Ej.8 Calcula el rango del objeto ‘numArtefactos_int’. Almacena el rango en un vector denominado ‘rango_artefactos’.
rango_artefactos <- range(numArtefactos_int)
rango_artefactos<-diff(range(numArtefactos_int))
rango_artefactos

#Ej.9. Calcula la varianza del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo.
var(numArtefactos_int) #Resultado=[1] 927.1026

#Ej.10Calcula la desviación estándar del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo.
desviacion_estandar <- sd(numArtefactos_int)
desviacion_estandar #[1] 30.44836
#otra opcion
desv_estandar <- sd(numArtefactos_int) #[1] 30.44836

#Ej.11.¿En qué se diferencia la desviación estándar de la varianza?
#ambos conceptos muestran si los valores se encuentran más o menos cercanos a las medidas de posición. Los he he encontrado de la desviación estándar, es que es la raíz cuadrada positiva de la varianza; y la la varianza es el promedio de las distancias al cuadrado que van desde las observaciones a la media.

#Ej.12.Visualiza gráficamente de manera horizontal la dispersión del objeto‘numArtefactos_int’.
library(ggplot2)
ggplot(data.frame(numArtefactos_int), aes(x=numArtefactos_int, y=0)) + 
  geom_point(size=3, shape=21, fill="darkcyan", color="black") +
  ylab("")+theme_minimal()  #+ theme_minimal(). sirve para ponerla como mas "minimalista" por asi decirlo. Si no, se ve con los cuadados grises grandes. 
   #Este gráfico muestra cada valor de 'numArtefactos_int' como un punto cyan en la línea horizontal. Podemos ver con el la distribución de los datos y la dispersión alrededor de la línea horizontal.
    #geom_point() es una función en el paquete ggplot2 de R que permite agregar puntos a un gráfico. Los argumentos size, shape, fill y color son parámetros estéticos que controlan el tamaño, la forma, el color de relleno y el color del borde de los puntos, respectivamente. 
    #size: controla el tamaño de los puntos. En este caso, se establece en 3 unidades.
    #shape: controla la forma de los puntos. El valor 21 se refiere a un círculo con borde sólido y relleno.
    #fill: controla el color de relleno de los puntos. En este caso, se establece en "darkcyan", que es un tono oscuro de cian.
    #En resumen, estas opciones permiten personalizar la apariencia visual de los puntos en el gráfico de dispersión.

#Ej.13.Crea un vector llamado ‘vector3’ a partir de la siguiente secuencia de valores ’21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1’
  #Esta función no hace falta ni que la explique, ya que la llevamos haciento tela de tiempo jaja. pero por si acaso: el vector3  contiene la secuencia de valores que le hemos dicho. Se puede usar este vector para realizar cálculos o para gráficar los datos.
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

#Ej.14.Calcula el coeficiente de variación de los objetos: 1)‘numArtefactos_int’ y 2) ‘vector3’. Emplea 2 funciones para su cálculo. Compara e interpreta los resultados.
       

 #El coeficiente de variación es una medida de dispersión relativa que se utiliza para comparar la variabilidad de dos conjuntos de datos con diferentes unidades de medida. Se calcula como la desviación estándar dividida por la media y se expresa como un porcentaje. Aquí Galo, te muestro cómo calcular el coeficiente de variación de los objetos 'numArtefactos_int' y 'vector3'(jajaja):
                # Para numArtefactos_int
                cv_numArtefactos <- 100 * sd(numArtefactos_int) / mean(numArtefactos_int)
                cv_numArtefactos
                # Resultado: 66.84602
                
                # Para vector3
                cv_vector3 <- 100 * sd(vector3) / mean(vector3)
                cv_vector3
                # Resultado: 63.59067
          #Esto significa que el conjunto de datos 'numArtefactos_int' tiene una variabilidad relativa más alta que 'vector3'.

                
#Ej.15.Genera una tabla-resumen de los estadísticos descriptivos expuestos: media,mediana, desviación estándar etc.
                
    #Para generar una tabla-resumen de los estadísticos descriptivos de los objetos 'numArtefactos_int' y 'vector3', podemos utilizar la función summary(). Esta función proporciona un resumen estadístico para un conjunto de datos, incluyendo la media, la mediana, el rango, la desviación estándar y los cuartiles.
                # Para numArtefactos_int
                summary(numArtefactos_int)
                # Resultado:
                #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                #   4.00   21.50   40.50   45.55   61.50  102.00 
                
                # Para vector3
                summary(vector3)
                # Resultado:
                #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                #   1.00  26.25  41.50  46.05  68.50  98.000
               
#16.Calcula el coeficiente de asimetría del objeto ‘vector3’. Interpreta su resultado. Exponga ejemplos de distribuciones de variables con asimetría positiva y negativa y simétricas. Explique cada uno de estos escenarios.
 #Podemos calcular el coeficiente de asimetría del objeto 'vector3' utilizando la función skewness() :
                  
                library(e1071)
                skewness(vector3)
                # Resultado: [1] 0.3138528
                # El coeficiente de asimetría para 'vector3' es de 0.3138528. Un coeficiente de asimetría negativo indica que la distribución es ligeramente asimétrica hacia la izquierda, lo que significa que la cola de la distribución se extiende hacia la izquierda. En otras palabras, la mayoría de los valores se concentran en la parte derecha de la distribución y hay algunos valores bajos (a la izquierda) que se alejan de la mayoría de los valores. En este caso, la distribución parece ser aproximadamente simétrica pero ligeramente sesgada hacia la izquierda.        


#17.Calcula la curtosis del objeto ‘vector3’. ¿Qué tipo de curtosis se encuentra asociada al anterior objeto? Justifica tu respuesta. 















