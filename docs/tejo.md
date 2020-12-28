---
title: "El juego del tejo, errores y aciertos"
author: "Gabriel González Medina"
date: "26-12-2020"
output: html_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# El tejo o la Rayuela

## INTRODUCCIÓN

En este escrito doy una primera aproximación al uso de R y conceptos estadísticos para realizar modelos predictos, a traves del **juego del tejo**

![](Muchachos 01.jpg)

En mi país, Chile, es un juego común (supuestamente) en el ambiente rural, y sobre todo en las fiestas nacionales, los "18 de septiembre". De hecho, tengo el recuerdo que con mi familia cuando niño, en Cabildo, haciendo varios juegos tradicionales, y entre estos tenían que preparar el terreno de juego para el tejo. aunque acá en Chile, tambien le dicen **"La Rayuela"**, pero en otros paises lationaméricanos, le dicen la Rayuela a lo que nosotros le decimos **"El Luche"**. Bueno, todo un enredo, que no vamos a profundizar.

![](Marco 04.jpg)

De acuerdo, a lo que encontré en [internet](http://www.revistachilena.com/La_Rayuela.html), el cajon de juego es de 120 cm largo y 120 ancho y la linea donde debe caer el tejo para marcar una "quemada" y obtener 2 puntos está a 60 cm horizontalmente en el cajon, de lado a lado. 

Basado en los elementos anterior me prepongo en esta ocasión a explicar cómo: 

+ Crear una base de datos basados en un situación concreto como los puntos en un juego de reglas sencillas
+ Gráficar los datos y ajustar los parametros visuales
+ Modificar esa base de datos para crear un grafico que permita representar todas las variables.
+ Comparar distintas maneras de resumir los aciertos y errores, de acuerdo a las reglas del juego


## Desarrollo 

### Registro de los datos


Vamos a empezar con algo bastante simple, los registros que podríamos haber sido anotados manualmente en una hoja de papel.

En este escenario tenemos dos jugadores que tiraron tres veces cada uno,
el arbitro luego de cada lanzamiento media con una regla la disitancia en que caía el tejo, contando desde el comienzo del cajon.

Entonces, en su libreta de notas él indicó que:

jugador 1 =  60, 80, 40

jugador 2 =  70, 65, 55

Escribirlo en el lenguaje de R, sería de la siguiente forma: 

```{r}
j1 <- c(60, 80, 40)
j2 <- c(70, 65, 55)
```

Lo transformo ahora en un **data frame**, para que nos quedé listo para trabajar más adelante. Digamos que en una forma tabulada, como en una planilla excel, o un viejo 
cuaderno de matematicas con lineas verticales y horizontales. 

```{r}
puntos <- data.frame(rbind(j1,j2))
```

Le ponemos nombre a las variables, para recordar qué es qué

```{r}
colnames(puntos) <- c("intento1", "intento2", "intento3")
```

Ahora creo una variable para distinguir a los jugadores, y activo previamente el paquete (dplyr) necesario para que funcione la siguiente linea de código

```{r message=FALSE}
library(dplyr) 
puntos <- puntos %>% 
  mutate(jugador = rownames(puntos))
print(puntos)
```

### Gráficos

Graficamos los resultados del primer intento
           
```{r message=FALSE}

library(ggplot2) 
ggplot(puntos, aes(jugador, intento1, color = jugador)) +
  geom_point(size = 8) + 
  geom_hline(yintercept =  60, color = "green", linetype = "dotted", 
             size = 3, alpha = 0.5) +
  scale_y_continuous(limits = c(0,120),  breaks = c(0,30,60, 90, 120))+
  theme_classic() +
  theme( axis.text.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x = element_blank()
  ) 
  
```
 Algunas explicaciones del codigo anterior: 
 
 *geom_hline()* es para crear  la linea horizontal con la que se gana si el tejo cae justo encima.
 *scale_y_continuous()* 
 *theme_classic()*
 *theme()* son para limpiar el gráfico y solo queden elementos principales y se parezca lo máximo posible a un cajón. 


Continuo. Tengo que ajustar la base de datos si queremos poder graficar todos los intentos en un gráfico

```{r message=FALSE}
library(tidyr)
puntos <- puntos %>% 
  pivot_longer(cols = -jugador, names_to = "intento", names_prefix = "intento",
               values_to = "distancia")
```

Graficar todo el juego completo

```{r}
ggplot(puntos, aes(jugador, distancia, shape = intento, color = jugador)) +
  geom_point(size = 8, alpha = 0.7) +
  geom_hline(yintercept =  60, color = "green", linetype = "dotted", 
             size = 3, alpha = 0.5) +
  scale_y_continuous(limits = c(0,120), breaks = c(0,30,60, 90, 120))+
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
```


### Definiendo los aciertos y errores

Calculamos la **diferencia** entre donde cayó el tejo y la linea objetivo

```{r}
puntos <- puntos %>% 
  mutate(diferencia = distancia - 60) 
print(puntos)
```

Graficamos ahora con esta nueva variable, usandola como una etiqueta.

```{r}
ggplot(puntos, aes(jugador, distancia, shape = intento, color = jugador)) +
  geom_point(size = 8, alpha = 0.7) +
  geom_text(aes(label = diferencia), position = position_nudge(x=0.1))+
  #anotamos con esta nueva linea la diferencia en el grafico
  geom_hline(yintercept =  60, color = "green", linetype = "dotted", 
             size = 3, alpha = 0.5) +
  scale_y_continuous(limits = c(0,120), breaks = c(0,30,60, 90, 120))+
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
```

calculemos el desempeño **promedio** de cada jugador

```{r}
puntos <- puntos %>% 
  group_by(jugador) %>%  #primero agrupar datos por jugador
  mutate(promedio_jugador = mean(distancia))
print(puntos)
```

Si vemos los resultados, en promedio el jugador 1 cayó en lugar de la linea, es decir es el ganador. Pero otra manera de resumir el desempeño general es promediando cuanto se equivocó

```{r}
puntos <- puntos %>% 
  group_by(jugador) %>%  #primero agrupar datos por jugador
  mutate(promedio_diferencia = mean(diferencia)) %>% 
  mutate(promedio_diferencia_absoluta = sqrt(mean(diferencia^2))) 
#en la última linea, se elevó la diferencia al cruadrado para dejarlo en una medida #absoluta del error de cada jugador con ^2 y luego se volvió a la medida original
#usando sqrt(), esto es,  la raiz cuadrada
print(puntos)
```

Lo interesante de este resultado, es que si bien en promedio de la distancia donde cayó su tejo, lo hizo mejor el jugador 1, si revisamos las diferencias absolutas, el jugador 2 lo hizo mejor, a la larga se equivocó menos, pesé a que nunca le atinó exactamente. , mientras que el uno, en el promedio simple lo hizo mejor ya que lo que se equivocó pasandose en su primer intento, se es escondé al promediarlo con lo mucho que le faltó en su tercer lanzamiento

## Los Aciertos

Luego, podemos tambien crear una nueva variable **dictomica** que define si el intento acertó o no.

```{r}
puntos <- puntos %>% 
mutate(resultado = ifelse(diferencia  == 0, "Acierto", "Fallo"))
#entonces, si la diferencia es 0, significá que acertó. Cualquier otro numero será
#un fallo. 
#Y entonces hacemos un tabla para resumir los resultados
table(puntos$resultado, puntos$jugador)
```

## REFLEXIONES FINALES

* ¿Que es mejor para evaluar los rendimientos entonces? 
+ ¿Los promedios?
- ¿Los promedios de los errores?
- ¿Los promedios de los errores absolutos?
+ ¿Aciertos vs fallos?

La respuesta para nada sorprendente a todas estas preguntas es **"depende"**
una palabra muy usada tanto por la estadística como por la psicología, porque básicamente es muy dificil encontrar algo que no se influenciado o causado por otra cosa. El contexto es sumamente importante para poder tomer una decisión. 
Seguiré con esto en proximo entregas, para que podamos entender cuando es mejor usar cada cosa.