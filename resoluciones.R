# Teoría de la Decisión || Trabajo 01 Individual || Parte 2, Resolución --------



## Problema I ------------------------------------------------------------------


### Enunciado I ----------------------------------------------------------------

# Teniendo la matriz:       e1  e2  e3  e4  e5 
#                      d1    9  11   6   3  12
#                      d2    4   2   8   7  13
#                      d3   10   1   4   8   7
# 
# Resuelve el problema con cada uno de los métodos o funciones individuales de
# Incertidumbre por separado (tanto en situación favorable como desfavorable).


### Solución I -----------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

tabla_1 <- crea.tablaX(c( 9, 11, 6, 3, 12,
                          4,  2, 8, 7, 13, 
                         10,  1, 4, 8,  7),
                       numalternativas = 3,
                       numestados = 5)
tabla_1

## Hacemos la función que escribe la solución dependiendo del método, el criterio,
## el número de alternativas óptimas y, obviamente, la alternativa óptima.

solucion <- function (x) {
  
  # Creamos las variables que se irán necesitando.
  crit_sol = x$criterio
  met_sol = x$metodo
  opt_sol = x$AlternativaOptima
  n_sol = length(opt_sol)
  
  # Hacemos la primera parte de la frase de la solución (método y criterio).
  cat("En situación", met_sol, "y mediante el criterio de", crit_sol, 
      if (n_sol == 1) {
        "la solución es "
      } else {
        "las soluciones son "
      })
  
  # Hacemos la parte final de la frase de la solución (alternativas óptimas).
  if(n_sol == 1){
    cat(opt_sol, ".")
  }
  else {
    for(i in 1:(n_sol-1)) {
      cat(opt_sol[i], "y ")
    }
    cat(opt_sol[n_sol], ".")
  }
  
}

#### Favorables ----------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_1))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_1))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_1))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_1))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_1))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_1))

##### Wald -------------------------------------------------------------------

solucion(criterio.Wald(tabla_1))


#### Desfavorables -------------------------------------------------------------


##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_1, favorable = F))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_1, favorable = F))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_1, favorable = F))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_1, favorable = F))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_1, favorable = F))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_1, favorable = F))

##### Wald -------------------------------------------------------------------

solucion(criterio.Wald(tabla_1, favorable = F))



## Problema II -----------------------------------------------------------------


### Enunciado II ---------------------------------------------------------------

# Supongamos que un inversor deportivo debe decidir entre tres posibles
# estrategias a la hora de destinar su dinero en la temporada 24/25 de Dallas
# Mavericks, el equipo de la liga estadounidense de baloncesto, la NBA.
#
# Estas estrategias de inversión son marketing (estrategia A), infraestructura (B)
# o apostar por jugadores jóvenes (C).
#
# Pueden generar diferentes beneficios dependiendo del rendimiento del equipo
# texano. Las cuatro posibles situaciones y los beneficios según los escenarios
# son las siguientes:
#
# - Si el equipo no clasifica a play-off: la estrategia A da 25 millones de
# dólares; la B da 15 y la C, 20.
#
# - Si Dallas clasifica a play-off pero no a las finales, con las estrategias se 
# obtiene un beneficio de 35, 45 y 40 millones ($) respectivamente.
# 
# - Si pierden la final los beneficios serán de 50, 55 y 45 (en millones de $).
#
# - Por último, si Dallas Mavericks resulta ganador del anillo, los beneficios se
# irían a 70 millones con la estrategia A, 65 millones con la B y 80 con la C.
#
#
# Resuelve este problema mediante todos los criterios conocidos y explica las
# conclusiones obtenidas.


### Solución II ----------------------------------------------------------------

# Primero, creamos la matriz del problema.

tabla_2 <- crea.tablaX(c( 25, 35, 50, 70,
                          15, 45, 55, 65, 
                          20, 40, 45, 80),
                       numalternativas = 3,
                       numestados = 4)
tabla_2

# Resolvemos mediante todos los métodos a la vez.

criterio.Todos(tabla_2, alfa = 0.5)

# Vemos que las alternativas óptimas son la 1 ó la 3.
#
# Para Wald es la estrategia A la que hay que tomar, ya que en el caso más
# pesimista (caer eliminados antes de play-off), es la que más beneficio reporta.
#
# Para los criterios Optimista, Hurwicz, Laplace y Punto Ideal es la estrategia 
# C la que se debe adoptar, siendo esta la más beneficiosa en el caso de quedar 
# campeones, la peor en caso de perder la final y la más equilibrada en los otros
# dos escenarios.
#
# Para Savage, hay un empate entre las estrategias A y C.
#
# La segunda estrategia, la de la inversión en infraestructura, queda descartada
# por todos los métodos.
