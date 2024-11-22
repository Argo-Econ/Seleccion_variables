#------------------------------------------------------------------------------#
# Algoritmos para selección de variables relevantes
#------------------------------------------------------------------------------#

# Descarga  ----
# Elaborado por: Arturo Gonzalez


# Carga de paquetes ----
# ---------------------------------------------------------------------------- #

pacman::p_load(dplyr, glue, janitor, readxl, tictoc, lubridate
               , httr, glue, dplyr, ggplot2, BMS, Coinprofile)


# Definición rutas ----
# ---------------------------------------------------------------------------- #
Data_in  <- "Datos_entrada/"
Data_out <- "Resultados/"



# Lectura de información ----
# ---------------------------------------------------------------------------- #
Datos_ent <- read_excel(path = glue("{Data_in}Base_Intro.xlsx")
  ,sheet = "Base",range = "a3:r229",col_names = T)
tail(Datos_ent)


## Transformaciones ----
# ---------------------------------------------------------------------------- #
names(Datos_ent)
Datos_ent_var_yoy_mom <- Datos_ent |> mutate(across(ISE_PM3:EMBI,list(
  var_yoy = function(x) ((x/lag(x, n = 12))-1)*100,
  var_mom = function(x) ((x/lag(x, n = 1))-1)*100)
  ,.names = "{.col}_{.fn}"))
  

# Correlaciones dinámicas ----
# ---------------------------------------------------------------------------- #

Correlacion_dinamica <- function(bd,x,y)
{
  salida <- NULL
  for (i in x) {
    print(i)
    a <- bd[i]
    b <- bd[y]
    #d <- ccf(x = a, y = b, plot = F, lag.max = 24, type = "correlation")
    d <- forecast::Ccf(x = a, y = b, plot = F, lag.max = 24, type = "correlation")
    d[["snames"]] <- paste(i,"&", y, sep = " ")
    windows()
    plot(d)
    cor = d$acf[,,1]
    lag = d$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(abs(res$cor)),]
    #print(res_max)
    
    salida <- rbind(salida,res_max)
  }
  row.names(salida) <- x
  print(salida)
}

names(Datos_ent_var_yoy_mom)

Correlacion_dinamica(bd = Datos_ent_var_yoy_mom,x = "US_IP_var_yoy", y="ISE_PM3_var_yoy")

## Evaluación multiple ----
test <- Correlacion_dinamica(bd = Datos_ent_var_yoy_mom,x = names(Datos_ent_var_yoy_mom), y="ISE_PM3_var_yoy")

# Seleccionar solo las varaibles que tienen correlación de liderazgo

## Resultados ----
var_relevantes1 <- test |>  filter(lag <0)
var_relevantes1

# Sugerencia (usar las mismas unidades)
names(Datos_ent)
var_relevantes2 <- Datos_ent_var_yoy_mom |> select(ends_with("_var_yoy") | DTF_90d:Exp_inf_12m)


var_relevantes2 <- Correlacion_dinamica(bd = var_relevantes2,x = names(var_relevantes2),
  y = "ISE_PM3_var_yoy")

var_relevantes2 <- var_relevantes2 |>  filter(lag <0)
var_relevantes2

## Exportación resultados ----
openxlsx::write.xlsx(x = var_relevantes2,file = glue("{Data_out}Var_relevantes.xlsx"))


# Metodo Bayesiano ----
# ---------------------------------------------------------------------------- #
# Ordenar los datos, donde la primera variable es el objetivo

## Ordenar los datos ----
Datos_eval_ord <- Datos_ent_var_yoy_mom |> select(-Fecha) |> relocate(ISE_PM3_var_yoy) |> na.omit() |> as.data.frame()
names(Datos_eval_ord)

# Datos_eval_ord <- Datos_ent_var_yoy_mom |> select(-c(Fecha,ISE_PM3,ISE_PM3_var_mom)) |> relocate(ISE_PM3_var_yoy) |> na.omit() |> as.data.frame()
# names(Datos_eval_ord)


str(Datos_eval_ord)
eval0 <- bms(X.data = Datos_eval_ord,mprior = "uniform", g = "UIP",nmodel = 10000)
image(eval0)
plotModelsize(eval0)

# Sugerencia: unidades homogeneas
Datos_eval_ord <- Datos_ent_var_yoy_mom|> select(-Fecha) |> select(ends_with("_var_yoy") | DTF_90d:Exp_inf_12m) |> relocate(ISE_PM3_var_yoy) |> na.omit() |> as.data.frame()



## Evaluación con priors ----

# mprior -> define la forma de la distribución del prior
# "g" -> ajusta la compensación o penalización entre la complejidad del modelo y su ajuste con  la variable objetivo


### met1 ----
# "BRIC" - Bayesian Regression Information Criterion
# Se busca un buen ajuste y se penaliza la complejidad del modelo
eval1 <- bms(Datos_eval_ord, mprior = "uniform", g="BRIC", nmodel = 10000)
image(eval1)
plotModelsize(eval1)

### met2 ----
# "RIC"  - Regularized Information Criterion
# método adecuado cuando se sospecha de presencia de multicolinealidad en las
# variables exxokicativas
eval2 <- bms(Datos_eval_ord, mprior = "uniform", g="RIC", nmodel = 10000)
image(eval2)
plotModelsize(eval2)

### met3 ----
# "ELB" - Evidence Lower Bound
# cuando existen modelos muy complejos y quiere medir su verosimilitud
eval3 <- bms(Datos_eval_ord, mprior = "uniform", g="RIC", nmodel = 50000)
image(eval3)
plotModelsize(eval3)

### met4 ----
# fijar un número fijo de parametros
eval4 <- bms(Datos_eval_ord, mprior = "random", g="UIP", user.int=F, mprior.size = 4)
image(eval4)
plotModelsize(eval4)


## Mas robusto ----
# PIP Predictive Information Prior se utiliza en casos donde la predicción es más importante 
# "BRIC" - Bayesian Regression Information Criterion
# UIP -> Uniformly Informative Prior
# MIP -> Model-Independent Prior - Penaliza el modelo basado en el npimero de parametros utilizados

# "RIC" - Regularized Information Criterion
# "ELB" - Evidence Lower Bound
names(Datos_eval_ord)
eval5 <- bms(Datos_eval_ord, burn=5000, iter=100000, mprior="pip", g="BRIC", nmodel=10000, mcmc="bd")
image(eval5)
plotModelsize(eval5)
density(eval5,reg="Repo")
density(eval5,reg="Repo",addons="Eebl")

## mas complejo en la estimación con MCMC
# rev.jump busqueda más robusta cuando se tiene muchas varaibles para evaluar
eval6 <- bms(Datos_eval_ord, burn=5000, iter=100000, mprior="pip", g="BRIC", nmodel=10000, mcmc="rev.jump")
image(eval6)
plotModelsize(eval6)
density(eval5,reg="Repo",addons="Eebl")

# El valor beta de todas las met probadas
windows()
plotComp(Uniforme=eval1, Fijo=eval2, PIP=eval3, Random=eval4, Robusto=eval5, rev_jump=eval6,comp = "Post Mean")

# El valor de probabilidad PIP de cada factor
windows()
plotComp(Uniforme=eval2, Fijo=eval3, PIP=eval1, Random=eval4, Robusto=eval5, rev_jump=eval6,comp ="PIP")

# Guardar los resultados en carpeta
png(filename = glue("{Data_out}Resultados_PMP_Variables relevantes.png"))
plotComp(Uniforme=eval2, Fijo=eval3, PIP=eval1, Random=eval4, Robusto=eval5, rev_jump=eval6,comp ="PIP")
dev.off()

png(filename = glue("{Data_out}Resultados_betas_BMA.png"))
plotComp(Uniforme=eval2, Fijo=eval3, PIP=eval1, Random=eval4, Robusto=eval5, rev_jump=eval6,comp ="PIP")
dev.off()


## Pronósticos con BMA -----
# -------------------------------------------------------------------------------------#

## Exogenas para pronos ----
exo_pronos0 <- Datos_ent_var_yoy_mom |> select(-Fecha) |> relocate(ISE_PM3_var_yoy) |> as.data.frame() |> tail(1)
dim(exo_pronos0)

exo_pronos <- Datos_ent_var_yoy_mom|> select(-Fecha) |> select(ends_with("_var_yoy") | DTF_90d:Exp_inf_12m) |> relocate(ISE_PM3_var_yoy) |> as.data.frame() |> tail(1)

dim(exo_pronos)

### pronos inicial ----
eval0_pronos <- pred.density(object = eval0,newdata = exo_pronos0[,-1])
eval0_pronos
# jeje!!

### pronos met 1 ----
eval1_pronos <- pred.density(object = eval1,newdata = exo_pronos[,-1])
eval1_pronos

### pronos met 2 ----
eval2_pronos <- pred.density(object = eval2,newdata = exo_pronos[,-1])
eval2_pronos

### pronos met 3 ----
eval3_pronos <- pred.density(object = eval3,newdata = exo_pronos[,-1])
eval3_pronos

### pronos met 4 ----
eval4_pronos <- pred.density(object = eval4,newdata = exo_pronos[,-1])
eval4_pronos

### pronos met 4 ----
eval5_pronos <- pred.density(object = eval5,newdata = exo_pronos[,-1])
eval5_pronos

### pronos met 4 ----
eval6_pronos <- pred.density(object = eval6,newdata = exo_pronos[,-1])
eval6_pronos


# Método Perfiles coincidentes ----
# -----------------------------------------------------------------------------/

# Ejemplo de ilustración

## X lidera a Y ----
set.seed(123)
w <- seq(-3, 7, length.out = 100)
x <- sin(pi*w)+rnorm(100,0,0.1)
y <- sin(pi*w-1)+rnorm(100,0,0.1)
coincident_profile(x = x, y = y,frequ =  4,MLag = 6
                   ,nvar1 =  "Serie_contraste"
                   ,nvar2 =  "Serie_objetivo"
                   ,print.graf =  TRUE
                   ,iyear = 2000,lyear =  2024,imonth =  4,lmonth =  4
                   ,tit1 = "serie X (Ref)"
                   ,tit2 = "serie Y (Obj)"
                   ,tit3 = "Series X & Y")


## Implementación ----

head(Datos_ent_var_yoy_mom)
tail(Datos_ent_var_yoy_mom)
names(Datos_ent_var_yoy_mom)

datos_prueba <- Datos_ent_var_yoy_mom |> select(Cartera_Comercial_var_yoy,ISE_PM3_var_yoy) |> na.omit()
dim(datos_prueba)

tail(datos_prueba)
names(datos_prueba)

## Contraste ----
test <- coincident_profile(x = datos_prueba$Cartera_Comercial_var_yoy
  , y = datos_prueba$ISE_PM3_var_yoy, frequ = 12, MLag = 12
  , nvar1 = names(datos_prueba)[1], nvar2 = names(datos_prueba)[2]
  , print.graf = TRUE
  , iyear = 2006,imonth = 1, lyear = 2024,lmonth = 8
  , tit1 = names(datos_prueba)[1], tit2 = names(datos_prueba)[2]
  , tit3 = paste(names(datos_prueba)[1],"&", names(datos_prueba)[2])
    )



## Implementación recurrente ----


Perfil_coincidente <- function(bd,x,y){
  
  salida <- NULL
  nom <- NULL
  for (i in x) {
    tryCatch(
      {
        cat("Perfil coincidente entre", i, "y", y, "\n")
        bd_test <- bd |> select(i,y) |> rename("s_ref"=i, "s_obj"=y)
        dim(bd_test)
        
        windows()
        test <- coincident_profile(x = bd_test$s_ref
          , y = bd_test$s_obj, frequ = 12, MLag = 12
          , nvar1 = names(bd)[i], nvar2 = y, print.graf = TRUE
          , iyear = 2006,imonth = 1, lyear = 2024,lmonth = 8
          , tit1 = i, tit2 = y
          , tit3 = paste(i,"&", y)
        )
        
        nom <- c(nom,i)
        result <- test$MainLag[1]
        salida <- rbind(salida,result)
        dim(salida)
        
        },
      error = function(e) {
        cat("Se produjo un error -No hay suficientes turning points (TP) para", i, ":", e$message, "\n")
        nom <- nom
        
      })

  }
  cat("Exportación resultados finales \n")
  
  #print(nom)
  rownames(salida) <- nom
  print(salida)
  return(salida)
}

## implementación 2 ----
Datos_contraste <- var_relevantes2 <- Datos_ent_var_yoy_mom |> select(ends_with("_var_yoy") | DTF_90d:Exp_inf_12m) |> na.omit()

length(Datos_contraste$IPC_var_yoy)


Prueba_PerCoin <- Perfil_coincidente(Datos_contraste,names(Datos_contraste)[2:17],"ISE_PM3_var_yoy")
Prueba_PerCoin

## Exportación resultados ----
openxlsx::write.xlsx(x = Prueba_PerCoin,file = glue("{Data_out}Resultados_Perfil_Coincidente.xlsx"))

# ---------------------------------------------------------------------------------------/
# Fin de programa
# ---------------------------------------------------------------------------------------/