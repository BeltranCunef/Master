##########################
### Autor: Beltrán Aller López
### Fecha: 02/11/2019
### Asignatura: Predicción
### Referencias: https://bankunderground.co.uk/2019/11/19/build-your-own-fancharts-in-r/
##########################

############# LIBRERIAS ###############

library(tidyverse)
library(quantmod)   # For data access
library(vars)

############# CARGA DE DATOS ############# 

series = c('JPNGDPRQPSMEI', 'CPALTT01JPQ661S') # FRED codes for JAPAN GDP growth and CPI
Growth = getSymbols(series[1], src = 'FRED', auto.assign = FALSE)
CPI    = getSymbols(series[2], src = 'FRED', auto.assign = FALSE)

### Juntamos las dos series y borramos los valores NA, ya que las series no son de la misma longitud, tenemos CPI desde el 85 y GROWTH desde el 95

Data = inner_join(tibble(Date = time(Growth), Growth = coredata(Growth)), 
                  tibble(Date = time(CPI), CPI = coredata(CPI)), by = c("Date")) %>% 
  mutate(Inflation = 100*(CPI/lag(CPI,4) - 1)) %>%
  dplyr::select(Date, Growth, Inflation) %>% 
  drop_na() 

### Ahora calculo el numero optimo de lags

VARselect(Data[,2:3], lag.max = 8, type = "const")[["selection"]]

VARselect(Data[,2:3], lag.max = 8, type = "const")[["criteria"]]

### Acorde al AIC deberia tomar 5 lags, sin embargo el BIC (SC) recomienda 1. Probamos tanto con 1 lag como con 5.

############# INSPECCION: PRIMER GRAFICO ############# 

centre_colour = c("seagreen","tomato") # Colours for time series/centre of fancharts
tail_colour   = "gray95"               # Colour for the tails, used later but defined here 
pivot_longer(Data, cols = -Date, names_to = "Variables", values_to = "Values") %>% 
  ggplot() + 
  geom_line(aes(x = Date, y = Values, group = Variables, colour = Variables), size = 1.1, show.legend = TRUE) +
  scale_colour_manual(values = centre_colour) +
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  labs(title = "JAPAN GDP growth and CPI inflation", x = "", y = "",
       caption = paste0("Source: FRED series ", paste(series, collapse = ", ")))

############# CON LIBRERIA VARS ############# 

####### Calculo el modelo con un unico lag ######

varp1 <- VAR(Data[,2:3], p =  1, type = "const")

summary(varp1)

### La constante o beta no es estadisticamente significativo, mientras que los restardos de las variables del sistema si, obtenemos un R^2
### ajustado de 0.7229, lo cual no esta nada mal

### Analizamos la causalidad de Granger para el modelo

causality(varp1, cause = "Growth")
causality(varp1, cause = "Inflation")

### Se obtiene un p-valor inferior a 0.05 para ambas variables, por lo que se rechaza la hipotesis nula y se puede decir que los resultados de ambas variables 
### sirven para predecir a la otra 

### Mido la respuesta al impulso

model.ri = irf(varp1)
model.ri
plot(model.ri)

fanchart(model.ri)

### Realizo la prediccion del primer modelo, a 8 periodos y con un intervalo de confianza del 95%

predict(varp1, n.ahead = 8, ci = 0.95)

####### Calculo el modelo con 5 lags ######

varp5 <- VAR(Data[,2:3], p = 5, type = "const")

summary(varp5)

### La constante o beta no es estadisticamente significativo, mientras que los primeros retardos de ambas variables, el 4 de la inflacion y el quinto de
### esta misma si, obtenemos un R^2 ajustado de 0.7875

### Analizamos la causalidad de Granger para el modelo

causality(varp5, cause = "Growth")
causality(varp5, cause = "Inflation")

### Se obtiene un p-valor inferior a 0.05 para Growth, por lo que se rechaza la hipotesis nula y se puede decir que los resultados de esta variable 
### sirven para predecir a la otra. No ocurre lo mismo para Inflation 

### Mido la respuesta al impulso

model.ri5 = irf(varp5)
model.ri5
plot(model.ri5)

### Realizo la prediccion del segundo modelo, también a 8 periodos y con un intervalo de confianza del 95%

predict(varp5, n.ahead = 8, ci = 0.95)

############# MODELO ############# 

m     = 1 # maximum lag in VAR
Datal = Data %>%
  pivot_longer(cols = -Date, names_to = "Names", values_to = "Values") %>%
  mutate(lag_value = list(0:m)) %>%
  unnest(cols = lag_value) %>%
  group_by(Names, lag_value) %>%
  mutate(Values = lag(Values, unique(lag_value))) %>%
  ungroup() %>%
  mutate(Names = if_else(lag_value == 0, Names,              # No suffix at lag 0
                         paste0(Names, "_", str_pad(lag_value, 2, pad = "0")))) %>% # All other lags 
  dplyr::select(-lag_value) %>%      # Drop the redundant lag index
  pivot_wider(names_from = Names, values_from = Values) %>%
  slice(-c(1:m)) %>%          # Remove missing lagged initial values
  mutate(constant = 1)           # Add column of ones at end

s = paste(paste0(str_pad(1:m, 2, pad = "0"), "$"), collapse = "|")
X = data.matrix(dplyr::select(Datal, matches(paste0(s,"|constant"))))
Y = data.matrix(dplyr::select(Datal, -matches(paste0(s,"|constant|Date"))))

(bhat = solve(crossprod(X), crossprod(X,Y)))

############# FORECAST ############# 

nv    = ncol(Y) # Number of variables
nf    = 8      # Periods to forecast
nb    = 16      # Periods of back data to plot, used later

v     = crossprod(Y - X %*% bhat)/(nrow(Y) - m*nv - 1)            # Calculate error variance
bhat2 = bhat[rep(seq(1,m*nv,m),m) + rep(seq(0,m - 1), each = nv),] # Reorder for simulation
A     = rbind(t(bhat2), diag(1,nv*(m - 1), nv*m))                # First order form - A 
B     = diag(1,nv*m,nv)                                        # First order form - B
cnst  = c(t(tail(bhat,1)), rep(0,nv*(m - 1)))                    # First order constants

# Simulation loop

Yf     = matrix(0,nv*m,nf + 1)                # Stores forecasts
Yf[,1] = c(t(tail(Y,m)[m:1,]))              # Lagged data
Pf     = matrix(0,nv,nf + 1)                  # Stores variances
P      = matrix(0,nv*m,nv*m)                # First period state covariance

for (k in 1:nf) { 
  Yf[,k + 1] = cnst + A %*% Yf[,k]
  P        = A %*% P %*% t(A) + B %*% v %*% t(B)
  Pf[,k + 1] = diag(P)[1:nv]
}

############# FANCHARTS ############# 

qu     = c(.05,.2,.35,.65,.8,.95)  # Chosen quantiles ensures 30% of the distribution each colour
nq     = length(qu)
fdates = seq.Date(tail(Data$Date,1), by = "quarter", length.out = nf + 1) # Forecast dates

forecast_data = tibble(Date     = rep(fdates, 2), 
                       Variable = rep(colnames(Data)[-1], each = (nf + 1)), 
                       Forecast = c(t(Yf[1:nv,])),
                       Variance = c(t(sqrt(Pf)))) %>% 
  bind_cols(map(qu, qnorm, .$Forecast, .$Variance)) %>%         # Calculate quantiles
  dplyr::select(-c("Forecast", "Variance")) %>% 
  {bind_rows(dplyr::select(., -(nq + 2)),                                # Drop last quantile 
             dplyr::select(., -3) %>%                                  # Drop first quantile
               arrange(Variable, desc(Date)) %>%                # Reverse order
               rename_at(-(1:2), ~paste0("V",1:(nq-1))) )} %>%  # Shift names of reversed ones 
  pivot_longer(cols = -c(Date, Variable), names_to = "Area", values_to = "Coordinates") %>% 
  unite(VarArea, Variable, Area, remove = FALSE) %>%              # Create variable to index polygons
  bind_rows(pivot_longer(tail(Data,nb), cols = -Date, names_to = "Variable", values_to = "Backdata"), .)


# Band colours 'ramp' from the centre to the tail colour 

band_colours = colorRampPalette(c(rbind(tail_colour, centre_colour), tail_colour), 
                                space = "Lab")(nv*nq + 1)[-seq(1, nv*nq + 1, nq)]

ggplot(forecast_data) + 
  geom_rect(aes(xmin = Date[nv*nb], xmax = max(Date), ymin = -Inf, ymax = Inf), fill = tail_colour, alpha = .2) +  
  geom_polygon(aes(x = Date, y = Coordinates, group = VarArea, fill = VarArea)) +
  scale_fill_manual(values = band_colours) +
  geom_line(aes(x = Date, y = Backdata, group = Variable, colour = Variable)) +
  scale_colour_manual(values = centre_colour) +
  scale_x_date(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Variable, ncol = 1) +
  labs(title = "Forecasts of Japan GDP growth and CPI inflation", 
       subtitle = paste("Quarterly data, annual rates of change, VAR with", m, "lags"), 
       caption = paste("Source: FRED series", paste(series, collapse = ", ")), x = "", y = "")
