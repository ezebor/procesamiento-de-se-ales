################ CARGA DATOS ################

# dir = "/home/braian/R/repo/procesamiento-de-se-ales/datos.csv"
dir = "/Users/ezequiel/Desktop/procesamiento-de-se-ales/datos.csv"
csv <- file(dir,open="r")
cantidad.registros = length(readLines(csv))
datos <- read.table(file(dir,open="r"), sep=",", header = TRUE)

################ INICIALIZACIÓN DE VECTORES ################

cantidad.meses = 36 # 12 meses de 2015, 12 meses de 2016 y 12 meses de 2017
total.ordenes.con.cupones = rep(0, cantidad.meses)
total.ordenes.sin.cupones = rep(0, cantidad.meses)
total.ordenes.canceladas = rep(0, cantidad.meses)
total.ordenes.canceladas.con.cupones = rep(0, cantidad.meses)
ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones = rep(0, cantidad.meses)
indice.ordernes.con.cupones.sobre.ordenes.totales = rep(0, cantidad.meses)
indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales = rep(0, cantidad.meses)

################ CÁLCULOS ################

for(i in 1:(cantidad.registros-1)){
  mes.indice = datos$Mes[i]
  if (datos$Anio[i] == 2016){
    mes.indice <- mes.indice + 12 
  }else if(datos$Anio[i] == 2017){
    mes.indice <- mes.indice + 24 
  }
  
  total.ordenes.sin.cupones[mes.indice] <- total.ordenes.sin.cupones[mes.indice] + datos$TotalCanceladas[i] - datos$TotalCanceladasConCupones[i] 
  total.ordenes.con.cupones[mes.indice] <- total.ordenes.con.cupones[mes.indice] + datos$TotalNoCanceladasConCupones[i] + datos$TotalCanceladasConCupones[i]
  total.ordenes.canceladas[mes.indice] <- total.ordenes.canceladas[mes.indice] + datos$TotalCanceladas[i]
  total.ordenes.canceladas.con.cupones[mes.indice] <- total.ordenes.canceladas.con.cupones[mes.indice] + datos$TotalCanceladasConCupones[i]
  
  # RATIO 1: razón entre órdenes con cupones y órdenes sin cupones
  ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones[mes.indice] <- total.ordenes.con.cupones[mes.indice] / total.ordenes.sin.cupones[mes.indice]

  # ÍNDICE 1: órdenes con cupones sobre totales
  indice.ordernes.con.cupones.sobre.ordenes.totales[mes.indice] <- 100 * (total.ordenes.con.cupones[mes.indice] / (total.ordenes.con.cupones[mes.indice] + total.ordenes.sin.cupones[mes.indice]))

  # ÍNDICE 2: órdenes canceladas con cupones sobre totales canceladas
  indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales[mes.indice] <- 100 * (total.ordenes.canceladas.con.cupones[mes.indice] / total.ordenes.canceladas[mes.indice])
  
}

tmin <- as.Date("2015-01-01")
tmax <- as.Date("2017-12-31")
meses <- seq(tmin, tmax, by="month")

close(csv)

################ GRÁFICOS ################

# RATIO 1
plot.index(ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones, 
           "Órdenes con cupones vs. Órdenes sin cupones",
           "Razón Con cupones / Sin cupones")

# Histograma de frecuencias de descuentos
hist(total.ordenes.con.cupones, 
     main="Frecuencia de descuentos", 
     xlab="Cantidades de cupones aplicados", 
     ylab="Cantidades de ocurrencias", 
     col="green",
     xlim=c(0,25000),
     las=1, 
     breaks=5)

# ÍNDICE 1
plot.index(indice.ordernes.con.cupones.sobre.ordenes.totales, 
           "Órdenes con cupones sobre órdenes totales",
           "% Con cupones / Totales")

# ÍNDICE 2
plot.index(indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales, 
           "Órdenes canceladas con cupones sobre totales canceladas",
           "% Con cupones canceladas / canceladas")

################ FUNCIONES AUXILIARES ################

plot.index = function(ratio, titulo, y.label){
  lab <- format(meses,format="%Y-%b")
  plot(meses, 
       ratio,
       main= titulo, 
       ylab= y.label,
       col= "red",
       type = 'l',
       xaxt="n", 
       xlab="",
       las = 2
  )
  axis(1, at=meses, labels=FALSE)
  text(x=meses, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
       labels=lab, srt=45, adj=0.8, xpd=TRUE)
  plot.lines()
}

plot.lines = function(){
  abline(v = meses[12], lty = 1, col = 'blue') # Fin 2015
  abline(v = meses[24], lty = 1, col = 'blue') # Fin 2016
  abline(v = meses[36], lty = 1, col = 'blue') # Fin 2017
  
  abline(v = meses[5], lty = 3) # Hot sale 2015
  abline(v = meses[11], lty = 3) # Black friday 2015
  abline(v = meses[17], lty = 3) # Hot sale 2016
  abline(v = meses[23], lty = 3) # Black friday 2016
  abline(v = meses[29], lty = 3) # Hot sale 2017
  abline(v = meses[35], lty = 3) # Black friday 2017
}
