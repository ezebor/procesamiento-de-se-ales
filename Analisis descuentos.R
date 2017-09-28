dir = "/Users/ezequiel/Desktop/procesamiento-de-se-ales/datos.csv"
cantidad.registros = length(readLines(file(dir,open="r")))
datos <- read.table(file(dir,open="r"), sep=",", header = TRUE)

cantidad.meses = 36 # 12 meses de 2015, 12 meses de 2016 y 12 meses de 2017
total.ordenes.con.cupones = rep(0, cantidad.meses)
total.ordenes.sin.cupones = rep(0, cantidad.meses)
ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones = rep(0, cantidad.meses)
for(i in 1:cantidad.registros){
  mes.indice = datos$Mes[i]
  if (datos$Anio[i] == 2016){
    mes.indice <- mes.indice + 12 
  }else if(datos$Anio[i] == 2017){
    mes.indice <- mes.indice + 24 
  }
  
  total.ordenes.sin.cupones[mes.indice] <- total.ordenes.sin.cupones[mes.indice] + datos$TotalCanceladas[i] - datos$TotalCanceladasConCupones[i] 
  total.ordenes.con.cupones[mes.indice] <- total.ordenes.con.cupones[mes.indice] + datos$TotalNoCanceladasConCupones[i] + datos$TotalCanceladasConCupones[i]
  
  # RATIO 1: razón entre órdenes con cupones y órdenes sin cupones
  ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones[mes.indice] <- total.ordenes.con.cupones[mes.indice] / total.ordenes.sin.cupones[mes.indice]
}

op <- par(mfrow = c(1,2))
meses = seq(from = 1, to = cantidad.meses, by = 1)
hist(total.ordenes.con.cupones, 
     main="Frecuencia de descuentos", 
     xlab="Cantidades de cupones aplicados", 
     ylab="Cantidades de ocurrencias", 
     col="green",
     xlim=c(0,25000),
     las=1, 
     breaks=5)
plot(meses, 
     ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones,
     main="Órdenes con cupones vs. Órdenes sin cupones", 
     xlab="Meses", 
     ylab="Razón con.cupones / sin.cupones",
     col="red",
     type = 'l')

# Cambio de año
abline(v = 12, lty = 1, col = 'blue')
abline(v = 24, lty = 1, col = 'blue')
abline(v = 36, lty = 1, col = 'blue')

# Fechas especiales
abline(v = 5, lty = 3) # Hot sale 2015
abline(v = 11, lty = 3) # Black friday 2015
abline(v = 17, lty = 3) # Hot sale 2016
abline(v = 23, lty = 3) # Black friday 2016
abline(v = 29, lty = 3) # Hot sale 2017
abline(v = 35, lty = 3) # Black friday 2017


