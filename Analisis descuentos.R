dir = "/Users/ezequiel/Desktop/procesamiento-de-se-ales/datos.csv"
cantidad.registros = length(readLines(file(dir,open="r")))
datos <- read.table(file(dir,open="r"), sep=",", header = TRUE)

cantidad.meses = 24 # 12 meses de 2016 y 12 meses de 2017
cantidad.ordenes.no.canceladas.con.cupones.por.mes = rep(0, cantidad.meses)
cantidad.ordenes.no.canceladas.sin.cupones.por.mes = rep(0, cantidad.meses)
ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones = rep(0, cantidad.meses)
for(i in 1:cantidad.registros){
  mes.indice = datos$Mes[i]
  if (datos$Anio[i] == 2017){
    mes.indice <- mes.indice + 12 
  }
  cantidad.ordenes.no.canceladas.con.cupones.por.mes[mes.indice] <- cantidad.ordenes.no.canceladas.con.cupones.por.mes[mes.indice] + datos$TotalNoCanceladasConCupones[i]
  cantidad.ordenes.no.canceladas.sin.cupones.por.mes[mes.indice] <- cantidad.ordenes.no.canceladas.sin.cupones.por.mes[mes.indice] + datos$TotalNoCanceladasSinCupones[i]
  
  # RATIO 1: razón entre órdenes con cupones y órdenes sin cupones
  ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones[mes.indice] <- cantidad.ordenes.no.canceladas.con.cupones.por.mes[mes.indice] / cantidad.ordenes.no.canceladas.sin.cupones.por.mes[mes.indice]
}

op <- par(mfrow = c(1,2))
meses = seq(from = 1, to = cantidad.meses, by = 1)
hist(cantidad.ordenes.no.canceladas.con.cupones.por.mes, 
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



