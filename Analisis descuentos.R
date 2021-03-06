################ CARGA DATOS ################

# dir = "/home/braian/R/repo/procesamiento-de-se-ales/datos.csv"
dir = "/Users/ezequiel/Desktop/procesamiento-de-se-ales/datos.csv"
csv <- file(dir,open="r")
cantidad.registros = length(readLines(csv))
datos <- read.table(file(dir,open="r"), sep=",", header = TRUE)

################ INICIALIZACI�N DE VECTORES ################

cantidad.meses = 36 # 12 meses de 2015, 12 meses de 2016 y 12 meses de 2017
total.ordenes.con.cupones = rep(0, cantidad.meses)
total.ordenes.sin.cupones = rep(0, cantidad.meses)
total.ordenes.canceladas = rep(0, cantidad.meses)
total.ordenes.generadas = rep(0, cantidad.meses)
total.ordenes.canceladas.con.cupones = rep(0, cantidad.meses)
ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones = rep(0, cantidad.meses)
indice.ordernes.con.cupones.sobre.ordenes.totales = rep(0, cantidad.meses)
indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales = rep(0, cantidad.meses)

montos.totales = rep(0, cantidad.meses)
montos.totales.descontado = rep(0, cantidad.meses)

indice.ordenes.canceladas.sobre.total.generadas = rep(0, cantidad.meses)

################ C�LCULOS ################

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
  
  montos.totales[mes.indice] <- montos.totales[mes.indice] + as.numeric(datos$Total[i])
  montos.totales.descontado[mes.indice] <- montos.totales.descontado[mes.indice] + as.numeric(datos$TotalDiscount[i])
  
  total.ordenes.generadas[mes.indice] <- total.ordenes.generadas[mes.indice] + datos$TotalGeneradas
  indice.ordenes.canceladas.sobre.total.generadas[mes.indice] <- total.ordenes.canceladas[mes.indice] / total.ordenes.generadas[mes.indice]
  
  # RATIO 1: raz�n entre �rdenes con cupones y �rdenes sin cupones
  ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones[mes.indice] <- total.ordenes.con.cupones[mes.indice] / total.ordenes.sin.cupones[mes.indice]

  # �NDICE 1: �rdenes con cupones sobre totales
  indice.ordernes.con.cupones.sobre.ordenes.totales[mes.indice] <- 100 * (total.ordenes.con.cupones[mes.indice] / (total.ordenes.con.cupones[mes.indice] + total.ordenes.sin.cupones[mes.indice]))

  # �NDICE 2: �rdenes canceladas con cupones sobre totales canceladas
  indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales[mes.indice] <- 100 * (total.ordenes.canceladas.con.cupones[mes.indice] / total.ordenes.canceladas[mes.indice])
  
  # Media movil
  tipoMA = rep(1/9,9)
  media.ratio1 = filter(ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones, tipoMA, circular = FALSE)
  media.indice1 = filter(indice.ordernes.con.cupones.sobre.ordenes.totales, tipoMA, circular = FALSE)
  media.indice2 = filter(indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales, tipoMA, circular = FALSE)
  media.indice3 = filter(indice.ordenes.canceladas.sobre.total.generadas, tipoMA, circular = FALSE)
  media.totales = filter(montos.totales, tipoMA, circular = FALSE)
  media.totales.descontados = filter(montos.totales.descontado, tipoMA, circular = FALSE)
  
}

tmin <- as.Date("2015-01-01")
tmax <- as.Date("2017-12-31")
meses <- seq(tmin, tmax, by="month")

close(csv)

################ FUNCIONES AUXILIARES ################

plot.index = function(ratio, media, titulo, y.label){
  op <- par(mfrow = c(1,1))
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
  lines(meses, media, col= "green")
  axis(1, at=meses, labels=FALSE)
  text(x=meses, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
       labels=lab, srt=45, adj=0.8, xpd=TRUE)
  plot.lines()
}

plot.ordenes = function(){
  plot.index(indice.ordenes.canceladas.sobre.total.generadas, media.indice3,
             "% de �rdendes canceladas sobre el total de �rdenes generadas", "")
}

plot.totales = function(montos, media, titulo, y.label){
  montos.pretty = rep(0, cantidad.meses)
  media.pretty = rep(0, cantidad.meses)
  for(i in 1:cantidad.meses){
    montos.pretty[i] = montos[i] / 1000000
    media.pretty[i] = media[i] / 1000000
  }
  plot(meses, 
       montos.pretty,
       main= titulo, 
       ylab= y.label,
       col= "red",
       type = 'l',
       las = 1
  )
  lines(meses, media.pretty, col= "green")
  plot.lines()
}

plot.montos.totales = function(){
  op <- par(mfrow = c(1,2))
  plot.totales(montos.totales, media.totales, "Montos totales [Millones]", "")
  plot.totales(montos.totales.descontado, media.totales.descontados, "Montos descontados [Millones]", "")
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

################ GR�FICOS ################

# RATIO 1
plot.index(ratio.ordenes.con.cupones.sobre.ordenes.sin.cupones, media.ratio1,
           "�rdenes con cupones sobre �rdenes sin cupones",
           "Raz�n: �rdenes con cupones / �rdenes sin cupones")

# Histograma de frecuencias de descuentos
hist(total.ordenes.con.cupones, 
     main="Frecuencia de descuentos", 
     xlab="Cantidades de cupones aplicados", 
     ylab="Cantidades de ocurrencias", 
     col="green",
     xlim=c(0,25000),
     las=1, 
     breaks=5)

# �NDICE 1
plot.index(indice.ordernes.con.cupones.sobre.ordenes.totales, media.indice1,
           "�rdenes con cupones sobre �rdenes totales",
           "% de �rdenes con cupones sobre �rdenes totales")

# �NDICE 2
plot.index(indice.ordernes.canceladas.con.cupones.sobre.ordenes.canceladas.totales, media.indice2,
           "% de �rdenes canceladas con cupones sobre �rdenes canceladas",
           "% �rdenes canceladas con cupones sobre �rdenes canceladas")

# MONTOS TOTALES VS MONTOS DESCONTADOS
plot.montos.totales()

# �RDENES CANCELADAS VS �RDENES TOTALES

plot.ordenes()

