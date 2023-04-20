#Instala y carga el siguiente paquete

install.packages("readr")
library(readr)

#Indícale a R en qué ruta está la carpeta donde están contenidos todos los archivos que quieres usar

all_files=list.files("/Users/mariaceleste/Desktop/TFG_testsR/Emg_anaysis", 
                     pattern = "*.csv", full.names = TRUE)

#Crea un bucle for para que, todo lo que contenga el bucle, lo ejecute para cada uno de los arvhivos
#que están en la carpeta que has indicado antes

for(file in all_files){
  
  #Lee el arhivo i-ésimo (puedes hacerlo así, o como tú lo tengas en tu script)
  data_df <- read_delim(file, 
                        ";", escape_double = FALSE, trim_ws = TRUE,
                        locale = locale(grouping_mark = "."))
  
########################################################
  
  #Añade aquí dentro todo tu script, es decir, todo el procesado que haya que hacer para el archivo i-ésimo
  
##########################################################  
  
  #La siguiente instrucción tendrás que ejecutarla aquí dentro del bucle si quieres que se genere un archivo .csv
  #para cada uno de los archivos originales.Si quieres un único archivo .csv que contenga todo, debes hacerlo fuera
  #del bucle. MIRA BIEN QUÉ TIPO DE ARCHIVO ESPERA SPM EN MATLAB, PARA GENERAR ESTOS ARCHIVOS CON EL FORMATO
  #ADECUADO.
  
  #IMPORTANTE: indica aquí en qué carpeta quieres que se te generen los archivos. DEBE SER UNA CARPETA DISTINTA
  #A LA DE ORIGEN (USA SETWD PARA DETERMINAR LA CARPETA DE TRABAJO, QUE SERÁ LA CARPETA DE DESTINO), 
  #YA QUE, COMO LOS ACRHIVOS SE VAN A GENERAR CON EL MISMO NOMBRE QUE TENÍAN AL PRINCIPIO, SI ESTÁS
  #EN LA MISMA CARPETA, SE VAN A SOBREESCRIBIR LOS ANTIGUOS.
  
  setwd ("C:/RUTA/.../CarpetaDESTINO")
  write.table(x=data_df, file=file, sep=";", row.names=FALSE)
  
  #Si quieres hacer un único data.frame con todos los datos de todos los arvhivos, debes generar el código para crearlo
  #primero y, después, generar un único archivo excel (tras cada iteración del bucle, debe ir rellenándose ese
  #"gran" data frame que contenga todo)
  
  ################
  #Código para ir creando en cada iteración un único data.frame con toda la información
  ################
  
}


setwd ("C:/RUTA/.../CarpetaDESTINO")
write.table(x=data_df, file="ArchivoFinal.csv", sep=";", row.names=FALSE)