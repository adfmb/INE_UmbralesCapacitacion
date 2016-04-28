# setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016")
#Código que genera archivos con los indicativos de los resultados de las pruebas de hipótesis por c/nivel de confianza

setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis")

###Importando las dos tablas del EdoMex y pegandolas en una sola
# entidad<-read.csv("fechas_mexico_ph2.csv",header = TRUE)
# entidad2<-read.csv("fechas_mexico2_ph2.csv",header = TRUE)
# edomex<-rbind(entidad,entidad2)
# write.csv(edomex,file=paste("fechas_edomex_ph2.csv",sep=""),row.names = FALSE)


tabla_nombres<-data.frame(NOMBRE_ESTADO=c("coahuila","edomex","nayarit","veracruz"))

lista_entidades_completas<-list()
lista_dfs_entidades<-list()

#Creamos la nueva carpeta donde guardareos los resultados de las ph2
#dir.create("ph2")

#con el siguiente for iniciamos el proceso para cada entidad que debemos de tener en un archi
for (i in 1:length(unique(tabla_nombres$NOMBRE_ESTADO))) {
  #i<-1
  setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis")
  edo<-as.character(tabla_nombres$NOMBRE_ESTADO)[i]
  entidad_temp<-read.csv(paste("fechas_",edo,"_ph2.csv",sep=""),header=TRUE)
  
  setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis/ph2")
  dir.create(edo)
  setwd(paste("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis/ph2/",edo,sep=""))
  
  #Aqui se guardara la info de todos los distritos de la entidad
  matriz_entidad_temporal<-matrix(ncol=21)
  
  
  #Creamos los objetos de lista de matrices donde guardaremos cada distrito, lista de df para exportarlos
  lista_matrices<-list()
  lista_dfs<-list()
  
  #antes de dividir por ubicacion, extraigamos las fechas que se utilizaron en la entidad
  todas<-c(as.character(entidad_temp$FECHA_VISITA),as.character(entidad_temp$FECHA_NOTIFICA),as.character(entidad_temp$FECHA_CAPACITA))
  todas2<-as.POSIXct(as.character(todas), format = "%m/%d/%y", tz = "GMT")
  
  todas3<-unique(todas2)
  todas4<-todas3[!is.na(todas3)]
  
  indices_ordenamiento_unicas_todas_fechas<-order(todas4, decreasing = FALSE )
  final_fechas<-todas4[indices_ordenamiento_unicas_todas_fechas]
  
  
  #ahora dividiremos en casos de urbana y no urbana
  for(ubicacion in as.character(unique(entidad_temp$TIPO_SECCION_DEOE))){
    
    # ubicacion<-as.character(unique(entidad_temp$TIPO_SECCION_DEOE[1]))
    #Aqui guardamos el subset de la entidad-temporal con la ubicacion-temporal
    tabla_ubic_temp<-subset(entidad_temp,TIPO_SECCION_DEOE==ubicacion)
    
    #Aqui guardamos los distritos que tiene esa entidad-temporal en el tipo de ubicacion-temporal.
    #Sobre estos se correrá el "for" con el que se harán las tablas por distrito
    distritos<-sort(unique(tabla_ubic_temp$ID_DISTRITO))
    
    #inicializamos la matriz en la que guardaremos la informacion por distrito en la ubicacion-temporal de la entidad-temporal
    matriz_temp<-matrix(ncol=21, nrow=length(final_fechas)) #Agregue tres columnas mas (de 16 a 19) por Ent,Dis,ubic
    colnames(matriz_temp)<-c("Fechas","Dia_del_periodo",
                             "Visitas_ANC","Visitas_AC",
                             "Revisitas_ANC","Revisitas_AC",
                             "Not_ANC","Not_AC",
                             "RechazosInme_ANC","RechazosInme_AC",
                             "Capacitados_ANC","Capacitados_AC",
                             "Aptos_ANC","Aptos_AC",
                             "RechazosCapacitacion_ANC","RechazosCapacitacion_AC",
                             "RechazosTotal_ANC","RechazosTotal_AC",
                             "Entidad","Distrito","Ubicacion") #Los tres campos que se agregaron
    
    tab_ANC_0<-subset(tabla_ubic_temp,ausentismo.critico.en.esa.seccion!="VERDADERO")
    tab_AC_0<-subset(tabla_ubic_temp,ausentismo.critico.en.esa.seccion=="VERDADERO")
    
    for(d in 1:length(distritos)){
      #d<-distritos[1]
      dis<-distritos[d]
      
      #limpiamos la matriz
      matriz_temp[][]<-0
      
      #llenamos los campos 1,17,18,19 que son fijos para todas las fechas y que son: fechas, edo, distrito y la ubic
      matriz_temp[,1]<-as.character(final_fechas) #no se utilizá este vector para operaciones, solo para que aparezca en la tabla final
      matriz_temp[,19]<-edo
      matriz_temp[,20]<-dis
      matriz_temp[,21]<-ubicacion
      
      #Hacemos el vector de las diferencias de dias con respecto al dia 1 de nuestro objeto final_fechas
      dias_diferencia<-data.frame(Diferencia=as.numeric(julian(final_fechas,origin = final_fechas[1]))) #las diferencias en días con respecto al primero día del período
      
      #Hacemos un subset por tipo de Ausentismo con el que se trabajará en el "for" por fecha que sigue a continuacion
      tab_ANC<-subset(tab_ANC_0,ID_DISTRITO==dis)
      tab_AC<-subset(tab_AC_0,ID_DISTRITO==dis)
      
      #Establecemos los universos (totales) de inscaulados en el distrito dis: uno para ANC y otro para AC
      univ_ANC<-nrow(tab_ANC)
      univ_AC<-nrow(tab_AC)
      
      #Corremos con todas las fechas
      for(j in 1:length(final_fechas)){
        #j<-2
        matriz_temp[j,2]<-1+dias_diferencia$Diferencia[j]
        
        if(j==1){
          
          
          matriz_temp[j,3]<-(length(tab_ANC$FECHA_VISITA[which(as.POSIXct(tab_ANC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_ANC
          matriz_temp[j,4]<-(length(tab_AC$FECHA_VISITA[which(as.POSIXct(tab_AC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_AC
          
          matriz_temp[j,5]<-(length(tab_ANC$FECHA_VISITA[which(as.POSIXct(tab_ANC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j]& tab_ANC$REVISITA=="Si")]))/univ_ANC
          matriz_temp[j,6]<-(length(tab_AC$FECHA_VISITA[which(as.POSIXct(tab_AC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$REVISITA=="Si")]))/univ_AC
          
          matriz_temp[j,7]<-(length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_ANC
          matriz_temp[j,8]<-(length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_AC
          
          matriz_temp[j,9]<-(length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos")]))/univ_ANC
          matriz_temp[j,10]<-(length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos")]))/univ_AC
          
          matriz_temp[j,11]<-(length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])
          )/univ_ANC
          matriz_temp[j,12]<-(length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
                                length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
                                length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])
          )/univ_AC
          
          matriz_temp[j,13]<-(length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")]))/univ_ANC
          matriz_temp[j,14]<-(length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")]))/univ_AC
          
          matriz_temp[j,15]<-(length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )/univ_ANC
          
          matriz_temp[j,16]<-(length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )/univ_AC
          
          matriz_temp[j,17]<-(length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos")])+
                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )/univ_ANC
          matriz_temp[j,18]<-(length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos")])+
                                length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )/univ_AC
          
          
        }else{
#                 j<-j+1
#                 matriz_temp[j,2]<-1+dias_diferencia$Diferencia[j]
#           
          matriz_temp[j,3]<-as.numeric(matriz_temp[j-1,3])+(length(tab_ANC$FECHA_VISITA[which(as.POSIXct(tab_ANC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_ANC
          matriz_temp[j,4]<-as.numeric(matriz_temp[j-1,4])+(length(tab_AC$FECHA_VISITA[which(as.POSIXct(tab_AC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_AC
          
          matriz_temp[j,5]<-as.numeric(matriz_temp[j-1,5])+(length(tab_ANC$FECHA_VISITA[which(as.POSIXct(tab_ANC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j]& tab_ANC$REVISITA=="Si"  )]))/univ_ANC
          matriz_temp[j,6]<-as.numeric(matriz_temp[j-1,6])+(length(tab_AC$FECHA_VISITA[which(as.POSIXct(tab_AC$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$REVISITA=="Si" )]))/univ_AC
          
          matriz_temp[j,7]<-as.numeric(matriz_temp[j-1,7])+(length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_ANC
          matriz_temp[j,8]<-as.numeric(matriz_temp[j-1,8])+(length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])]))/univ_AC
          
          matriz_temp[j,9]<-as.numeric(matriz_temp[j-1,9])+(length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos")]))/univ_ANC
          matriz_temp[j,10]<-as.numeric(matriz_temp[j-1,10])+(length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos")]))/univ_AC
          
          matriz_temp[j,11]<-as.numeric(matriz_temp[j-1,11])+
            (length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
               length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
               length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])
            )/univ_ANC
          matriz_temp[j,12]<-as.numeric(matriz_temp[j-1,12])+
            (length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
               length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
               length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])
            )/univ_AC
          
          matriz_temp[j,13]<-as.numeric(matriz_temp[j-1,13])+(length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")]))/univ_ANC
          matriz_temp[j,14]<-as.numeric(matriz_temp[j-1,14])+(length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")]))/univ_AC
          
          matriz_temp[j,15]<-as.numeric(matriz_temp[j-1,15])+
            (length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
               length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
            )/univ_ANC
          matriz_temp[j,16]<-as.numeric(matriz_temp[j-1,16])+
            (length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
               length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
            )/univ_AC
          
          matriz_temp[j,17]<-as.numeric(matriz_temp[j-1,17])+
            (length(tab_ANC$FECHA_NOTIFICA[which(as.POSIXct(tab_ANC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos")])+
                                                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                                                length(tab_ANC$FECHA_CAPACITA[which(as.POSIXct(tab_ANC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_ANC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )/univ_ANC
          matriz_temp[j,18]<-as.numeric(matriz_temp[j-1,18])+
            (length(tab_AC$FECHA_NOTIFICA[which(as.POSIXct(tab_AC$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos")])+
               length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
               length(tab_AC$FECHA_CAPACITA[which(as.POSIXct(tab_AC$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tab_AC$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
            )/univ_AC
          
          # matriz_temp
          
        }
        
      }
      
      #print(paste("Matriz de la Entidad de ",edo,", distrito",dis,"(",d,"/",length(distritos),") lista"))
      lista_matrices[[d]]<-matriz_temp
      names(lista_matrices)[d]<-paste("Distrito_",dis)
      
      lista_dfs[[d]]<-as.data.frame(matriz_temp,row.names = NULL)
      
      write.csv(lista_dfs[[d]],file=paste(edo,"_Distrito_",dis,"_",ubicacion,".csv",sep=""),row.names = FALSE)
      
      print(paste("Matriz de la Entidad de ",edo,", distrito",dis," ",ubicacion,"(",d,"/",length(distritos),") exportada"))
      
      matriz_entidad_temporal<-matrix(rbind(matriz_entidad_temporal,matriz_temp),ncol=21)
      #print(paste("Matriz del distrito ",dis," con ubicacion ",ubicacion," incorporada a la matriz completa de la entidad ",edo))
      
    }
    
    
  }
  
  colnames(matriz_entidad_temporal)<-c("Fechas","Dia_del_periodo",
                                       "Visitas_ANC","Visitas_AC",
                                       "Revisitas_ANC","Revisitas_AC",
                                       "Not_ANC","Not_AC",
                                       "RechazosInme_ANC","RechazosInme_AC",
                                       "Capacitados_ANC","Capacitados_AC",
                                       "Aptos_ANC","Aptos_AC",
                                       "RechazosCapacitacion_ANC","RechazosCapacitacion_AC",
                                       "RechazosTotal_ANC","RechazosTotal_AC",
                                       "Entidad","Distrito","Ubicacion")
  
  matriz_entidad_temporal_2<-matriz_entidad_temporal[-1,]
  lista_entidades_completas[[i]]<-matriz_entidad_temporal_2
  names(lista_entidades_completas)[i]<-paste("Entidad_",edo)
  
  lista_dfs_entidades[[i]]<-as.data.frame(matriz_entidad_temporal_2,row.names = NULL)
  
  write.csv(lista_dfs_entidades[[i]],file=paste("Entidad_",edo,".csv",sep=""),row.names = FALSE)
  print(paste("Fin de Entidad -> ",edo,"... Completamente exportada"))
  
  
  # print("Completado")
 }

print("Completado")
