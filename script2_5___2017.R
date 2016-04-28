# setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016")
#Código para generar archivos de las medias de las variables a las que se les calculará sus umbrales
#y que serán los archivos donde se harán las tablas de umbrales por cada nivel de confianza

setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis")

tabla_nombres<-data.frame(NOMBRE_ESTADO=c("coahuila","edomex","nayarit","veracruz"))
lista_entidades_completas<-list()
lista_dfs_entidades<-list()

#dir.create("para_resumenes_maya_y_jaqui")
#con el siguiente for iniciamos el proceso para cada entidad que debemos de tener en un archi
for (i in 1:length(unique(tabla_nombres$NOMBRE_ESTADO))) {
  #i<-1
  setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis")
  edo<-as.character(tabla_nombres$NOMBRE_ESTADO)[i]
  entidad_temp<-read.csv(paste("fechas_",edo,"_ph2.csv",sep=""),header=TRUE)
  
  
  
  
  setwd("~/Google Drive/alfredo/PE_2014-2015/Capacitacion_2014-2015/2016/2017/nuevas_bases/archivos_pruebas_hipotesis/ph2/para_resumenes_maya_y_jaqui")
  
  #Aqui se guardara la info de todos los distritos de la entidad
  matriz_entidad_temporal<-matrix(ncol=20)
  
  
  #Creamos los objetos de lista de matrices donde guardaremos cada distrito, lista de df para exportarlos
  #lista_matrices<-list()
  #lista_dfs<-list()
  
  #antes de dividir por ubicacion, extraigamos las fechas que se utilizaron en la entidad
  todas<-c(as.character(entidad_temp$FECHA_VISITA),as.character(entidad_temp$FECHA_NOTIFICA),as.character(entidad_temp$FECHA_CAPACITA))
  todas2<-as.POSIXct(as.character(todas), format = "%m/%d/%y", tz = "GMT")
  
  todas3<-unique(todas2)
  todas4<-todas3[!is.na(todas3)]
  
  indices_ordenamiento_unicas_todas_fechas<-order(todas4, decreasing = FALSE )
  final_fechas<-todas4[indices_ordenamiento_unicas_todas_fechas]
  

for(AC in as.character(unique(entidad_temp$ausentismo.critico.en.esa.seccion))){
  print(paste("Empezando Ausentismo Critico -> ",AC,sep=""))
  entidad_temp_AC<-subset(entidad_temp,ausentismo.critico.en.esa.seccion==AC)
  

  #ahora dividiremos en casos de urbana y no urbana
  # for(ubicacion in as.character(unique(entidad_temp_AC$TIPO_SECCION_DEOE))){
    
    #print(paste("Empezando AC: ",AC," y ubicacion -> ",ubicacion,sep=""))
    
    
    # ubicacion<-as.character(unique(entidad_temp$TIPO_SECCION_DEOE[1]))
    #Aqui guardamos el subset de la entidad-temporal con la ubicacion-temporal
    tabla_ubic_temp<-entidad_temp_AC
    #tabla_ubic_temp<-entidad_temp
    
    #Aqui guardamos los distritos que tiene esa entidad-temporal en el tipo de ubicacion-temporal.
    #Sobre estos se correrá el "for" con el que se harán las tablas por distrito
    # distritos<-sort(unique(tabla_ubic_temp$ID_DISTRITO))
    
    #inicializamos la matriz en la que guardaremos la informacion por distrito en la ubicacion-temporal de la entidad-temporal
    matriz_temp<-matrix(ncol=20, nrow=length(final_fechas)) #Agregue tres columnas mas (de 16 a 19) por Ent,Dis,ubic
    colnames(matriz_temp)<-c("Fechas","Dia_del_periodo",
                             "VISITAS","Por_VISITAS",
                             "REVISITAS","Por_REVISITAS",
                             "NOTIFICACIONES","Por_NOTIFICACIONES",
                             "RECHAZOS_INMEDIATOS","Por_RECHAZOS_INMEDIATOS",
                             "CAPACITADOS","Por_CAPACITADOS",
                             "APTOS","Por_APTOS",
                             "RECHAZOS_CAPACITACION","Por_RECHAZOS_CAPACITACION",
                             "RECHAZOS_TOTALES","Por_RECHAZOS_TOTALES",
                             "Entidad","Ausentismo_Critico") #Los tres campos que se agregaron
    
#     tab_ANC_0<-subset(tabla_ubic_temp,ausentismo.critico.en.esa.seccion!="VERDADERO")
#     tab_AC_0<-subset(tabla_ubic_temp,ausentismo.critico.en.esa.seccion=="VERDADERO")
     
#    for(d in 1:length(distritos)){
      #d<-distritos[1]
      # dis<-distritos[d]
      
      #limpiamos la matriz
      matriz_temp[][]<-0
      
      #llenamos los campos 1,17,18,19 que son fijos para todas las fechas y que son: fechas, edo, distrito y la ubic
      matriz_temp[,1]<-as.character(final_fechas) #no se utilizá este vector para operaciones, solo para que aparezca en la tabla final
      matriz_temp[,19]<-edo
      # matriz_temp[,20]<-ubicacion
      matriz_temp[,20]<-AC
      
      #Hacemos el vector de las diferencias de dias con respecto al dia 1 de nuestro objeto final_fechas
      dias_diferencia<-data.frame(Diferencia=as.numeric(julian(final_fechas,origin = final_fechas[1]))) #las diferencias en días con respecto al primero día del período
      
      #Hacemos un subset por tipo de Ausentismo con el que se trabajará en el "for" por fecha que sigue a continuacion
#       tab_ANC<-subset(tab_ANC_0,ID_DISTRITO==dis)
#       tab_AC<-subset(tab_AC_0,ID_DISTRITO==dis)
#       
      #Establecemos los universos (totales) de inscaulados en el distrito dis: uno para ANC y otro para AC
#       univ_ANC<-nrow(tab_ANC)
#       univ_AC<-nrow(tab_AC)
#       
      universo<-nrow(tabla_ubic_temp)
      #Corremos con todas las fechas
      for(j in 1:length(final_fechas)){
        #j<-1
        matriz_temp[j,2]<-1+dias_diferencia$Diferencia[j]
        
        if(j==1){
          
          
          matriz_temp[j,3]<-length(tabla_ubic_temp$FECHA_VISITA[which(as.POSIXct(tabla_ubic_temp$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])])
          matriz_temp[j,4]<-as.numeric(matriz_temp[j,3])/universo
          
          matriz_temp[j,5]<-length(tabla_ubic_temp$FECHA_VISITA[which(as.POSIXct(tabla_ubic_temp$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j]& tabla_ubic_temp$REVISITA=="Si")])
          matriz_temp[j,6]<-as.numeric(matriz_temp[j,5])/universo
          
          matriz_temp[j,7]<-length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])])
          matriz_temp[j,8]<-as.numeric(matriz_temp[j,7])/universo
          
          matriz_temp[j,9]<-length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos")])
          matriz_temp[j,10]<-as.numeric(matriz_temp[j,9])/universo
          
          matriz_temp[j,11]<-(length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
                                length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
                                length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])
                              )
          
          matriz_temp[j,12]<-as.numeric(matriz_temp[j,11])/universo
          
          matriz_temp[j,13]<-length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])
          matriz_temp[j,14]<-as.numeric(matriz_temp[j,13])/universo
          
          matriz_temp[j,15]<-(length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )
          
          matriz_temp[j,16]<-as.numeric(matriz_temp[j,15])/universo
          
          matriz_temp[j,17]<-(length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos")])+
                                length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
                                length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
          )
          matriz_temp[j,18]<-as.numeric(matriz_temp[j,17])/universo
          
          
        }else{
#                 j<-j+1
#                 matriz_temp[j,2]<-1+dias_diferencia$Diferencia[j]
#           
          matriz_temp[j,3]<-as.numeric(matriz_temp[j-1,3])+length(tabla_ubic_temp$FECHA_VISITA[which(as.POSIXct(tabla_ubic_temp$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])])
          matriz_temp[j,4]<-as.numeric(matriz_temp[j,3])/universo
          
          matriz_temp[j,5]<-as.numeric(matriz_temp[j-1,5])+length(tabla_ubic_temp$FECHA_VISITA[which(as.POSIXct(tabla_ubic_temp$FECHA_VISITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j]& tabla_ubic_temp$REVISITA=="Si")])
          matriz_temp[j,6]<-as.numeric(matriz_temp[j,5])/universo
          
          matriz_temp[j,7]<-as.numeric(matriz_temp[j-1,7])+length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j])])
          matriz_temp[j,8]<-as.numeric(matriz_temp[j,7])/universo
          
          matriz_temp[j,9]<-as.numeric(matriz_temp[j-1,9])+length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos")])
          matriz_temp[j,10]<-as.numeric(matriz_temp[j,9])/universo
          
          matriz_temp[j,11]<-as.numeric(matriz_temp[j-1,11])+
            (length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])+
               length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos capacitados NO aptos")])+
               length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")]))
          
          matriz_temp[j,12]<-as.numeric(matriz_temp[j,11])/universo
          
          matriz_temp[j,13]<-as.numeric(matriz_temp[j-1,13])+length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Ciudadanos aptos")])
          matriz_temp[j,14]<-as.numeric(matriz_temp[j,13])/universo
          
          matriz_temp[j,15]<-as.numeric(matriz_temp[j-1,15])+
            (length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
               length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
            )
          matriz_temp[j,16]<-as.numeric(matriz_temp[j,15])/universo
          
          matriz_temp[j,17]<-as.numeric(matriz_temp[j-1,17])+
            (length(tabla_ubic_temp$FECHA_NOTIFICA[which(as.POSIXct(tabla_ubic_temp$FECHA_NOTIFICA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos")])+
               length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion capacitados")])+
               length(tabla_ubic_temp$FECHA_CAPACITA[which(as.POSIXct(tabla_ubic_temp$FECHA_CAPACITA, format = "%m/%d/%y", tz = "GMT")==final_fechas[j] & tabla_ubic_temp$ID_ESTATUS_INSACULADO=="Rechazos durante la capacitacion No capacitados")])
            )
          matriz_temp[j,18]<-as.numeric(matriz_temp[j,17])/universo
          
          # matriz_temp
          
        }
      print(paste("dia ",j," listo",sep=""))  
      }
      
      #print(paste("Matriz de la Entidad de ",edo,", distrito",dis,"(",d,"/",length(distritos),") lista"))
      # lista_matrices[[d]]<-matriz_temp
      # names(lista_matrices)[d]<-paste("Distrito_",dis)
      
      # lista_dfs[[d]]<-as.data.frame(matriz_temp,row.names = NULL)
      
      # write.csv(lista_dfs[[d]],file=paste(edo,"_Distrito_",dis,"_",ubicacion,".csv",sep=""),row.names = FALSE)
      
      # print(paste("Matriz de la Entidad de ",edo,", distrito",dis," ",ubicacion,"(",d,"/",length(distritos),") exportada"))
#       
      matriz_entidad_temporal<-matrix(rbind(matriz_entidad_temporal,matriz_temp),ncol=20)
#       #print(paste("Matriz del distrito ",dis," con ubicacion ",ubicacion," incorporada a la matriz completa de la entidad ",edo))
#       
#   #  }
#     
#     

#   
#   
#   lista_dfs_entidades[[i]]<-as.data.frame(matriz_temp,row.names = NULL)
# 
#   lista_entidades_completas[[i]]<-matriz_temp
#   names(lista_entidades_completas)[i]<-paste("Entidad_",edo)
#       
  write.csv(as.data.frame(matriz_temp,row.names = NULL),file=paste("Entidad_",edo,"_",AC,".csv",sep=""),row.names = FALSE)
  print(paste("Entidad_",edo,"_",AC,".csv","... Completamente exportada",sep=""))
  # print(paste("Terminando ubicacion -> ",ubicacion," con AC: ",AC,sep=""))
  
  #}
  print(paste("Terminando Ausentismo Crítico -> ",AC,sep=""))
  
  
}
   
  colnames(matriz_entidad_temporal)<-c("Fechas","Dia_del_periodo",
                                       "VISITAS","Por_VISITAS",
                                       "REVISITAS","Por_REVISITAS",
                                       "NOTIFICACIONES","Por_NOTIFICACIONES",
                                       "RECHAZOS_INMEDIATOS","Por_RECHAZOS_INMEDIATOS",
                                       "CAPACITADOS","Por_CAPACITADOS",
                                       "APTOS","Por_APTOS",
                                       "RECHAZOS_CAPACITACION","Por_RECHAZOS_CAPACITACION",
                                       "RECHAZOS_TOTALES","Por_RECHAZOS_TOTALES",
                                       "Entidad","Ausentismo_Critico") 
  
  matriz_entidad_temporal_2<-matriz_entidad_temporal[-1,]
  
  write.csv(as.data.frame(matriz_entidad_temporal_2,row.names = NULL),file=paste(edo,"_Por_tipos_Ausentismo_y_Sin_distinguir_en_Ubicaciones_2_5R.csv",sep=""),row.names = FALSE)
  
  print(paste(edo," Completado"))
}



print("Completado")
