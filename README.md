# Movilidad Medellín: Accidentalidad en 2016

El presente trabajo tuvo como objetivo acercarse a los datos de movilidad disponibles en Medellín, para aportar en conocimiento que sirva para tomar acciones que mitiguen los problemas de accidentalidad de la ciudad. Se realizaron análisis descriptivos sobre accidentalidad, y análisis bivariados cruzando variables de interés. Se encuentran resultados respecto a qué días, horas, barrios y comunas se relacionan con cantidades de accidentes y la gravedad de dichos accidentes. Se incluye un analisis con geolocalizacion con la gravedad y cantidad de accidentes en Medellín. En cada sección se dan recomendaciones con base en la información obtenida.

## Para correr los análisis

Los análisis fueron realizados en RStudio 1.1.442, una version de R 3.4.4. El archivo PDF se realizó utilizando RMarkdown en RStudio. Para una visualización óptima se recomienda utilizar las mismas versiones del software, y paquetes actualizados. No se requiere descargar bases de datos, puesto que cada uno de los archivos toma los datos de internet, por tanto se requiere coneccion a Internet para correr todos los archivos.

### Paquetes requeridos

Se utilizaron los paquetes
- ggplot2
- leaflet
- plyr
- lubridate
- reshape2

### Archivos
- Accidents.R: Contiene todos los análisis y procesamiento de datos
- Analisis Accidentalidad.Rmd: Contiene el RMarkdown para crear el PDF de Análisis Accidentalidad
- Geolocalización Accidentalidad: Contiene el RMarkdown para crear una web con los análisis geográficos.

La geolocalización de Accidentes puede encontrarse en este link:
http://rpubs.com/danielm322/372929

## Autor

* **Daniel Montoya Vásquez** - *Estudiante de física* - [GitHub](https://github.com/danielm322)

