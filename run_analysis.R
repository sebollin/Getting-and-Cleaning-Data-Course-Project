setwd("D:/Seba/Formación/Data Science Specialization - Johns Hopkins University/Getting and Cleaning Data/proyecto")
#PARTE 1 - Combina los datos del training y los datos de prueba (test) para 
#crear un unico conjunto de datos.

#Descargando los datos

library(data.table)
url = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
        download.file(url,'./UCI HAR Dataset.zip', mode = 'wb')
        unzip("UCI HAR Dataset.zip", exdir = '.')
}

#Leer y convertir los datos a DF

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(subject_train, y_train, X_train)
names(data.train) <- c(c('subject', 'activity'), features)

X_train <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_train <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

#Combino "X_train", "y_train", "subject_train", "X_test", "y_test" y "subject_test" en un nuevo DF
#llamado "data.test", y renombro las variables/columnas del nuevo DF.
data.test <-  data.frame(subject_train, y_train, X_train)
names(data.test) <- c(c('subject', 'activity'), features)

#Mergeo todas las observaciones, es decir pego ambos conjuntos de datos para
# crear una sola base de datos. Pego las filas utilizando "rbind"
data.all <- rbind(data.train, data.test)
#FINAL DE LA PARTE 1

#PARTE 2 - Extrae solo las mediciones de la media y la desviación estándar 
# para cada medición

col.select <- grep('mean|std', features)
data.sub <- data.all[,c(1,2,col.select + 2)]
#Se han seleccionado todas las columnas que representan la media o 
# la  desviación mestándar
#FINAL DE LA PARTE 2

#PARTE 3 - Utilizar nombres de actividades descriptivas para nombrar las
# actividades en el conjunto de datos

activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
#borro la primera columna pues no me sirve
activity.labels <- as.character(activity.labels[,2])

data.sub$activity <- activity.labels[data.sub$activity]
#FINAL DE LA PARTE 3

#PARTE 4 - Etiquetar apropiadamente el conjunto de datos 
# con nombres de variables descriptivos
name.new <- names(data.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.sub) <- name.new
#FINAL DE LA PARTE 4

#PARTE 5 - A partir del conjunto de datos de la parte 4, crear un segundo 
# conjunto de datos, independiente y ordenado, con el promedio de cada 
# variable para cada actividad y cada tema

data.tidy <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, 
                                                  subject = data.sub$subject),
                                                        FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)
data.tidy
# "data.tidy" es el conjunto ordenado de datos.
#FINAL DE LA PARTE 5