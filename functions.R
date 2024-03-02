library(ggplot2)
library(ggpubr)
library(RVAideMemoire)
library(nortest)
library(dplyr) 
library(car)
library(questionr)
library(stats)
library(DescTools)
library(epiDisplay)

# ---------------- Leer csv ----------------

read_file = function(datapath,header,sep){
  datos_dieta = read.csv(file = datapath, header = header, sep = sep)
  datos_dieta  
}

# -------------- tipos de datos -----------

data_type = function(df){
  L<-length(df)
  datos<-data.frame(matrix(ncol=L,nrow=0)) #Creo un dataframe con L columnas y 0 filas
  names(datos)<-names(df)  #Pongo las columnas del dataframe input (names(df)) en las columnas del dataframe datos (names(datos))
  for (x in 1:L){
    type=typeof(df[1,x])   #Tipo de datos en la columna
    datos[1,x]=type
  }
  datos
}


# ---------- NAs  ------------
# Armar una funci?n que devuelva en formato dataframe
# la cantidad de NAs presentes por columna 

na_counts = function(df){
  L <- length(df)
  dataframe = data.frame(matrix(ncol=L))
  names(dataframe) = names(df)
  for (x in 1:L){
    count = sum(is.na(df[x]))
    dataframe[1,x] = count
  }
  dataframe
}

# ------- Convertir a factor -----------
convert_to_factor = function(df,colname){
  df[,colname]<-as.factor(df[,colname])
  df
}

# ------- Histograma --------
# 

histogram = function(df,groupcol,colname,inpbins){
  
  if (groupcol == "No agrupar" ){
    
    ggplot(data=df, aes_string(x=colname,)) +
      geom_histogram(aes(y= ..density.. ), position="identity", alpha=0.5,bins=inpbins, color = "yellow")+
      geom_density(alpha=0.6, fill="#E69F00")
  }
  else{
    
    
    p<-ggplot(data=df, aes_string(x=colname, fill=groupcol, color=groupcol)) +
      geom_histogram(aes(y=..density..),position="identity", alpha=0.5, bins=inpbins) +
      
      geom_density(alpha=0.6)+
      
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      
      theme_classic()
    p
    
  }
}

# ------ BOX PLOT --------
my_boxplot = function(df,colname,groupcol){
  if (groupcol == "No agrupar" ){
    
    ggplot(data=df, aes_string(x=colname)) +
      geom_boxplot() + coord_flip()
  }
  else{
    
    
    ggplot(data=df, aes_string(x=colname, fill=groupcol, color=groupcol)) +
      geom_boxplot() + coord_flip()
  }
}

#------- QQ PLOT --------
my_qqplot = function(df,colname,groupcol){
  if (groupcol == "No agrupar" ){
    
    ggplot(data=df, aes_string(sample=colname)) +
      stat_qq() + stat_qq_line()
  }
  else{
    
    
    ggplot(data=df, aes_string(sample=colname, fill=groupcol, color=groupcol)) +
      stat_qq() + stat_qq_line()
  }
}

# ----------- SHAPIRO _ WILK ---------------
shapiro_w = function(df,colname,groupcol){
  variable <- df[,colname]
  if (length(variable) > 5000){
    variable = variable[0:5000]
  }
  
  
  if (groupcol == 'No agrupar'){
    shapiro.test(variable)
  }
  else{
    if (length(df[,groupcol]) > 5000){
      factor = df[,groupcol][0:5000]
    }
    else{
      factor = df[,groupcol] 
    }
    byf.shapiro(formula=variable~factor)
  }
}

# --------- Lilliefors -------------
lilliefors = function(df,colname,groupcol){
  variable <- df[,colname]
  
  if (groupcol == 'No agrupar'){
    lillie.test(variable)
  }
  else{
    factor = df[,groupcol]
    tapply(variable, factor, function(x){lillie.test(x)})
  }
}

# --------- Test de Levene ---------
levene = function(df, colname, groupcol){
  data=df[,colname]
  group = as.factor(df[,groupcol])
  leveneTest(data,group=group, center=mean) 
}


# ------ Test ... ---------

my_test = function(df,colname,groupcol){
  t.test(df[,colname]~as.factor(df[,groupcol]))
}


# --------- Tabla de contingencia ----------
contingency = function(df,colname1,colname2){
  Columna_1 <- df[,colname1]
  Columna_2 <- df[,colname2]
  
  dataframe = data.frame(Columna_1, Columna_2)
  
  names(dataframe) <- c(colname1, colname2)
  
  tabla_contingencia <- table(dataframe)
}

# ---------- Chi 2 --------------
chicuadrado = function(df, colname1, colname2){
  Columna_1 <- df[,colname1]
  Columna_2 <- df[,colname2]
  
  chisq.test(Columna_1,Columna_2)
}

# -------- Valores esperados ----------
chisq_expected = function(df,colname1,colname2){
  
  Columna_1 <- df[,colname1]
  Columna_2 <- df[,colname2]
  chisq <- chisq.test(Columna_1,Columna_2)
  
  esperados <- chisq$expected
}


# ----- Residuos  ---------
chisq_residuals = function(df,colname1,colname2){
  Columna_1 <- df[,colname1]
  Columna_2 <- df[,colname2]
  chisq <- chisq.test(Columna_1,Columna_2)
  
  esperados <- chisq$expected
}

# ------ ODDS RATIO CRUDO ----
odds_r = function(df,colname1,colname2){
  
  Columna_1 = df[, colname1]
  Columna_2 = df[, colname2]
  dataframe = data.frame(Columna_1,Columna_2)
  names(dataframe) = c(colname1, colname2)
  
  tabla_contingencia = table(dataframe)
  odds.ratio(tabla_contingencia)
}

# ---- ODDS RATIO MANTEL HAENZEL ---------
odds_r_mh = function(df,colname1,colname2, groupcol){
  outcome <- df[,colname1]
  exposure <- df[,colname2]
  stratification <- df[,groupcol]
  mhor(outcome, exposure, stratification)
}

# -------- Tabla de contingencia por estratos --------
cont_t_s = function(df,colname1,colname2,groupcol){
  
  tabla_df = table(df)
  tablas_parciales = margin.table(tabla_df,c(colname1,colname2,groupcol))
  tablas_parciales
  
}
# ------ ODDS ratio por estrato -----------
odds_s = function(df,colname1,colname2,groupcol){
  
  
  tabla_df = table(df)
  
  tablas_parciales = margin.table(tabla_df,c(colname1,colname2,groupcol))
  ods_estrat = as.vector(apply(tablas_parciales,3,odds.ratio))
  ods_estrat
}


# ------- Breslow-Day Test ------------
bdt = function(df,colname1,colname2,groupcol){
  tabla_df = table(df)
  tablas_parciales <- margin.table(tabla_df,c(colname1,colname2,groupcol))
  BreslowDayTest(x = tablas_parciales)
}