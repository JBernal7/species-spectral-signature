library(raster)
path_folder = "E:/ARTICULOS/R/4.FIRMAS_ESPECTRALES/S2B_MSIL1C_20190514_bandas/"
jp2_files <- list.files(path_folder, full.names = T)
pattern <- c("B02", "B03", "B04", "B05", "B06", "B07","B08", "B11", "B12")
stack_files <- unique(grep(paste(pattern,collapse="|"), jp2_files, value=TRUE))
sen2 <- stack(stack_files)
df_clic <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_clic) <- c("x","y")
plotRGB(brick(jp2_files[14]), r=1, g=2, b=3)
for (c in 1:10){
  df_clic <- rbind(df_clic,as.data.frame(click()))
}
spdf <- SpatialPointsDataFrame(coords = df_clic[,c(1,2)], data = df_clic, proj4string = crs(sen2))
text(spdf,1:10,cex=1.5)
ext_sen <- as.data.frame(t(extract(sen2, spdf)))
print(ext_sen)
ext_sen <- cbind(ext_sen,c(492,560,665,705,740,783,842,1610,2190))
rownames(ext_sen) <- c("blue","green","red","RE1","RE2","RE3","NIR","SWIR1","SWIR2") 
colnames(ext_sen) <- c("1","2","3","4","5","6","7","8","9","10","bands") 
library(reshape)
ext_melt <- melt(ext_sen,id=c("bands"))
ext_melt$value <- ext_melt$value/10000*100
library(randomcoloR)
palette <- distinctColorPalette(10)
library(ggplot2)
ggplot(data = ext_melt, aes(x=bands, y=value, color=variable)) + 
  geom_line(size=1.3,  alpha=0.6) +
  geom_point(shape=19, size=2) +
  scale_color_manual(values = palette) +
  scale_x_sqrt(breaks=c(492,560,665,705,740,783,842,1610,2190)) +
  ylim(0, 100) +
  annotate("text", x = c(492,560,665,705,740,783,842,1610,2190), y=0, 
           label = c("blue","green","red","RE1","RE2","RE3","NIR","SWIR1","SWIR2"), size=2.5) +
  theme_bw(base_size = 10) + 
  labs(title="Firmas espectrales Sentinel-2", x="Longitud de onda (μm)", y="Reflectividad (%)")