ll <- list.files(path = "visualizations", patt='*.png')

ll
library(png)
library(grid)
library(glue)

imgs <- lapply(ll,function(x){
  img <- as.raster(readPNG(glue("visualizations/", x, sep = "")))
  ## get the file name
  x.name <- gsub('(.*).png','\\1',x)
  ## new device for new image version
  png(file = glue("visualizations/", paste(x.name,'_modified','.png',sep=''), sep = ""))
  grid.raster(img)
  ## here I add title
  grid.text(label = x.name,x=0.5,y=0.9,gp=gpar(cex=2))
  dev.off()
  
})
