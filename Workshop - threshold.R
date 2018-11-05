### Explained  in: https://www.bioconductor.org/packages/devel/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html
##  To load:

library("EBImage")
setwd("D:/3D data analysis - interpretation/images")







img =  readImage("cabbage - cam_05.png")
##img =  readImage("tomato_cam8.tif")
##img =  readImage("carrot - cam06.png")






display(img, method="raster")

## threshold using Otsu's method
y = img > otsu(img)
display(y)







a <- imageData(img)
output <- imageData(img)
display(a)




##Threshold and count number of pixels
Count = 0
## Pixel by pixel threshold
for (i in 1:nrow(a)) {
  for(j in 1:ncol(a)) {
    Value<-a[i,j]
    if (Value > 0.2) {
      output[i,j]<-1
      Count = Count + 1
    } else {
      output[i,j]<-0
    }
  }
}
display(output)   
print(Count)








## Crop relevant part of image for further analysis
img_crop = a[820:430, 510:710]
display(img_crop)
output2 <- imageData(img_crop)










Count = 0
## Pixel by pixel threshold
for (i in 1:nrow(img_crop)) {
  for(j in 1:ncol(img_crop)) {
    Value<-img_crop[i,j]
    if (Value > 0.2) {
      output2[i,j]<-1
      Count = Count + 1
    } else {
      output2[i,j]<-0
    }
  }
}
display(output2)   
print(Count)
