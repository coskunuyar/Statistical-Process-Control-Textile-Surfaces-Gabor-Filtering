library(jpeg)
library(wvtool)
library("matrixStats")
imgActual <- readJPEG("C:/Fabric9-ConvertImage.jpg")
img1 <- readJPEG("C:/Fabric9-ConvertImage.jpg")

gaborFilter <- gabor.filter(x=imgActual, lamda=15, theta=45, bw=5, phi=0, asp=0.8, disp=TRUE)
imgGabor <-gaborFilter$filtered_img

# Column Gabor
mean_col_Gabor <- colMeans(imgGabor)
std_col_Gabor <- colSds(imgGabor)
control_limits_col_Gabor <- matrix(,512,3)
control_limits_col_Gabor[1:512,1] = mean_col_Gabor[1:512]-(3)*std_col_Gabor[1:512]
control_limits_col_Gabor[1:512,2] = mean_col_Gabor[1:512]
control_limits_col_Gabor[1:512,3] = mean_col_Gabor[1:512]+(3)*std_col_Gabor[1:512]
for(i in 1:512){
  for(k in 1:512){
    if(imgGabor[i,k] < control_limits_col_Gabor[i,1] | imgGabor[i,k] > control_limits_col_Gabor[i,3] ){
      img1[i,k] = 0;
    } 
  }
}
# Row Gabor
mean_row_Gabor <- rowMeans(imgGabor)
std_row_Gabor <- rowSds(imgGabor)
control_limits_row_Gabor <- matrix(,512,3)
control_limits_row_Gabor[1:512,1] = mean_row_Gabor[1:512]-(3)*std_row_Gabor[1:512]
control_limits_row_Gabor[1:512,2] = mean_row_Gabor[1:512]
control_limits_row_Gabor[1:512,3] = mean_row_Gabor[1:512]+(3)*std_row_Gabor[1:512]
for(i in 1:512){
  for(k in 1:512){
    if(imgGabor[i,k] < control_limits_row_Gabor[i,1] | imgGabor[i,k] > control_limits_row_Gabor[i,3] ){
      img1[i,k] = 0;
    } 
  }
}

require(grDevices)
op <- par(bg = "magenta")
plot(c(100, 1200), c(100, 710), type = "n", xlab = "", ylab = "")
rasterImage(imgActual, 100, 105, 612, 617, interpolate = FALSE)
rasterImage(img1, 652, 105, 1164, 617, interpolate = FALSE)
par(op)