library(oro.dicom)
library(tidyverse)
library(plotly)
library(data.table)
library(htmlwidgets)

ref<-1000*data.frame(hmax=(4.64 +0.77),hmin=(4.64 -0.77), mmax=(3.14 + .65), mmin =(3.14- .65 ))

osis<-path.expand("E:/projects/kaggle/osic-pulmonary-fib/osic-pulmonary-fibrosis-progression")
osistrain<-path.expand("E:/projects/kaggle/osic-pulmonary-fib/osic-pulmonary-fibrosis-progression/train")
osistest<-path.expand("E:/projects/kaggle/osic-pulmonary-fib/osic-pulmonary-fibrosis-progression/test")
osisimg<-path.expand("E:/projects/kaggle/osic-pulmonary-fib/img")

setwd(osis)

#invalids<-"ID00011637202177653955184"   #https://www.kaggle.com/rashmibanthia/corrupt-files

trd<-read.csv("train.csv")  %>% dplyr::rename(Week=Weeks)
#subset(Patient!= invalids)
ted<-read.csv("test.csv") %>% dplyr::rename(Week=Weeks)
invalids<-c("ID00011637202177653955184","ID00052637202186188008618", "ID00128637202219474716089","ID00132637202222178761324")

dd<-list.files(osistrain)
dd <- setdiff(dd,invalids)


rd<-function(x,index){
  path<-file.path(osistrain,x)
  s<-readDICOM(path)
  img<-s$img[[index]] 
  
  white<-img < 1
  img[white]=NA
  # useful<- img > 1
  # img1<-img[useful]
  # d<-dim(img)
  # p<-image(1:d[2], 1:d[1],t(img),col=gray(0:256/256),xlab = "",ylab="")
  
  
  pal <- colorRamp(c("blue","white"))
  # axx <- list(
  #   gridcolor='rgb(255, 255, 255)',
  #   zerolinecolor='rgb(255, 255, 255)',
  #   showbackground=TRUE,
  #   backgroundcolor='rgb(230, 230,230)'
  # )
  
  # fig <- plot_ly(z = ~img,scene=paste0("sc",index))
  # fig<- fig %>%
  #   add_surface(
  #     showscale=FALSE
  #     
  #   )
  
  fig <- plot_ly(z = ~img, colors = pal,scene=paste0("sc",index))
  fig<- fig %>%
    add_surface(
      showscale=FALSE,
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      )
    )
  # %>%
  #   layout(
  #     scene = list(
  #       xaxis=axx, yaxis=axx, zaxis=axx,
  #       aspectmode='cube')
  #   )
  return(fig)
}
p1<-rd(x,1)
p2<-rd(x,2)
p<-subplot(p1,p2)

subplot(p1, p2) %>% 
  layout(scene = list(domain=list(x=c(0.1,0.5),y=c(0.1,0.5))),
         scene2 = list(domain=list(x=c(0.5,0.9),y=c(0.5,0.9))))


interv<-1:3 #length(s$img)
plots<-lapply(interv,rd,x=x)
p<-subplot(plots,shareX=T, titleX=F,nrows = 3)  %>%
  layout(title = "Ambient Lighting",
         grid = list(rows = 1, columns = 1,
                     pattern = 'independent'))
hide_legend(p)#lets look into some hints given by graphics
p
d<-dim(allslice$img[[5]])
image(1:d[2], 1:d[1],t(allslice$img[[5]]),col=gray(0:64/64),xlab = "",ylab="")

###coreCT
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html
library(coreCT)
library(magick)
library(raster)
library(doParallel)
library(oro.dicom)

# extract metadata and convert raw values to Hounsfield Units
x<-dd[1]

path<-file.path(osistrain,x)
s<-readDICOM(path)

ct.slope <- unique(extractHeader(s$hdr, "RescaleSlope"))
ct.int   <- unique(extractHeader(s$hdr, "RescaleIntercept")) 
ss <- lapply(s$img, function(x) x*ct.slope + ct.int)
# Use coreCT::conv to convert Hounsfield Units to densities and quantify component masses, volumes
materials <- convert(HU_426, pixelA = voxDims("core_426")$pixelArea.mm2, thickness = voxDims("core_426")$thickness.mm)
plot(-depth ~ peat.cm3, data = materials, xlab = "Peat volume (cm3; per slice)", ylab = "Depth (cm)")



img<-HU_426$`E:/projects/kaggle/osic-pulmonary-fib/osic-pulmonary-fibrosis-progression/train/ID00007637202177411956430/7.dcm`
image(1:d[2], 1:d[1],t(img),col=gray(0:256/256),xlab = "",ylab="")


volumes<-function(x){
  path<-file.path(osistrain,x)
  #s<-readDICOM(path)
  #img<-s$img[[index]] 
  features<-c("air.cm3","water.cm3")
  featuresv<- grep("cm3",names(a) )
  a<-convertDir(path)
  sums<-apply(a,2,sum)[featuresv]
  maxair<-which.max(a$air.cm3 * a$water.cm3)
  
  
}

printimg<-function(x){
    path<-file.path(osistrain,x)
    s<-readDICOM(path)
    #img<-s$img[[index]] 
    
    ct.slope <- unique(extractHeader(s$hdr, "RescaleSlope"))
    ct.int   <- unique(extractHeader(s$hdr, "RescaleIntercept")) 
    Ct.n<- extractHeader(s$hdr, "InstanceNumber")
    ss <- lapply(s$img, function(x) x*ct.slope + ct.int) 
    ff<-function(x){x [ x < 0 ]<- NA;return(x)}
    # trim background
    ss<-  lapply(ss,function(x){x[x< -2000]<-NA;return(x)})
    #reorder:
    fo<-function(i){
      index<-which(Ct.n ==i)
      return(ss[[index]])
    }
    s<-lapply(1:length(ss),fo)
    #for(i in 1:length(s)){plot(raster(s[[i]]),col= hcl.colors(64,"Grays"))}
    
    # now the magic happens
    img <- image_graph(600, 600, res = 96)
    out <- lapply(s, function(x){
      # p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
      #   scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
      #   scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
      # print(p)
      ii<-raster(x)
      plot(ii,col= hcl.colors(64,"Grays"))
      
    })
    dev.off()
    animation <- image_animate(img, fps = 2, optimize = TRUE)
    print(animation)
    image_write(animation,paste0(x,".gif"))
   
    #plot(img,col= hcl.colors(64,"Grays"))
}    
    #############################################################################################################
    #    animating 
    #############################################################################################################

printimg<-function(x){
  path<-file.path(osistrain,x)
  s<-readDICOM(path)
  #img<-s$img[[index]] 
  
  ct.slope <- unique(extractHeader(s$hdr, "RescaleSlope"))
  ct.int   <- unique(extractHeader(s$hdr, "RescaleIntercept")) 
  Ct.n<- extractHeader(s$hdr, "InstanceNumber")
  ss <- lapply(s$img, function(x) x*ct.slope + ct.int) 
  ff<-function(x){x [ x < 0 ]<- NA;return(x)}
  # trim background
  ss<-  lapply(ss,function(x){x[x< -2000]<-NA;return(x)})
  #reorder:
  fo<-function(i){
    index<-which(Ct.n ==i)
    return(ss[[index]])
  }
  s<-lapply(1:length(ss),fo)
  #for(i in 1:length(s)){plot(raster(s[[i]]),col= hcl.colors(64,"Grays"))}
  
  # now the magic happens
  img <- image_graph(600, 600, res = 96)
  out <- lapply(s, function(x){
    # p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    #   scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
    #   scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
    # print(p)
    ii<-raster(x)
    plot(ii,col= hcl.colors(64,"Grays"))
    
  })
  dev.off()
  animation <- image_animate(img, fps = 2, optimize = TRUE)
  print(animation)
  image_write(animation,paste0(x,".gif"))
  
  #plot(img,col= hcl.colors(64,"Grays"))
}    
cores = detectCores()-1
cl = makeCluster(cores)
registerDoParallel(cl)
r<-foreach(dd[1:2], .combine=cbind,.packages=c('coreCT',"oro.dicom","magick","raster")
) %dopar% {
  printimg(dd)
}
stopCluster(cl)


a<-convertDir(path)

HUfreq <- coreHist(path)
names(HUfreq)
HUfreq$splits
#-----------------------------------------------------------------------------------------------------------------------------------------------------------






###RASTER
x<-dd[1];index=5 #( 13.dcm)
path<-file.path(osistrain,x)
s<-readDICOM(path)
ct.slope <- unique(extractHeader(s$hdr, "RescaleSlope"))
ct.int   <- unique(extractHeader(s$hdr, "RescaleIntercept")) 
ss <- lapply(s$img, function(x) x*ct.slope + ct.int)

img<-raster(ss[[index]])
white<-img < -2000
img[white]=NA

plot(img,col= hcl.colors(64,"Grays"))


#####

#https://www.researchgate.net/post/How_to_create_a_simple_project_to_convert_DICOM_images_to_3d_images
#Lets convet to 3d

path<-file.path(osistrain,x)
s<-readDICOM(path)

readF<-function(s){
  et<-function(tag){
    a1<-extractHeader(s$hdr,tag ,numeric=FALSE) %>%
      as.list %>%
      str_split(pattern =" ")
    a2<-lapply(a1,as.numeric)
    return(a2)
  }
  imagep<-et("ImagePositionPatient")
  pixels<-et("PixelSpacing")
  vec<-et("ImageOrientationPatient")
  rowx<-lapply(vec,function (x)x[1:3]);colx<-lapply(vec,function (x)x[4:6])
  px<-map(pixels,1);py<-map(pixels,2)
}
zf<-function(x,y,i){
  d<-imagep[[i]] + rowx[[i]]* px[[i]] *x  + colx[[i]] * py[[i]] * y
  return(d)
}
# 
# a<-sapply(1:512,function(x,y){expand.grid(x,513-x)},simplify = F)
# b<-lapply(a,zf,i=1)
# 
# # a<-pmap(.l=(..1=1:512,..2=1:512,..3=1:length(s$hdr)),.f=zf)
# a<-pmap(.l=list(..1=1:512,..2=1:512,..3=1:length(s$hdr)),.f=zf)
# a<-mapply(zf,x=list(1:512),y=list(1:512), MoreArgs = list(i=1:30))
# 
# # a<-map2(.x=list(1:512),.y=list(1:512),.f=zf,i=1)
# nf<-function(i){
#   r<-expand.grid(x=1:512,y=1:512,i=(1:30))
#   for(x in 1:512){
#     for(y in 1:512){
#       r(x,y,i)<-zf(x,y,i)
#    }
#   }
#   return(r)
# }

r<-expand.grid(x=1:512,y=1:512,i=(1:30))

# lapply(1:length(s$hdr),nf)
et<-function(tag){
  a1<-extractHeader(s$hdr,tag ,numeric=FALSE) %>%
    as.list %>%
    str_split(pattern =" ")
  a2<-lapply(a1,as.numeric)
  return(a2)
}
imagep<-et("ImagePositionPatient")
pixels<-et("PixelSpacing")
vec<-et("ImageOrientationPatient")
rowx<-lapply(vec,function (x)x[1:3]);colx<-lapply(vec,function (x)x[4:6])
px<-map(pixels,1);py<-map(pixels,2)
a<-pmap(.l=r,.f=zf)

b<-do.call("rbind",a)
bd<-as.data.frame(b);names(bd)<-c("x","y","z")
pal <- colorRamp(c("black","white"))

fig <- bd%>%
  plot_ly(
  type = 'streamtube',
  x = ~x,
  y = ~y,
  z = ~z,

  colors = pal) %>%
  add_surface(
    showscale=FALSE,
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  )
#https://ropensci.org/technotes/2017/11/02/image-convolve/

a<-with( bd[1:10,], mesh(x,y,z))
x<-a$x
y<-a$y
z<-bd$z[1:10]


surf3D(x, y, z, colvar = y, colkey = FALSE, shade = 0.5,box = FALSE, theta = 60)