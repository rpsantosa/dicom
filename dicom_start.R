#Digital Imaging and Communications in Medicine
## to make submissions, have to take the Sample submission.
#submission    
infos<-trd %>%
  select(!c(FVC,Percent,Weeks)) %>%
  unique()

st<-sb %>%
  separate(Patient_Week, sep="_",c("Patient","Weeks")) %>%
  mutate(Patient_Week=sb$Patient_Week) %>%
  merge(infos, by="Patient") %>%
  mutate(Weeks=as.numeric(Weeks)) %>%
  arrange(Weeks)


#__________________________________________________________________________
library(oro.dicom)
library(oro.nifti)
detach("package:RNifit", unload = T)
dcmImages <- readDICOM("/dev/DICOMRT2", verbose = TRUE,
                       recursive = FALSE, exclude = "sql")

fname <- system.file(file.path("dcm", "Abdo.dcm"), package="oro.dicom")
abdo <- readDICOMFile(fname)
names(abdo)


#-----------------
train<-path.expand("E:/Downloads/osic-pulmonary-fibrosis-progression/train")
traindata<-readDICOM(train, exclude=)

unlist(lapply(a, length))
#reading the header
tail(a$hdr[[1]])
#in data.frame
b<-dicomTable(a$hdr)

seriesTime <- extractHeader(hk40$hdr, "SeriesTime", numeric=FALSE)
head(extractHeader(hk40$hdr, "SliceLocation"))

image(t(a$img[[1]]), col=grey(0:64/64), axes=FALSE, xlab="", ylab="")

# unique(a$hdr, "Manufacturer", numeric=FALSE)
table(extractHeader(a$hdr, "Manufacturer", numeric=FALSE))

# GE MEDICAL SYSTEMS           PACSGEAR            Philips            SIEMENS 
# 473                 28                698                 62 
# only fields with info
b[1,!is.na(b[1,])]
unlist(b[1,!is.na(b[1,])])
unique(b["0008-0008-ImageType"])
unique(dcm.info["0018-1130-TableHeight"])
#_____________________________________________________d
#exploring data
pathkaggle<-path.expand("E:/Downloads/osic-pulmonary-fibrosis-progression")
setwd(pathkaggle)

trd<-read.csv("train.csv")
ted<-read.csv("test.csv")







library(gifski)
png_files <- list.files("path/to/your/pngs/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", width = 800, height = 600, delay = 1)


list.files(path='/$PATH/', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current dir



library(dcmtk)
file<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264/1.dcm")
png_file = dcmj2pnm(file)
print(png_file)
print(file.exists(png_file))
print(normalizePath(png_file))
img = png::readPNG(png_file)
plot(1:2, type='n')
image(img)



library(gifski)
png_files <- list.files("path/to/your/pngs/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", width = 800, height = 600, delay = 1)


#____________________________________________________________
#dicom course
p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264/1.dcm")
slice<-readDICOM(p)

d<-dim(t(slice$img[[1]]))

image(1:d[1], 1:d[2], t(slice$img[[1]]),col=gray(0:64/64))

image(200:700, 400:800, t(slice$img[[1]]),col=gray(0:64/64))



p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/train/ID00283637202278714365037")
p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264")

allslice<-readDICOM(p)


p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264/1.dcm")
slice<-readDICOM(p)

nii<-dicom2nifti(allslice)
#__________________________________________________
#another try
library(tractor.base)
slice<-readDicomFile(p)
showImagesInViewer(slice)




fig <- plot_ly(x = slice$img[[1]][,1], y = slice$img[[1]][,2]) 

fig <- fig %>%
  add_trace(type='histogram2dcontour')


#__________________

library(plotly)
library(oro.dicom)
#library(RNifti)
p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264")
p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/train/ID00014637202177757139317/")

allslice<-readDICOM(p)
hk40n <- dicom2nifti(allslice)
# p<-file.path("E:/Downloads/osic-pulmonary-fibrosis-progression/test/ID00419637202311204720264/1.dcm")
# slice<-readDICOM(p)
# d<-dim(t(slice$img[[1]]))
# 
# image(1:d[1], 1:d[2], t(slice$img[[1]]),col=gray(0:64/64))
length(allslice$hdr)
hist(allslice$img[[5]])
d<-dim(allslice$img[[5]])
image(1:d[2], 1:d[1],t(allslice$img[[5]]),col=gray(0:64/64),xlab = "",ylab="")
pal <- colorRamp(c("black","white"))

fig<-plot_ly(z = ~ allslice$img[[5]],type =  "contour" , colors = pal) 

orca(fig, "surface-plot.png")

#remove the black from square
hist(allslice$img[[5]])
slicex<-allslice
white<-slicex$img[[5]] < -1000
slicex$img[[5]][white]=0

#crop the highest values ( bones hipersinal)
bones<-slicex$img[[5]] > 100
slicex$img[[5]][bones]=100


pal <- colorRamp(c("black","white"))
fig <- plot_ly(z = ~slicex$img[[5]], colors = pal)
fig <- fig %>% add_surface()

fig

extractHeader(allslice$hdr,"SliceLocation")
extractHeader(allslice$hdr,"ImagePositionPatient",numeric = F)

# ImagePositionPatient
# devtools::install_github("jonclayden/RNifti")


fig <- plot_ly(z = ~allslice$img[[5]], colors = pal) %>%
add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig

#_________________________________________________________

# pp<-c("randomForest", "caret", "e1071", "mlbench"); install.packages((pp))


library(randomForest)
library(mlbench)
library(caret)
library(e1071)

# # Load Dataset
# data(Sonar)



#Helo World
library(oro.dicom)
library(tidyr)
library(plotly)

pathkaggle<-path.expand("E:/projects/kaggle/osic-pulmonary-fib/osic-pulmonary-fibrosis-progression/")
setwd(pathkaggle)

trd<-read.csv("train.csv") 
ted<-read.csv("test.csv")
sb<-read.csv("sample_submission.csv")

# x<-trd %>%
#   select(!c(FVC,Percent))
#     
# y<-trd %>% select(FVC)   

dataset<-trd %>%
  select(!c(Percent))

#submission    
infos<-trd %>%
  select(!c(FVC,Percent,Weeks)) %>%
  unique()

st<-sb %>%
  separate(Patient_Week, sep="_",c("Patient","Weeks")) %>%
  mutate(Patient_Week=sb$Patient_Week) %>%
#   mutate(Patient=strsplit(Patient_Week,"_")[[1]]) %>%
#   mutate(Week=strsplit(Patient_Week,"_")[[1]][2]) %>%
  merge(infos, by="Patient")
#test
test<-ted %>%
  select(!FVC)
#grab the infos to the train set


#parallel to speed up____________________________________________________________________________________________________________________
library(doParallel)
cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid',
                        allowParallel = T)
#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (10:20)) 

rf_gridsearch <- train(FVC~., 
                       data=dataset,
                       method = 'rf',
                       tuneGrid = tunegrid)
stopCluster(cl)
plot(rf_gridsearch)
#preparing the test set for the output
summary(dataset$Weeks)
res<-predict(rf_gridsearch,ted[,-c(3,4)])
ted$FVCest<-res

## another modle to get sigma_____________________________________________________________________________________________________________________
library(randomForest)
library(doParallel)
library(caret)

invalids<-"ID00011637202177653955184"   #https://www.kaggle.com/rashmibanthia/corrupt-files

dd<-list.dirs()

# r <- foreach(icount(trials), .combine=cbind) %dopar% {
#   + ind <- sample(100, 100, replace=TRUE)
#   + result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
#   + coefficients(result1)
ids<-unique(trd$Patient)
strf<-st %>%
  select(!c(Patient_Week,FVC,Confidence))
pweek<-preProcess(trd[,-c(3,4)], method = c("range"), rangeBounds = c(0,1))
trdp<-cbind(predict(pweek,trd[,-c(3,4)]),FVC=trd$FVC)
tedp<-cbind(predict(pweek,ted[,-c(3,4)]),FVC=ted$FVC) 


result <- rfcv( trdp[,-ncol(trdp)]  , trdp$FVC    , cv.fold=10)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2)) #5

result <- replicate(5,  rfcv( trdp[,-ncol(trdp)]  , trdp$FVC    , cv.fold=10), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")


ff<-function(strf,nt){ 
  #j <- 1+ i %% 3
  # data<-ted[trd$Patient %in% id ,-c(1,3,4)]
  rr<-randomForest (FVC ~.,data=trdp, importance=T,ntree=nt,corr.bias=T ,mtry=2)
  a<-predict(rr,strf)
  return(a)
}

a<-Sys.time()
cores = detectCores()-1
cl = makeCluster(cores)
registerDoParallel(cl)
r<-foreach(nt=seq(50,1000,50), .combine=cbind, .multicombine=TRUE,.packages='randomForest'
        ) %dopar% {
          a<-replicate(10,ff(strf=tedp[,-ncol(tedp)],nt))
        }
stopCluster(cl)
Sys.time()-a
          
          







#system.time(r<-sapply(seq(50,1000,50),ff,strf=tedp[,-ncol(tedp)]))


sigma<-apply(r,1,sd)
FVCp<-apply(r,1,mean)
result<-sum(perf(r,tedp))





perf<-function(r,testset){
  sigma<-apply(r,1,sd)
  FVCp<-apply(r,1,mean)
  FVC<-testset$FVC
  sigmac<- sapply(sigma,FUN=function(x) max(x,70))
  delta<-sapply(FVCp,FUN=function(x)min((FVC-x),1000))
  metric<-  -sqrt(2)* delta/sigmac - log(sqrt(2)*  sigmac)
  return(metric)
}


testset<- st %>%
  select(!c(,Confidence,FVC))%>%
  mutate(Weeks=as.numeric(Weeks)) 
testset<-predict(pweek,testset)


r<-sapply(1:100,ff,strf=testset)
sigma<-apply(r,1,sd)
FVCp<-apply(r,1,mean)
submission<- st %>%
  cbind(FVC=FVCp) %>%
  confidence=sigma%>%
  select(c(Patient_Week,FVC,Confidence))



# 1        2        3        4        5 
# 357.7017 329.8575 357.7017 369.1481 444.0918
# a<-tuneRF(x=trd[,-4],y=trd[,4])
############end sigma model





#________________________________________________________
#taking a look on pacints

library(oro.dicom)
library(tidyverse)
library(plotly)
library(data.table)
library(oro.dicom)
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

    

dd<-list.files(osistrain)
dd <- setdiff(dd,invalids)

getNumberSlices<-function(x){
  return(length(list.files(x)))
}
ns<-sapply(dd,getNumberSlices)

# fvcn<-data.table(table(trd$Patient))
# maxfvc<-fvcn[N==max(N),]
fvcn<-data.frame(table(trd$Patient)) %>%
  subset(Freq==max(Freq)) %>% 
  select(Var1)
toplot<- trd %>%
  subset(Patient %in% fvcn$Var1)

plot_ly(toplot, x=~Week, y=~FVC, type = "scatter", mode= "lines+markers", color = ~ Patient)
#now with age
plot_ly(toplot, x=~Age, y=~FVC, type = "scatter", mode= "lines+markers", color = ~ Patient) #there is no relation with Age

# lets plot the first and last weeks

toplot<- trd %>%
  subset(Weeks > 70)

plot_ly(toplot, x=~Weeks, y=~FVC, type = "scatter", mode= "lines+markers", color = ~ Patient)


#the dicom data had the weeks? 

tofile<-readDICOM(oasistest)

hdrs<-dicomTable(tofile$hdr)
num<-extractHeader(tofile$hd, "InstanceNumber" )


# how manu slices in each directory?
getNumberSlices<-function(x){
  fp<-file.path(osistrain,x)
  return(length(list.files(fp)))
}
ns<-sapply(dd,getNumberSlices)
hist(ns)


#lets see this one with more than 1000
x<-dd[which.max(ns)]
fp<-file.path(osistrain,x)
list.files(fp)  # rip my memory 1018

# lest implement a model that fits a lm or something for each pacient
#once the FVC is more imporant predictor, and changes fore each pacient

#lets organize the data About Sex, SmokingStatus, Age

hline <- function(y = 0, color = "blue") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}



#fisrt: lets plot
lmf<-function(x){
  tolm<-trd %>%
        subset(Patient == x) #%>% 
       # mutate(FVC=FVC + 100*rnorm(length(FVC)))

  fit<-lm(FVC ~ Weeks, tolm)
  tolm %>%
    plot_ly(x = ~ Weeks) %>% 
    add_lines(x = ~ Weeks, y= ~ FVC) %>%
    add_markers(y= ~ FVC) %>%
    add_lines(x = ~  Weeks, y=fitted(fit)) %>%
    #ifelse ( tolm$Sex[1]=="Male",
     # add_segments(y = ref$hmax, color="red")  %>%add_segments(x = 4, xend = 4, y = min(Weeks), yend = max(Weeks))
   #   add_segments(y = ref$hmin)
  #  ) %>%
      
    ##anotatons only, no needed
   # layout(shapes = list(hline(ref$hmax)))%>%
    add_annotations(
      text = with(tolm,paste0(Age[1],"/",Sex[1],"/",SmokingStatus[1])),
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 10)
    )
    #layout(xaxis=list(title=with(tolm,paste0(Age[1],"/",Sex[1],"/",SmokingStatus[1]))))
  #predict(fit, newdata=data.frame(Weeks=c(70,80,90)))
}
interv<-list(1:30,31:60,61:90,121:150,151:length(dd))
pf<-function(x){
  setwd(osisimg)
  pname<-paste0(x[1],"-",x[30])
  dds<-dd[x]
  plots<-lapply(dds,lmf)
  p<-subplot(plots, nrows=floor(length(dds)/3),shareX=T, titleX=F)  %>%
  hide_legend()
  saveWidget(p, paste0(pname,".html"), selfcontained = F, libdir = "lib")
}
lapply(interv,pf)

#lets look into some hints given by graphics

datas<-trd %>% 
  group_by(Patient,Age,Sex,SmokingStatus) %>%
  summarise(rangeweeks=max(Week)-min(Week),
            rangefvc=max(FVC)-min(FVC), maxfvc=max(FVC),meanfvc=mean(FVC),ratioRangeMean=rangefvc/meanfvc)
#samll function to help
lms<-function(x){
  tolm<-trd %>%
    subset(Patient == x) #%>% 
  r<-fit<-lm(FVC ~ Week, tolm)
  return(r$coefficients[2])
}

Slopes<-sapply(dd,lms)
datas<-cbind(datas,Slopes=Slopes)
# %>%
#   mutate(s1=Slopes/maxfvc,s2=Slopes * maxfvc)

plot_ly(datas, x= ~ Slopes, y= ~ Age, type= "scatter", mode="markers",
        marker= list(size= ~ 50* rangefvc/maxfvc, opacity=.7)) %>%
        layout(title="Slopes of FVC changes, with the size equal to maximum FVC") %>%
        add_markers(color= ~ factor(SmokingStatus) )

plot_ly(datas, x= ~ Slopes, y= ~ s1, type= "scatter", mode="markers",
        marker= list(size= ~ rangeweeks/5, opacity=.7)) %>%
  layout(title="Slopes of FVC changes, with the size equal to maximum FVC") %>%
  add_markers(color= ~ factor(Sex) )

plot_ly(datas, x= ~ Slopes, y= ~maxfvc, type= "scatter", mode="markers",
        marker= list(size= ~ 50 *ratioRangeMean, opacity=.7))  %>%
  add_markers(color= ~ factor(SmokingStatus) )

plot_ly(datas, x= ~ Slopes, y= ~maxfvc, type= "scatter", mode="markers",
        marker= list(size= ~maxfvc/500, opacity=.7)) %>%
  add_markers(color= ~ factor(SmokingStatus) )
#This graph show us that the slope is proporcional to range (what makes sense)

#lets see how much of them are on normal interval:
smale<-trd %>% 
  group_by(Patient) %>%
  subset(Sex="Male")%>%
  subset((FVC) > ref$hmin)

#nah loess
sb<-read.csv(file.path(osis,"sample_submission.csv"))
infos<-trd %>%
  select(!c(FVC,Percent,Weeks)) %>%
  unique()

st<-sb %>%
  separate(Patient_Week, sep="_",c("Patient","Weeks")) %>%
  mutate(Patient_Week=sb$Patient_Week) %>%
  merge(infos, by="Patient") %>%
  mutate(Weeks = as.integer((Weeks)))

lmf<-function(x){
  tolm<-trd %>%
    subset(Patient == x) #%>% 
  # mutate(FVC=FVC + 100*rnorm(length(FVC)))
  #ll.rough = loess(xx~tt, span=0.1)
  #fit<-loess(FVC ~ Weeks,tolm)
  #fit<- supsmu(tolm$Weeks,tolm$FVC,span=.1)
  fit<-lm(FVC ~ Weeks, tolm)
  totest<-data.frame(Patient=x,Weeks=-12:133)
  r<-predict(fit,totest,se.fit=T)
  return(r)
}
r<-as.data.frame(sapply(dd, lmf) )

fsub<-function(df,x){
  dfs<-df[[x]]
  ans<-data.frame(Patient_Week=paste0(rep(x,length(-12:133)),"_",-12:133),FVC = dfs$fit, Confidence = dfs$se.fit)
  return(ans)
}   
    
tosub<-lapply(dd,fsub,df=r)
ans<-do.call("rbind",tosub ) %>%
  separate(Patient_Week, sep="_",c("Patient","Week")) %>%
  mutate(Week=as.numeric(Week))%>%
  arrange(Week) %>%
  mutate(Patient_Week=paste0(Patient,"_" ,Week)) %>%
  select(c(Patient_Week,FVC,Confidence))
write.csv(ans,file="submission.csv",row.names = F)
#_________________________correlation
library(data.table)
library(corrgram)
library(vita)
library(randomForest)
picklast<-function(x){
  top<-trd %>%
    subset(Patient==x) %>%
    
}
tt<-data.table(datas[,-1])
tds <- data.frame(model.matrix( ~ .- 1, data=tt)) 
cor_tds <- cor(tds, tds, method = "pearson")
#cor_df<- data.frame(cor=cor_tds[1:40,41], varn = names(cor_tds[1:40,41])) 
# cor_df<- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
# plot(cor_df$cor_abs, type="l")

corrgram(cor_tds,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
tt<- datas %>%
  select(!c(Slopes))

X=tt[,-1]
y=datas$Slopes

reg.rf = randomForest(X,y,mtry = 3,ntree=500,importance=TRUE)

imp<-PIMP(X,y,reg.rf,S=10, parallel=F)



# 
# 
# load(system.file("hk-40/hk40.RData", package="oro.dicom"))
# dcmList <- hk40
# dcmImage <- create3D(dcmList)
# image(dcmImage[,,2], col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
#       main=paste("First Slice from HK-40"))
# imagePositionPatient <- attributes(dcmImage)$ipp
# dSL <- abs(diff(imagePositionPatient[,3]))
# plot(dSL, ylim=range(range(dSL) * 1.5, 0, 10), xlab="Image", ylab="mm",
#      main="Difference in Slice Location")
# 
# ## Not run: 
# ## pixelData = FALSE
# ## The DICOM image data are read from create3D()
# ## This may save on memory for large batches of DICOM data
# dcmList <- readDICOM(system.file("hk-40", package="oro.dicom"),
#                      pixelData=FALSE)
# dcmImage <- create3D(dcmList, pixelData=FALSE)
# image(dcmImage[,,1], col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
#       main=paste("First Slice from HK-40 (again)"))
# 
# ## End(Not run)
# ## mosaic = TRUE
# mosaicFile <- system.file("dcm/MR-sonata-3D-as-Tile.dcm", package="oro.dicom")
# dcm <- readDICOMFile(mosaicFile)
# image(t(dcm$img), col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
#       main="Siemens MOSAIC")
# dcmImage <- create3D(dcm, mode="integer", mosaic=TRUE)
# z <- trunc(dim(dcmImage)[3]/2)
# image(dcmImage[,,z], col=grey(0:64/64), axes=FALSE, xlab="", ylab="",
#       main=paste("Slice", z, "from Siemens MOSAIC"))
# 
# 
# 
# 
# 
# 


a<-data.frame(economics)
p <- subplot(
  plot_ly(a, x = date, y = psavert)%>%layout(showlegend = FALSE),
  plot_ly(a, x = date, y = unemploy)%>%layout(showlegend = FALSE),
  margin = 0.05
) 
# 


testset<- st %>%
  #select(!c(,Confidence,FVC))%>%
  mutate(Week=as.numeric(Weeks)) %>%
  arrange((Week))