#all<-read.csv("data/all_occ_data.csv") 'all' data upon request


# add one to observations, so 0 becomes 1, 1 -> 2, for use in the model
all$Common.Loon<-all$Common.Loon+1


#array for observations, time_obs
# protocol, duration, eff_dis, eff_are

#number of days for each year in the summer months, to order observations by
mdnumeric<-seq(1,92,by=1)
unmd<-unique(all$Date[which(all$year==2017)])
mdnumeric<-cbind(mdnumeric, unmd)
MD<-numeric(length(all$Date))
all<-cbind(all,MD)
for(i in 1:length(all$Date)){
  for(j in 1:length(mdnumeric[,1])){
    if(all$Date[i]==mdnumeric[j,2]){
      all$MD[i]<-mdnumeric[j,1]
    }else{}
  }
}

yarray<-function(){
  yrs<-seq(1,18, by=1)
  siteid<-sites[,1]
  al$sids<-as.numeric(al$sids)
  al$yr<-as.numeric(al$yr)
  al$MD<-as.numeric(al$MD)
  al$Common.Loon<-as.numeric(al$Common.Loon)
  al$protocol_cat<-as.numeric(al$protocol_cat)
  y<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(y)<-list(siteid, 2000:2017, 1:92,1:30)
  ind<-y
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempy<-c(al$Common.Loon[ty])
          t2y<-rep(NA, 30-length(tempy))
          y[i,j,k,]<-c(tempy,t2y)
        } else{
          y[i,j,k,]<-rep(NA,30)
        }
      }
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempy<-c(al$protocol_cat[ty])
          t2y<-rep(NA, 30-length(tempy))
          ind[i,j,k,]<-c(tempy,t2y)
        } else{
          ind[i,j,k,]<-rep(NA,30)
        }
      }
    }
  }
  ind[ind==3]<-1
  ind[ind==4]<-1
  ind[ind==5]<-2
  #ind: when eBird ==1, when wdfw/other ==2
  
  al$time_obser<-as.numeric(al$time_obser)
  timeobs<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(timeobs)<-list(siteid, 2000:2017, 1:92,1:30)
  
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempTime<-c(al$time_obser[ty])
          ttime<-rep(NA, 30-length(tempTime))
          timeobs[i,j,k,]<-c(tempTime, ttime)
        } else{
          timeobs[i,j,k,]<-rep(NA, 30)
        }
      }
    }
  }
  
  al$duration_m<-as.numeric(al$duration_m)
  dur_m<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(dur_m)<-list(siteid, 2000:2017, 1:92,1:30)
  
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempDur<-c(al$duration_m[ty])
          tDur<-rep(NA, 30-length(tempDur))
          dur_m[i,j,k,]<-c(tempDur, tDur)
        } else{
          dur_m[i,j,k,]<-rep(NA, 30)
        }
      }
    }
  }
  protocol<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(protocol)<-list(siteid, 2000:2017, 1:92,1:30)
  
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempProt<-c(al$protocol_cat[ty])
          tProt<-rep(NA, 30-length(tempProt))
          protocol[i,j,k,]<-c(tempProt, tProt)
        } else{
          protocol[i,j,k,]<-rep(NA, 30)
        }
      }
    }
  }
  
  effort_diskm<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(effort_diskm)<-list(siteid, 2000:2017, 1:92,1:30)
  
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempED<-c(al$effort_dis[ty])
          tED<-rep(NA, 30-length(tempED))
          effort_diskm[i,j,k,]<-c(tempED, tED)
        } else{
          effort_diskm[i,j,k,]<-rep(NA, 30)
        }
      }
    }
  }
  
  effort_areaHa<-array(dim=c(length(siteid),length(2000:2017),length(1:92),length(1:30)))
  dimnames(effort_areaHa)<-list(siteid, 2000:2017, 1:92,1:30)
  
  for(i in 1:length(siteid)){
    for(j in 1:length(yrs)){
      for(k in 1:length(mdnumeric[,1])){
        ty<-c(as.numeric(which(al$sids==siteid[i] & al$yr==yrs[j] & al$MD==mdnumeric[k,1])))
        if (isTRUE(ty[1]>0)){
          tempEA<-c(al$effort_are[ty])
          tEA<-rep(NA, 30-length(tempEA))
          effort_areaHa[i,j,k,]<-c(tempEA, tEA)
        } else{
          effort_areaHa[i,j,k,]<-rep(NA, 30)
        }
      }
    }
  }
  return(list(y=y, timeobs=timeobs,ind=ind,
              dur_m=dur_m, protocol=protocol, effort_areaHa=effort_areaHa,
              effort_diskm=effort_diskm))
}

system.time(dat<-yarray())
#about an hour and a half to run


#calculate perimeter complexity, perimetr/sqrt(area_sqkm)
#make array/matrix/vectors for site level covariates

#make a matrix with area, elevation, perimeter, hii for each site

sitecovs_mat<-function(){
  sitecovs<-matrix(nrow=length(sites[,1]), ncol=6)
  
  sites_un<-al[!duplicated(al$sids),]
  sitecovs[,1]<-unique(al$sids)
  for(i in 1:length(sites_un$X)){
    for(j in 1:length(sitecovs[,1])){
      if(al$sids[i]==sitecovs[j,1]){
        sitecovs[j,2]<-al$area_sq_km[i]
        sitecovs[j,3]<-al$elevation_m[i]
        sitecovs[j,4]<-al$Perimeter_km[i]
        sitecovs[j,5]<-al$hii[i]
      }else{}
    }
  }
  
  sitecovs<-as.data.frame(sitecovs)
  colnames(sitecovs)<-c("siteid", "AreaSqKm", "elevation_m", "Perimeter_km", "HII", "PerimeterCom")
  
  for(i in 1:length(sitecovs$siteid)){
    sitecovs$PerimeterCom[i]<-sitecovs$Perimeter_km[i]/(sqrt(sitecovs$AreaSqKm[i]))
  }
  return(sitecovs)
}
sitecovs<-sitecovs_mat()



zinit<-function(){
  y<-dat$y
  y[is.na(y)]<-0
  zinit<-array(dim=c(766,18))
  for(i in 1:766){
    for(t in 1:18){
      zinit[i,t]<-max(y[i,t,,])
    }
  }
  return(zinit)
}
z<-zinit()

#Data output to an RData object



