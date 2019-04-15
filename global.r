#Turns off warnings for the loading of the packages
#Loads in necessary libraries from the external hard drive
suppressMessages(library(rgdal))
suppressMessages(library(maptools))
suppressMessages(library(tmap))
suppressMessages(library(dplyr))
suppressWarnings(library(RColorBrewer))
suppressMessages(suppressWarnings(library(BAMMtools)))
suppressWarnings(library(shinycssloaders))
suppressWarnings(library(RODBC))
suppressWarnings(library(shinyjs))

# dbconnection<-odbcDriverConnect("Driver=ODBC Driver 13 for SQL Server;Server=WH-255A\\SQLEXPRESS; Database=NRCS;Uid=mattrich2387; Pwd=dinorawr2387")
# Easements<-sqlQuery(dbconnection,paste("select * from ACEP_WRP_Enrollment;"))
# odbcClose(dbconnection)

#Reads in the necessary data sets
Easements<-read.csv("Updated_WRP_ACEP.csv",fileEncoding = "latin1",stringsAsFactors = F)
County<-readShapePoly("USA_Counties")
State<-readShapePoly("USA_States")

#Turns the easement data set into a spatial points data frame and formats some of the variables
Easements$County<-trimws(Easements$County)
coordinates(Easements)<-c("Longitude","Latitude")
proj4string(Easements)<-proj4string(State)

#Updates the names of the variables to be uniform accross data sets
colnames(State@data)[2]<-"State"
colnames(County@data)[c(3,4)]<-c("County","State")
statez<-State@data[order(State@data$State),]

#Removes DC from the states list and creates a variable for our options
z<-as.character(statez$State[-9])
a<-c("Select All","Deselect All","Continental","MAV")
z<-c(a,z)
trouble_state<-c("Arizona","Colorado","Indiana","Iowa","Kansas","Minnesota","New Mexico","North Dakota","Oregon","Utah","Wyoming")

#This section calculates the acres enrolled between the time period x and y. Z is a list of states and countylevel is set to
#either true or false depending on if you want the calculation to be done on the county(T) or state(F) level. Borders 
#determines whether or not you want borders between states.
#Calculation
Enrollment_Map<-function(x,y,z,countylevel,borders){
  if (length(z)==0){
  } else{
  #Creates the necessary subsetted data sets
      Counties<-subset(County,State %in% z)
      Easementz<-subset(Easements,State %in% z)
      States<-subset(State,State %in% z)
      #Runs the calculation if the county level is selected
      if (countylevel){
        #Creates a data set for the enrollment per county depending on the start and finish years
        if(x==y){
          data<-Easementz@data %>% group_by(County) %>% filter(Enroll_FY==x)%>% summarize("Wetland Area" = sum(ACRE_COUNT))
        }else{
          data<-Easementz@data %>% group_by(County) %>% filter(Enroll_FY >=x,Enroll_FY<=y)%>% summarize("Wetland Area" = sum(ACRE_COUNT))
        }
        #Joins the enrollment per county information with the locational data set
        Counties@data<-full_join(Counties@data,data,by="County")
        #Replaces any missing values for wetland area with a value of 0
        for(i in (1:length(Counties@data$County))){
          if (Counties@data$`Wetland Area`[i] %in% NA){
            Counties@data$`Wetland Area`[i] <- 0
          }
        }
        
        #Creates the map
        breaks<-getJenksBreaks(Counties@data$`Wetland Area`,k=7)
        #Creates an empty map if there is no enrollment
        if (sum(breaks)==0){
          if(x==y){
            tm_shape(Counties)+tm_fill(col = "white")+tm_borders("black")+
              tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "white")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep="")
                        ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = F)+
              tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                           if(borders){
                                                             lwd=2}
                                                           else{lwd=1
                                                           })
          } else{
            tm_shape(Counties)+tm_fill(col = "white")+tm_borders("black")+
              tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "white")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep="")
                        ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = F)+
              tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                           if(borders){
                                                             lwd=2}
                                                           else{lwd=1
                                                           })
          }
        } else{
          #Creates two similar graphs with differing titles depending on the start and end date
          if (length(z) == 1 & sum(z %in% trouble_state)>0){
            if(x==y){
              tm_shape(Counties)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+tm_borders("black")+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep="")
                          ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = T,legend.outside.position = "right")+
                tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                             if(borders){
                                                               lwd=2}
                                                             else{lwd=1
                                                             })
            } else{
              tm_shape(Counties)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+tm_borders("black")+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep="")
                          ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = T,legend.outside.position = "right")+
                tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                             if(borders){
                                                               lwd=2}
                                                             else{lwd=1
                                                             })
          }
        } else{
          if(x==y){
            tm_shape(Counties)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+tm_borders("black")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep="")
                        ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = F)+
              tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                           if(borders){
                                                             lwd=2}
                                                           else{lwd=1
                                                           })
          } else{
            tm_shape(Counties)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+tm_borders("black")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep="")
                        ,main.title.position = c("center","TOP"),frame = F,asp=1,legend.outside = F)+
              tm_shape(States)+tm_fill(alpha=0)+tm_borders("black",
                                                           if(borders){
                                                             lwd=2}
                                                           else{lwd=1
                                                           })
            }
          }
        }
      } else{
        #Runs the calculations if the county level is not selected
        States<-State
        Easementz<-subset(Easements,State %in% z)
        #Creates a data set for the enrollment per State depending on the start and end years
        if(x==y){
          data<-Easementz@data %>% group_by(State) %>% filter(Enroll_FY==x)%>% summarize("Wetland Area" = sum(ACRE_COUNT))
        }else{
          data<-Easementz@data %>% group_by(State) %>% filter(Enroll_FY >=x,Enroll_FY<=y)%>% summarize("Wetland Area" = sum(ACRE_COUNT))
        }
        #Joins the enrollment data set with the locational data set
        data$State<-as.character(data$State)
        States@data<-full_join(States@data,data,by="State")
        States<-subset(States, State %in% z)
        
        #Replaces any missing values for wetland area with a value of 0
        for(i in (1:length(States@data$State))){
          if (States@data$`Wetland Area`[i] %in% NA){
            States@data$`Wetland Area`[i] <- 0
          }
        }
        #Creates the maps
        breaks<-getJenksBreaks(States@data$`Wetland Area`,k=7)
        #Creates an empty map if there is no enrollment
        if(sum(breaks)==0){
          if(x==y){
            tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col = "white")+
              tm_borders("black",if(borders){
                lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "white")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep=""),main.title.position = c("center","TOP"),
                        frame = F,asp=1,legend.outside = F)
          } else{
            tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col="white")+
              tm_borders("black",if(borders){
                lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "white")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep=""),main.title.position = c("center","TOP"),
                        frame = F,asp=1,legend.outside = F)
          }
        } else{
          #Creates two different sets of maps. The first set is if there is only one state, then the script updates the shading
          #to only be of one value. If there are more than one state selected, then the normal break algorithm is applied
          if (length(z)==1){
          if (z %in% trouble_state){
            if(x==y){
              tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col = "#2B8CBE")+
                tm_borders("black",if(borders){
                  lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "#2B8CBE")+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep=""),main.title.position = c("center","TOP"),
                          frame = F,asp=1,legend.outside = T,legend.outside.position = "right")
            } else{
              tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col="#2B8CBE")+
                tm_borders("black",if(borders){
                  lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "#2B8CBE")+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep=""),main.title.position = c("center","TOP"),
                          frame = F,asp=1,legend.outside = T,legend.outside.position = "right")
            }
          } else{
          if(x==y){
            tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col = "#2B8CBE")+
              tm_borders("black",if(borders){
                lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "#2B8CBE")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep=""),main.title.position = c("center","TOP"),
                        frame = F,asp=1,legend.outside = F)
          } else{
            tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col="#2B8CBE")+
              tm_borders("black",if(borders){
                lwd=2}else{lwd=1})+tm_add_legend(type="fill",labels = max(breaks),title="Acres Enrolled",col = "#2B8CBE")+
              tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep=""),main.title.position = c("center","TOP"),
                        frame = F,asp=1,legend.outside = F)
              }
            }
          }else{
            if(x==y){
              tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+
                tm_borders("black",if(borders){
                  lwd=2}else{lwd=1})+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,sep=""),main.title.position = c("center","TOP"),
                          frame = F,asp=1,legend.outside = F)
            } else{
              tm_shape(Counties)+tm_fill(alpha=0)+tm_borders("black",lwd=0)+tm_shape(States)+tm_fill(col = "Wetland Area",breaks=c(unique(breaks),Inf),palette = "GnBu",title = "Acres Enrolled")+
                tm_borders("black",if(borders){
                  lwd=2}else{lwd=1})+
                tm_layout(main.title = paste("Acres Enrolled in the ACEP-WRP ",x,"-",y,sep=""),main.title.position = c("center","TOP"),
                          frame = F,asp=1,legend.outside = F)
          
          }
        }
      }
    }
  }
}