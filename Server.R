library(shinydashboard)
library(leaflet)
library(raster)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(rgdal)
library(lubridate)
library(curl)
library(ggplot2)
library(stringr)
## read data
# Council Population data
population <- read.csv("data/CDpopu&area.csv")
# CD shapefile
cd <- readOGR("cdinfo/l.a. city council district (2012).shp")  

shinyServer(function(input, output,session){
  # --------------sidebar Hide Default-------
  addClass(selector = "body", class = "sidebar-collapse")
  # ----------------input---------------
  output$serviceText <- renderUI({
    h5(strong(paste("Requests of",input$serviceType)),align = "center")
  })
  # ------download data--------------------
  requestdata <- reactive({
    date1 <- input$dates[1]
    date2 <- input$dates[2]
    url = NULL
    request <- NULL
    for (i in seq(0,1500000000000,50000)){
      options("scipen"=20)
      url <- append(url,paste("https://data.lacity.org/resource/ndkd-k878.csv?$select=createddate,updateddate,status,servicedate,closeddate,requesttype,address,cd,longitude,latitude&$order=createddate%20DESC&$where=createddate%3E%20%27",date1,"%27%20AND%20createddate%3C%20%27",date2,"%27&$limit=50000&$offset=",i,sep = ""))
      a<- length(url)
      requestone <- read.csv(curl_download(url[a], "request.csv", 
                                           quiet = TRUE, mode = "wb",
                                           handle = new_handle()))
      if (nrow(requestone)>1){
        request<- rbind(request,
                        requestone)
      }else{
        break
      }}
    request$CreatedDate <- mdy_hms(request$CreatedDate)
    request$UpdatedDate <- mdy_hms(request$UpdatedDate)
    return(request)
  })
  
  solveddata <- reactive({
      request <- requestdata()
      solved <- filter(request, Status=="Closed" & !is.na(ServiceDate))
      solved <- filter(solved, !is.na(Longitude) & !is.na(Latitude))
      
      # calculate solve time
      solved$duration <- as.numeric(solved$UpdatedDate-solved$CreatedDate,units="mins")
      solved <- filter(solved, duration >20)
      
      solved
  })
    #---------------output----------------
  
    #--------------1.Distribution-------------------
  
  #------------------1.1 map--------------------
  
  # Make a list of icons. We'll index into it based on name.
  typeicon <- iconList(
    Graffiti = makeIcon("icon/giraffiti.png"),
    Bulky = makeIcon("icon/bulky.png"),
    Dump = makeIcon("icon/dump.png"),
    Appliance = makeIcon("icon/appliance.png"),
    Ewaste = makeIcon("icon/ewaste.png"),
    Rat = makeIcon("icon/rat.png"),
    Singlelight = makeIcon("icon/singlelight.png"),
    Multilight = makeIcon("icon/multilight.png"),
    Homeless = makeIcon("icon/homeless.png"))
  
  
  # summary data
  summarytable <- reactive({
    table=solveddata()%>%
      filter(RequestType==input$serviceType)%>%
      group_by(CD)%>%
      summarise(Duration=mean(duration, na.rm=T),Frequency = n())
    
    table$Duration <- as.integer(table$Duration)
    table$DurationNum <- table$Duration
    
    # mean duration & duration String
    meanDuration <- as.integer(mean(table$Duration))
    meanDurStr <- as.character(seconds_to_period(meanDuration*60))
    meanDurStr <- str_sub(meanDurStr, start= 1, end=str_locate(meanDurStr,"H")[2]+1)
    
    # each duration string
    table$Duration <- as.character(seconds_to_period(table$Duration*60))
    table$Duration <- str_sub(table$Duration, start= 1,end=str_locate(table$Duration,"H")[2]+1)
    
    # add average row
    table <- rbind(table,c("Average",meanDurStr,as.integer(mean(table$Frequency)),meanDuration))
    
    # order of durationNum
    
    table$DurationNum <- as.numeric(table$DurationNum)
    table$Duration <- as.factor(table$Duration)
    table$Duration <- factor(table$Duration, ordered=T,
                             levels = unique(table[order(table$DurationNum),"Duration"][[1]]))
    table$Frequency <- as.numeric(table$Frequency)
    # order CD levels
    table$CD <- factor(table$CD, levels = c(1:15,"Average"))
    colnames(table)[2:3] <- c("AveSolvingTime","NumofRequestsSolved")
    table<-arrange(table,CD)
    table
  })
  # leaflet data
  cddata <- reactive({
    table <- summarytable()
    cdcount <- table[-16,]
    cd@data <- merge(cd@data, cdcount, by.x = "name", by.y = "CD", all.x = T, all.y = F)
    cd@data <- merge(cd@data, population,by.x = "name",by.y="CD",all.x=T,all.y=F)
#   cd@data$aveDurationNum <- cd@data$aveDuration
#    cd@data$aveDuration <- as.character(seconds_to_period(60*cd@data$aveDuration))
#    cd@data$aveDuration <- str_sub(cd@data$aveDuration, start= 1, end=str_locate(cd@data$aveDuration,"H")[2]+1)
    
#    cd@data$totalAveDurationNum <- cd@data$totalAveDuration
#    cd@data$totalAveDuration <- as.character(seconds_to_period(60*cd@data$totalAveDuration))
#    cd@data$totalAveDuration <- str_sub(cd@data$totalAveDuration, start= 1, end=str_locate(cd@data$totalAveDuration,"H")[2]+1)
    cd@data$name <- factor(cd@data$name, levels = c(1:15))
    cd@data <- cd@data[order(cd@data$name), ]
    rownames(cd@data) <- c(0:14)
    cd
  })
  
  # spatical data link location with icon
  solved2data <- reactive({
    solved <- solveddata()
    solved <- filter(solved, RequestType==input$serviceType)
    
    # threshold now: data max volumn -- 65536
    ## Scale the solved1 dataset if necessary
    ThresholdVery = dim(solved)[1]
    if (ThresholdVery <= 65536) {
      solved1 = solved
    } else {
      solved1 = solved[1:65536, ]
    }
    
    solved1$duration = round(solved1$duration)
    solved1$durationStr = seconds_to_period(60*solved1$duration)
    solved1 = select(solved1, RequestType, Longitude, Latitude, CreatedDate, CD, Address, durationStr)
    solved1$CreatedDate = as.factor(solved1$CreatedDate)
    solved1$CD = as.factor(solved1$CD)
    solved1$Address = as.factor(solved1$Address)
    solved1$durationStr = as.character(solved1$durationStr)
    
    # Spatial data - solved2
    solved2 <- sp::SpatialPointsDataFrame(
      cbind(
        solved1[,"Longitude"],  # lng
        solved1[,"Latitude"],  # lat
        solved1[,"RequestType"],  # RequestType
        solved1[,"CD"]  # CD
      ),
      data.frame(type = factor(
        ifelse(solved1$RequestType == "Graffiti Removal", "Graffiti", 
               ifelse(solved1$RequestType == "Bulky Items", "Bulky", 
                      ifelse(solved1$RequestType == "Illegal Dumping Pickup", "Dump", 
                             ifelse(solved1$RequestType == "Metal/Household Appliances", "Appliance", 
                                    ifelse(solved1$RequestType == "Electronic Waste", "Ewaste", 
                                           ifelse(solved1$RequestType == "Dead Animal Removal", "Rat", 
                                                  ifelse(solved1$RequestType == "Single Streetlight Issue", "Singlelight", 
                                                         ifelse(solved1$RequestType == "Multiple Streetlight Issue", "Multilight", "Homeless")))))))),
        c("Graffiti", "Bulky", "Dump", "Appliance", "Ewaste", "Rat", "Singlelight", "Multilight", "Homeless")
      ))
    )
    # add more features to the solved2 spatial dataset
    solved2@data$requestType = solved1$RequestType
    solved2@data$CD = solved1$CD
    solved2@data$Address = solved1$Address
    solved2@data$durationStr = solved1$durationStr
    solved2@data$CreatedDate = solved1$CreatedDate
    solved2
  })
  
  
  # leaflet: draw everything
  output$map <- renderLeaflet({
    solved2 <-solved2data()
    cd <- cddata()
    # district pop-up
    content <- NULL
    for (i in c(10:15, 1:9)) {
      content <- append(content, paste(sep = "<br/>",
                                       paste("<b><a><font color = 'Grey'>", "CD Number: ", "</font>", as.numeric(cd@polygons[[i]]@ID)+1, "</a ></b>"),
                                       paste("<b><a><font color = 'Grey'>", "Population: ", "</font>", as.numeric(cd@data$Population[i]), "</a ></b>"),
                                       paste("<b><a><font color = 'Grey'>", "Area: ", "</font>", cd@data$Area..Sq.Mi.[i],"<font color = 'Grey'>", "square miles", "</font>","</a ></b>"),
                                       paste("<b><a><font color = 'Grey'>", "Population Density: ", "</font>", as.integer(cd@data$Density[i]), "<font color = 'Grey'>", "per sq. mi", "</font>","</a ></b>"),
                                       paste("<b><a><font color = 'Grey'>", "District Avg. Solving Time: ", "</font>", cd@data$AveSolvingTime[i], "</a ></b>"),
                                       paste("<b><a><font color = 'Grey'>", "Overall Avg. Solving Time: ", "</font>", summarytable()$AveSolvingTime[16],"</a ></b>")
                                       
      ))
    }
    ## build the html popup for solved2
    contentSol <- paste(sep = "<br/>",
                        paste("<b><a><font color = 'Grey'>", "Request Type: ", "</font>", as.character(solved2@data[, 2]), "</a ></b>"),
                        paste("<b><a><font color = 'Grey'>", "CD Number: ", "</font>", as.character(solved2@data[, 3]), "</a ></b>"),
                        paste("<b><a><font color = 'Grey'>", "Address: ", "</font>", as.character(solved2@data[, 4]), "</a ></b>"),
                        paste("<b><a><font color = 'Grey'>", "Created Date: ", "</font>", as.character(solved2@data[, 6]), "</a ></b>"),
                        paste("<b><a><font color = 'Grey'>", "Processing Time: ", "</font>", as.character(solved2@data[, 5]), "</a ></b>"))
    leaflet(solved2) %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -118.4, lat = 34.09, zoom = 9) %>%
      addMarkers(lng = solved2@coords[, 1], lat = solved2@coords[, 2], icon = ~typeicon[type], 
                 clusterOptions = markerClusterOptions(),
                 popup = paste(contentSol)) %>%
      addPolygons(data = cd, opacity = 0.3, fillOpacity = 0.4,
                  stroke = T, weight = 1, popup = content,
                  color =~ colorNumeric("YlOrRd", DurationNum)(DurationNum))
  })
  
  # ------------ 1.2 Performance Table------------
  output$requestPerform <- renderDataTable({
    table <- summarytable()[,c(1:3)]
    table},
    options = list(searching = FALSE,paging = FALSE))
  #--------------2.compare--------------------
  
  #--------------3.history------------------------------
  #------------------One District Data---------------
  distdata <- reactive({
    datea <- input$histDates[1]
    dateb <- input$histDates[2]
    districtnum <- input$oneDistrict
    # ------------- load district Historical Data--------------
    url = NULL
    reqdistall <- NULL
    
    for (i in seq(0, 1500000000000, 50000)){
      options("scipen" = 20)
      url <- append(url,paste("https://data.lacity.org/resource/ndkd-k878.csv?$select=createddate,updateddate,servicedate,closeddate,requesttype,address,cd,longitude,latitude&$order=createddate%20DESC&$where=createddate%3E%20%27",datea,"%27%20AND%20createddate%3C%20%27",dateb,"%27%20AND%20cd=",districtnum,"&$limit=50000&$offset=",i,sep = ""))
      a<- length(url)
      requestdist <- read.csv(curl_download(url[a], "requestdis.csv", 
                                            quiet = TRUE, mode = "wb",
                                            handle = new_handle()))
      if (nrow(requestdist)>1){
        reqdistall<- rbind(reqdistall,
                           requestdist)
      }else{
        break
      }
    }
    
    
    reqdistall_new <- merge(reqdistall, population, by.x = "CD", by.y = "CD", all.x = T)
    reqdistall_new$CreatedDate <- mdy_hms(reqdistall_new$CreatedDate)
    reqdistall_new$UpdatedDate <- mdy_hms(reqdistall_new$UpdatedDate)
    reqdistall_new$duration <- as.numeric(reqdistall_new$UpdatedDate - reqdistall_new$CreatedDate, units="mins")
    
    # service type - color
    # per week - count, duration
    new1 = 8 - wday(input$histDates[1])
    datestart = input$histDates[1] + days(new1)
    wday(datestart, label = T, abbr = F)
    
    new2 = 6 - wday(input$histDates[2])
    dateend = input$histDates[2] - days(new2)
    wday(dateend, label = T, abbr = F)
    
    ## weeks that in our calculation
    weekcal = (dateend - datestart + 1) / 7
    weekcal = as.numeric(weekcal)
    
    reqdistall_new$CreateDate1 = paste(year(reqdistall_new$CreatedDate), 
                                       month(reqdistall_new$CreatedDate), 
                                       day(reqdistall_new$CreatedDate), sep = "-")
    reqdistall_new$CreateDate1 = ymd(reqdistall_new$CreateDate1)
    
    reqdistall_new$weeknum = ceiling(as.numeric((reqdistall_new$CreateDate1- datestart + 1)/7))
    
    ## filter data start from weeknum-1 to weeknum-weekcal
    reqdistall_new = filter(reqdistall_new, weeknum >= 1, weeknum <= weekcal)
    reqdistall_new$weeknum = factor(reqdistall_new$weeknum)
    
    ## Groupped dataset
    reqdistall_group = reqdistall_new %>%
      group_by(weeknum, RequestType) %>%
      summarise(count = n(), Avg_duration = mean(duration))
    
    reqdistall_group})

  ## plot count by weeknum, color by RequestType
  output$trend1 <- renderPlot({
    data <- distdata()%>%
      filter(RequestType%in%input$serviceTypeAll)
    
   ggplot(data, aes(x = weeknum, y = count, color = RequestType, group = RequestType)) + 
      geom_point(size = 1) + 
      geom_line(size = 1) 
  })
  
  
  ## plot duration by weeknum, color by RequestType
  output$trend2 <- renderPlot({
    data <- distdata()%>%
      filter(RequestType%in%input$serviceTypeAll)
    
    ggplot(data, aes(x = weeknum, y = Avg_duration, color = RequestType, group = RequestType)) + 
      geom_point(size = 1) + 
      geom_line(size = 1)
  })
})
