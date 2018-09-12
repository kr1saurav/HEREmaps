#Calculate road distance between two points
#inputs are geolocations
#to get geolocatiojn from adress/city/country use geolocationHERE
#Usage - 
#distanceHERE("41.9798","-87.8801","41.9043","-87.9216")
#Outpur distance is in meters


distanceHERE <- function(fromLat, fromLong,toLat, toLong, App_id="", App_code=""){
  if(!is.character(App_id)){stop("'App_id' must be a character string")}
  if(!is.character(App_code)){stop("'App_code' must be a character string")}
  
  if(App_id=="" & App_code==""){    
    App_id <- "6zyidlgBmlONH3Jtbp7R"
    App_code <- "XeVxytH8G5wa053A5EWflg"
    base_url <- "https://route.api.here.com/routing/7.2/calculateroute.json"
  }else{
    base_url <- "https://route.api.here.com/routing/7.2/calculateroute.json"
  }
  
  
  origin <- paste(fromLat,",",fromLong,sep="")
  dest<- paste(toLat,",",toLong,sep="")

  a <- httr::GET(base_url, query=list(waypoint0 = origin,
                                      waypoint1 = dest,
                                      mode="fastest;truck",
                                      app_id = App_id,
                                      app_code = App_code))
 # distanceR <- "NA"
  distanceR <- httr::content(a)
  
  if(length(distanceR) > 0){
    ret <-  distanceR$`response`$route[[1]]$summary$`distance`
  }else{
    ret <- "NA"
  }
  return(ret)
}


