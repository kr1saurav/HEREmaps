#' Attempt to geocode a string
#'
#' Enter a string and if found, the latitude and longitude is returned using the HERE API
#' @param search A string to search
#' @param App_id App_id to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @param App_code App_code to use the production HERE API. Get one here... http://developer.here.com/get-started. If left blank, will default to demo key with an unknown usage limit.
#' @return A list containing Latitude and Longitude if found, NA otherwise
#' @keywords geocode
#' @export
#' @examples
#' geocodeHERE_simple("chicago")
#' geocodeHERE_simple("wrigley field chicago IL")
#' geocodeHERE_simple("233 S Wacker Dr, Chicago, IL 60606")
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


