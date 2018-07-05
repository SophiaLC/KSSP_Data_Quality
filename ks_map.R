ks_map <- 
  function(data, key, variable, label="", lowcolor="#56B1F7", highcolor="#132B43", title="") {
    # loading packages
    suppressPackageStartupMessages(require(ggplot2))
    suppressPackageStartupMessages(require(ggmap))
    suppressPackageStartupMessages(require(maps))
    suppressPackageStartupMessages(require(dplyr))
    suppressPackageStartupMessages(require(tidyr))
    
    # get mapping data
    mapdata <- map_data("county", "kansas")
    
    # combine with fips codes
    data("county.fips", envir=environment())
    mapdata <- county.fips %>%
      separate(polyname, c("region", "subregion"), "[,]") %>% 
      filter(region=="kansas") %>% 
      select(-region) %>% 
      full_join(mapdata, by="subregion") %>% 
      rename(county=subregion) %>% 
      select(-region)
    
    if (key=="fips") {
      mapdata <- left_join(mapdata, data, by="fips")
      plot <- ggplot(mapdata, aes(x=long, y=lat, group=group, fill=get(variable))) +
        geom_polygon() +
        coord_map() +
        theme_void() +
        scale_fill_gradient(name=label, low=lowcolor, high=highcolor) + 
        labs(title=title)
      return(plot)
    } else if (key=="county") {
      mapdata <- left_join(mapdata, data, by="county")
      plot <- ggplot(mapdata, aes(x=long, y=lat, group=group, fill=get(variable))) +
        geom_polygon() +
        coord_map() +
        theme_void() +
        scale_fill_gradient(name=label, low=lowcolor, high=highcolor) +
        labs(title=title)
      return(plot)
    } else {
      stop("Please specify key as either 'county' or 'fips'")
    }
  }
