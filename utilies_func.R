
get_new_coordinates <- function(ns,bg_shape,isid,technique,lay_out,preLoc=NULL){
  set.seed(isid)
  if(bg_shape=="Circle"){
    radius  <- 1
    if(lay_out=="Random"){
      pp      <- rpoispp(ns/(pi*radius^2),win=disc(radius,centre=rep(radius,2)))
      simLoc  <- cbind.data.frame(x=pp[['x']],y=pp[['y']],group=rep("A",pp[["n"]]))
    }else{
      shp     <- st_as_sf(disc(radius,centre=rep(radius,2)))
      nspots  <- area(st_bbox(shp))/area(shp)*ns ## approxmiate number of spots generated to the one required
      grid    <- shp %>% 
        st_make_grid(n=round(sqrt(nspots)), what = "centers") %>% # grid of points
        st_intersection(shp)                               # only within the polygon

      simLoc  <- as.data.frame(st_coordinates(grid))
      simLoc$group <- rep("A",nrow(simLoc))
    }
    
  }else if(bg_shape=="Square"){
    if(lay_out=="Random"){
      pp      <- rpoispp(ns)
      simLoc  <- cbind.data.frame(x=pp[['x']],y=pp[['y']],group=rep("A",pp[["n"]]))
    }else{
      shp     <- st_as_sf(owin(xrange=c(0,1), yrange=c(0,1)))
      nspots  <- area(st_bbox(shp))/area(shp)*ns ## approxmiate number of spots generated to the one required
      grid    <- shp %>% 
        st_make_grid(n=round(sqrt(nspots)), what = "centers") %>% # grid of points
        st_intersection(shp)                               # only within the polygon

      simLoc  <- as.data.frame(st_coordinates(grid))
      simLoc$group <- rep("A",nrow(simLoc))
    }
  }else if(bg_shape=="User Define Shape"){
    if(!is.null(preLoc)){
      # if(length(grep("group",colnames(preLoc)))==0){
      # if(simNew){
        pnts    <- preLoc[,1:2] %>% st_as_sf(coords = c("x", "y"))
        polygon <- concaveman(pnts,2.0) 
        poly_coords = as.data.frame(as.matrix(polygon$polygons[[1]]))
        colnames(poly_coords) <- c("x","y")
        Pl      <- Polygon(poly_coords)
        if(lay_out=="Random"){
          pts     <- spsample(Pl,n=ns,"random")
          simLoc  <- data.frame(x=pts$x,y=pts$y,group=rep("A",nrow(pts@coords)))
        }else{
          shp     <- polygon
          nspots  <- area(st_bbox(shp))/area(shp)*ns ## approxmiate number of spots generated to the one required
          grid    <- shp %>% 
            st_make_grid(n=round(sqrt(nspots)), what = "centers") %>% # grid of points
            st_intersection(shp)                               # only within the polygon

          simLoc  <- as.data.frame(st_coordinates(grid))
          simLoc$group <- rep("A",nrow(simLoc))
        }
    }else{
      # warnings("Please provide a dataframe with spots location for the boundary estimation")
      print("No dataframe provided, simulate using the square backgound shape")
      pp        <- rpoispp(ns)
      simLoc    <- cbind.data.frame(x=pp[['x']],y=pp[['y']],group=rep("A",pp[["n"]]))
    }
  }else if(bg_shape=="User Define Spots"){
      if(!is.null(preLoc)){
          if(length(grep("group",colnames(preLoc)))!=0){
            simLoc  <- data.frame(x=preLoc[,1],y=preLoc[,2],group=preLoc[,grep("group",colnames(preLoc))])
          }else{
            simLoc <- data.frame(x=preLoc[,1],y=preLoc[,2])
            simLoc$group <- rep("A",nrow(simLoc))
          }
   
      }else{
        # warnings("Please provide a dataframe with spots location for the boundary estimation")
        print("No dataframe provided, simulate using the square backgound shape")
        pp        <- rpoispp(ns)
        simLoc    <- cbind.data.frame(x=pp[['x']],y=pp[['y']],group=rep("A",pp[["n"]]))
      }
  }

  colnames(simLoc) <- c("x","y","group")
  rownames(simLoc) <- paste0("Loc",1:nrow(simLoc))

  simPara       <- param_tech_func(technique)
  simLoc$mu0    <- rep(simPara$mu,nrow(simLoc))
  simLoc$theta0 <- rep(simPara$theta,nrow(simLoc))
  simLoc$technique <- rep(technique,nrow(simLoc))
  return(simLoc)
}


param_tech_func <- function(ST_tech=c('ST', 'HDST',  'Slideseq','SlideseqV2')){
  ST_tech <- match.arg(ST_tech)
  if(ST_tech=="ST"){
    ST_tech_mu0 <- 0.5
    ST_tech_theta0 <- 0.2
  }else if(ST_tech == "HDST"){
    ST_tech_mu0 <- 6e-5
    ST_tech_theta0 <- 1
  }else if(ST_tech == "Slideseq"){
    ST_tech_mu0 <- 1e-3
    ST_tech_theta0 <- 3.5
  }else if(ST_tech == "SlideseqV2"){
    ST_tech_mu0 <- 5e-3
    ST_tech_theta0 <- 1
  }
  return(list(mu=ST_tech_mu0,theta=ST_tech_theta0))
}


pattern_count_func <- function(pattern_in,numSignal=100,numBG=0,verbose=FALSE,isid=1){
  set.seed(isid)
  numSpots <- nrow(pattern_in)
  numGenes <- numSignal + numBG

  # cat(paste0("\n## ========== Spatial Simulator ===========\n"))
  # cat(paste0("## Number of Locations: ", numSpots,"\n"))
  # cat(paste0("## Number of Signal Genes to Simulate: ", numSignal,"\n"))
  # cat(paste0("## Number of Noise Genes to Simulate: ", numBG,"\n"))
  # cat(paste0("## ========================================\n \n"))

  if(verbose) {message("Simulating Signal Genes...")}
  if(numSignal!=0){
    signal_df <- t(sapply(1:numSignal,function(x){rnbinom(n=numSpots,size=1/pattern_in$theta,mu=pattern_in$mu0)}))
    rownames(signal_df) <- c(paste0("signal",1:numSignal))
  }else{
    signal_df <- NULL
  }

  if(numBG!=0){
    if (verbose) {message("Simulating Noise Genes...")}
    technique <- unique(pattern_in$technique)   
    if(length(technique)>1){
      warning("The spatial transcriptomics technique is not unique, used the first one for background gene simulation")
      technique <- pattern_in$technique[1]
    }
    noise_para  <- param_tech_func(technique)
    noise_df    <- t(sapply(1:numBG,function(x){rnbinom(n=numSpots,size=1/(noise_para$theta),mu=noise_para$mu)}))
    rownames(noise_df) <- c(paste0("noise",1:numBG))
    count_out   <- rbind.data.frame(signal_df,noise_df)
    # rownames(count_out) <- c(paste0("signal",1:numSignal),paste0("noise",1:numBG))
  }else{
    count_out <- signal_df
    # rownames(count_out) <- c(paste0("signal",1:numSignal))
  }
  colnames(count_out) <- rownames(pattern_in)
  return(count_out)
}



relative_func <- function(rx){
  rexpr   = (rx-min(rx))/(max(rx)-min(rx))
  return(rexpr)
}


