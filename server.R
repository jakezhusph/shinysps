function(input, output, session){
  ## This function is repsonsible for loading in the selected file
  filedata <- reactive({
    inFile <- input$datafile
    if(is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = input$header,row.names=as.numeric(input$rownames))
  })

  newLocdata <- reactive({
    get_new_coordinates(
      ns=input$numloc, bg_shape=input$shape,
      isid=input$sid,technique=input$technique,
      lay_out=input$arrange,preLoc=filedata()
    )
  })

  newCountdata <- reactive({
    pattern_count_func(
      pattern_in=locdf, numSignal=input$numSig,
      numBG = input$numNoise,isid=input$count_sid)
  })

  newpara <- reactive({
    param_tech_func(ST_tech=input$technique)
  })

  observeEvent(input$reload, {
    session$reload()
  })

  output$plot1 <- renderPlotly({
    locdf <<- newLocdata()
    g1 <- newLocdata()
    gg_scatter <- ggplot(data=g1,aes(x, y,color=group)) + geom_point(size=input$ptsize) + xlab("Coordinate 1") + ylab("Coordinate 2")+ 
                  theme(legend.title=element_blank(),text = element_text(size=input$textsize))+ 
                  coord_fixed(ratio = 1)
                  # coord_fixed(ratio = 1,xlim=range(locdf$x),ylim=range(locdf$y),expand=FALSE)
    # ggplotly(gg_scatter) %>% layout(dragmode = 'select',legend = list(title=list(text='<b> Group </b>',side="top"),orientation = "h", y=-0.05))
    ggplotly(gg_scatter, height = 600, width= 1.1*600*(diff(range(g1$x))/diff(range(g1$y)))) %>% layout(dragmode = 'select',legend = list(title=list(text='<b> Group </b>',side="top"),orientation = "v", x=1.05))
    # ggplotly(gg_scatter) %>% layout(dragmode = 'select',legend = list(title=list(text='<b> Group </b>',side="top"),orientation = "v", x=1.05))
  })

  output$brush4 <- renderText({
    if(!input$technique %in% locdf$technique){
      print(paste0("The new selected technique is different from the original setup, consider restarting the pattern design or change back to ",unique(locdf$technique)))
    }
  })

  ## initial table
  output$summary_table <- renderTable({
    g1 <- newLocdata()
    summary_df <- cbind.data.frame(aggregate(g1$mu0~g1$group,FUN=length),
                            aggregate(g1$mu0~g1$group,FUN=mean)[,2])
    colnames(summary_df) <- c("Group","NumSpots","Mu")
    summary_df$PropSpots <- round(summary_df$NumSpots/sum(summary_df$NumSpots),3)
    return(summary_df[,c("Group","NumSpots","PropSpots","Mu")])
  },digits=3)

  output$brush <- renderPrint({
    if(is.null(filedata())&(input$shape %in% c("User Define Shape","User Define Spots"))){
        cat("!!! Warning: No dataframe provided, simulate using the square backgound shape \n")
    }
    g1 <- newLocdata()
    group <- g1[,3]
    names(group) <- rownames(g1)
    d  <- event_data('plotly_selected')
    if (is.null(d)){
      cat("Select points (i.e., box/lasso) to define new group")
    }else{
      dd <- round(cbind(d[[3]],d[[4]]),3)
      vv <- group[which(round(g1[,1],3) %in% dd[,1] & round(g1[,2],3) %in% dd[,2])]
      vv <<- vv
      cat(paste0("Number of points selected: ",length(vv)," (",100*round(length(vv)/length(group),3),"%)"))
    } 
  })

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(locdf,input$maxrows), row.names = TRUE)
    options(orig)
  })

  output$distTable <- renderDataTable({
      locdf
    }, 
    options = list(pageLength=10)
  )

  output$SpotPlot <- renderPlot({
    gg_scatter_spot <- ggplot(data=locdf,aes(x, y,color=group)) + 
                geom_point(size=input$ptsizeCount) + 
                xlab("Coordinate 1") + ylab("Coordinate 2")+ 
                # ggtitle(paste0("Spots Spatial Pattern")) + 
                theme(legend.title=element_blank(),text = element_text(size=input$textsizeCount))+ coord_fixed()
    ptlist <- list(gg_scatter_spot)
    ggarrange(plotlist=ptlist,ncol=length(ptlist))
  })



  observeEvent(input$Change > 0, {
    simPara <- newpara()
    if (!is.null(vv)) {
      locdf[which(row.names(locdf) %in% names(vv)),]$group <<- input$NewGroup
      locdf[which(row.names(locdf) %in% names(vv)),]$mu0 <<- simPara$mu * input$fc
      output$plot1 <- renderPlotly({
        gg_scatter <- ggplot(data=locdf,aes(x, y,color=group)) + geom_point(size=input$ptsize) + 
                      xlab("Coordinate 1") + ylab("Coordinate 2") + 
                      theme(legend.title=element_blank(),text = element_text(size=input$textsize)) + 
                      coord_fixed(ratio = 1)
        ggplotly(gg_scatter,height = 600, width= 1.1*600*(diff(range(locdf$x))/diff(range(locdf$y)))) %>% layout(dragmode = 'select',legend = list(title=list(text='<b> Group </b>',side="top"),orientation = "v", x=1.05))
      })

      ## check if there is multiple effects in the same group
      output$brush3 <- renderText({
        check_multi <- aggregate(locdf$mu0~locdf$group,FUN=unique)
        if(length(grep(",",check_multi[,2]))!=0){
           print(paste0("Group ", check_multi[,1][grep(",",check_multi[,2])]," has more than one mu, consider redefining the effect size for the group"))
        }
      })

      ## update display of the locdf after the group assignment
      output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(head(locdf,input$maxrows), row.names = TRUE)
        options(orig)
      })

      ## update pop display of the locdf after the group assignment
      output$distTable <- renderDataTable({
          locdf
        }, 
        options = list(pageLength=10)
      )

      output$summary_table <- renderTable({
        summary_df <- cbind.data.frame(aggregate(locdf$mu0~locdf$group,FUN=length),
                                  aggregate(locdf$mu0~locdf$group,FUN=mean)[,2])
        colnames(summary_df) <- c("Group","NumSpots","Mu")
        summary_df$PropSpots <- round(summary_df$NumSpots/sum(summary_df$NumSpots),3)
        # return(summary_df[,c("Group","NumSpots","PropSpots","Mu")])
        summary_df[,c("Group","NumSpots","PropSpots","Mu")]
      },digits=3)

      output$SpotPlot <- renderPlot({
        gg_scatter_spot <- ggplot(data=locdf,aes(x, y,color=group)) + 
                    geom_point(size=input$ptsizeCount) + 
                    xlab("Coordinate 1") + ylab("Coordinate 2")+ 
                    # ggtitle(paste0("Spots Spatial Pattern")) + 
                    theme(legend.title=element_blank(),text = element_text(size=input$textsizeCount))+ coord_fixed()
        ptlist <- list(gg_scatter_spot)
        ggarrange(plotlist=ptlist,ncol=length(ptlist))
      })

    } ## end of the is.null if
    vv <<- NULL
  })  

  ## Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {
      if(!is.null(input$datafile)){
          paste(unlist(strsplit(input$datafile,split=".csv")), "_sps_location_seed",input$sid,".csv", sep = "")
        }else{
          paste(input$shape, "_sps_location_seed",input$sid,".csv", sep = "")
        } 
    },
    content = function(file) {
      write.csv(locdf, file, row.names = TRUE)
    }
  )

  ## Downloadable pop csv of selected dataset 
  output$downloadPop <- downloadHandler(
    filename = function() {
      if(!is.null(input$datafile)){
          paste(unlist(strsplit(input$datafile,split=".csv")), "_sps_location_seed",input$sid,".csv", sep = "")
        }else{
          paste(input$shape, "_sps_location_seed",input$sid,".csv", sep = "")
        } 
    },
    content = function(file) {
      write.csv(locdf, file, row.names = TRUE)
    }
  )

  ## without >0, won't run before click
  observeEvent(input$countGenerate, {
    if((input$numSig + input$numNoise)== 0){
      output$countbrush <- renderText({
        print(paste0("Total number of genes to simulate is zero, please change the simulation setting"))
      })
    }else{
      output$countbrush  <- NULL
      output$countInfobrush <- renderPrint({
        cat(paste0("Number of Locations: ", nrow(locdf),"\n"))
        cat(paste0("Number of Signal Genes to Simulate: ", input$numSig,"\n"))
        cat(paste0("Number of Noise Genes to Simulate: ", input$numNoise,"\n"))
      })

      countdf <<- newCountdata()

      ## otherwise, the newPlotdata is not update with the sid
      newPlotdata <- reactive({
        combined_df     <- cbind.data.frame(locdf,apply(countdf,1,relative_func))
        selected_gene   <- c(paste0("signal",input$sigidx),paste0("noise",input$noidx))
        npdf            <- combined_df[,c("x","y","group",selected_gene)]
        names(npdf)     <- c("x","y","group","signal_gene","noise_gene")
        return(npdf)
      })

      ## update pop display of the locdf after the group assignment
      output$countTable <- renderDataTable({
          display_count_df <- cbind.data.frame(GeneID=rownames(countdf),countdf)
          display_count_df[,1:10]
        }, options = list(pageLength=10)
      )

      output$ExpressionPlot <- renderPlot({
        pltdf <- newPlotdata()

        # output$pltdfcheck <- renderDataTable({
        #     pltdf
        #   }, options = list(pageLength=10)
        # )

        if(input$dosignal){
          gg_scatter_signal <- ggplot(data=pltdf,aes(x, y,color=signal_gene)) + 
              geom_point(size=input$ptsizeCount)+ 
              scale_color_viridis(option="D",direction=-1) + 
              xlab("Coordinate 1") + ylab("Coordinate 2")+ 
              ggtitle(paste0("Signal Gene #",input$sigidx)) + 
              theme(legend.title=element_blank(),text = element_text(size=input$textsizeCount))+ coord_fixed()
        }else{
          gg_scatter_signal <- NULL
        }

        if(input$donoise){
          gg_scatter_noise <- ggplot(data=pltdf,aes(x, y,color=noise_gene)) + 
                          geom_point(size=input$ptsizeCount)+ 
                          scale_color_viridis(option="D",direction=-1) + 
                          xlab("Coordinate 1") + ylab("Coordinate 2")+ 
                          ggtitle(paste0("Noise Gene #",input$noidx)) + 
                          theme(legend.title=element_blank(),text = element_text(size=input$textsizeCount)) + coord_fixed()
        }else{
          gg_scatter_noise <- NULL
        }

        ptlist    <- list(gg_scatter_signal,gg_scatter_noise)
        to_delete <- !sapply(ptlist,is.null)
        ptlist    <- ptlist[to_delete] 
        if(length(ptlist)==0){
          return(NULL)
        } else if(length(ptlist)==2){
          ggarrange(plotlist=ptlist,ncol=length(ptlist), common.legend = TRUE, legend="right")
        }else{
          ggarrange(plotlist=ptlist,ncol=length(ptlist))
        }
      })
    }
  })  


  ## Downloadable pop csv of selected dataset 
  output$downloadPopCount <- downloadHandler(
    filename = function() {
      if(!is.null(input$datafile)){
          paste(unlist(strsplit(input$datafile,split=".csv")), "_sps_count_seed",input$sid,".csv", sep = "")
        }else{
          paste(input$shape, "_sps_count_seed",input$sid,".csv", sep = "")
        } 
    },
    content = function(file) {
      write.csv(countdf, file, row.names = TRUE)
    }
  )

  ## Quit the app and get back to the R
  observeEvent(input$exit,{
    stopApp()
  })
} ## end of server

