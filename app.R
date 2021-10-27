library(shiny)
library(dplyr)
library(ggplot2)
library(keys)

hotkeys <- c("a","s","d","q","e")

ui <- fluidPage(
    titlePanel("tetRis!"),
    sidebarLayout(
        sidebarPanel(
            actionButton("left","Left (a)")
            ,actionButton("right","Right (d)")
            ,actionButton("drop","Drop (s)")
            ,actionButton("rCw","Rotate Clockwise (e)")
            ,actionButton("rACw","Rotate Anti-Clockwise (q)")
            ,actionButton("reset","New game")
            ,textOutput("score")
            ,textOutput("level")
            ,h4("Next block:")
            ,plotOutput("nextBlock")
            ,h6("Welcome to shiny tetris! The game can be played using either the action buttons above, or the keyboard shortcuts (shown in brackets).")
        ),
        mainPanel(
           textOutput("clockface")
           ,plotOutput("frame")
           ,useKeys()
           ,keysInput("keys",hotkeys)
        )
    )
)

server <- function(input, output, session) {

    #TBC: investigate issue where drop down after scoring is causing scoring on a new line below
    keypress <- reactiveValues(
        a = 0
        ,s = 0
        ,d = 0
        ,q = 0
        ,e = 0
    )
    observeEvent(input$keys,{
        if(input$keys == "a"){keypress$a <- keypress$a + 1}
        if(input$keys == "s"){keypress$s <- keypress$s + 1}
        if(input$keys == "d"){keypress$d <- keypress$d + 1}
        if(input$keys == "q"){keypress$q <- keypress$q + 1}
        if(input$keys == "e"){keypress$e <- keypress$e + 1}
    },ignoreInit = TRUE)
    observeEvent(input$left,{keypress$a <- keypress$a + 1},ignoreInit = TRUE)
    observeEvent(input$right,{keypress$d <- keypress$d + 1},ignoreInit = TRUE)
    observeEvent(input$drop,{keypress$s <- keypress$s + 1},ignoreInit = TRUE)
    observeEvent(input$rCw,{keypress$e <- keypress$e + 1},ignoreInit = TRUE)
    observeEvent(input$rACw,{keypress$q <- keypress$q + 1},ignoreInit = TRUE)

    source("tetrominoes.r")
    frame <- reactiveValues(
        df = data.frame(
            x = sort(rep(10:1,26))
            ,y = rep(26:1,10)
            ,data = "grey"
            ,shape = NA
            ,stringsAsFactors = FALSE
        )
    )

    startingShape <- sample(1:7,1)

    # frame <- reactiveValues( #this frame is for testing a nearly complete row on the bottom
    #     df = data.frame(
    #         x = sort(rep(10:1,26))
    #         ,y = rep(26:1,10)
    #         ,data = c(rep(c(rep("grey",22),"blue","blue","blue","blue"),9),rep("grey",26))
    #         ,shape = NA
    #         ,shapeCounter = NA
    #         ,stringsAsFactors = FALSE
    #     )
    # )

    score <- reactiveValues(n = 0, level = 1)

    colours <- c("blue","red","yellow","green","pink","orange","purple")
    tick <- reactiveValues(pause = FALSE)
    clockTime <- reactiveValues(time = 0,invalidate = 1000)
    activeBlock <- reactiveValues(
        blockNumber = 1
        ,blockCell = c(5, 25)
        ,orient = 1
        ,nOrient = tetrominoes[[startingShape]]$n.orient
        ,shape = startingShape
        ,nextShape = sample(1:7,1)
        ,block2 = c(tetrominoes[[startingShape]]$orient1$b2[1],tetrominoes[[startingShape]]$orient1$b2[2])
        ,block3 = c(tetrominoes[[startingShape]]$orient1$b3[1],tetrominoes[[startingShape]]$orient1$b3[2])
        ,block4 = c(tetrominoes[[startingShape]]$orient1$b4[1],tetrominoes[[startingShape]]$orient1$b4[2])
        ,previousBlockCell = NA
        ,blockColour = colours[startingShape]
        ,shapeCounter = 1
    )


    pulseTimer <- reactiveValues(n = 0,enable = FALSE)
    observe({
        invalidateLater(100,session)
        if(isolate(pulseTimer$enable)){
            pulseTimer$n <- isolate(pulseTimer$n) + 1
        }
    })

    observeEvent(score$n,{
        if(score$n%%1000 == 0){
            if(clockTime$invalidate > 200){
                clockTime$invalidate <- clockTime$invalidate - 75
            }
            score$level <- score$level + 1
        }
    },ignoreInit =  TRUE)

    observe({
        invalidateLater(isolate(clockTime$invalidate),session)
        if(!(tick$pause)){
            clockTime$time <- isolate(clockTime$time) + 1
            canDescend <- FALSE
            if(isolate(activeBlock$blockCell)[2] > 1){
                activeCols <- c(
                    isolate(activeBlock$blockCell)[1]
                    ,isolate(activeBlock$blockCell)[1] + isolate(activeBlock$block2)[1]
                    ,isolate(activeBlock$blockCell)[1] + isolate(activeBlock$block3)[1]
                    ,isolate(activeBlock$blockCell)[1] + isolate(activeBlock$block4)[1]
                )
                activeRows <- c(
                    isolate(activeBlock$blockCell)[2]
                    ,isolate(activeBlock$blockCell)[2] + isolate(activeBlock$block2)[2]
                    ,isolate(activeBlock$blockCell)[2] + isolate(activeBlock$block3)[2]
                    ,isolate(activeBlock$blockCell)[2] + isolate(activeBlock$block4)[2]
                )
                uniqueCols <- unique(activeCols)
                minRowCheck <- rep(NA,length(uniqueCols))
                for(n in 1:length(uniqueCols)){
                    minRowCheck[n] <- min(
                        activeRows[activeCols == uniqueCols[n]]
                    )
                }
                freeCells <- rep(NA,length(minRowCheck))
                for(n in 1:length(minRowCheck)){
                    test <-    isolate(frame$df$data)[isolate(frame$df$x)==uniqueCols[n] & isolate(frame$df$y) == minRowCheck[n] - 1] == "grey"
                    if(length(test) == 1){
                        freeCells[n] <- test
                    } else {
                        freeCells[n] <- TRUE
                    }
                }
                if(any(activeRows == 1)){
                    canDescend <- FALSE
                } else if(all(freeCells)){
                    canDescend <- TRUE
                }
            }
            if(canDescend){
                activeBlock$previousBlockCell <- isolate(activeBlock$blockCell)
                activeBlock$blockCell <- c(isolate(activeBlock$blockCell[1]),isolate(activeBlock$blockCell[2])-1)
            } else {
                localFrame <- isolate(frame$df)
                r25count <- length(localFrame$data[localFrame$data != "grey" & localFrame$y == 25])
                if(r25count > 0){
                    tick$pause <- TRUE
                    showModal(
                        modalDialog(
                            title = "Game over!"
                            ,h3("Better luck next time!")
                            ,h4(paste0("Final score: ",score$n))
                        )
                    )
                } else {
                # Check for completed rows
                    completeRows <- localFrame %>%
                        filter(data != "grey") %>%
                        group_by(y) %>%
                        summarise(count = n()) %>%
                        ungroup() %>%
                        filter(count == 10) %>%
                        arrange(desc(y))

                    if(nrow(completeRows) > 0){
                        pulseTimer$enable <- TRUE
                        tick$pause <- TRUE
                        for(n in 1:nrow(completeRows)){
                            localFrame$data[localFrame$y %in% completeRows$y] <- "white"
                        }
                        frame$df <- localFrame
                    } else {

                    activeBlock$shape <- activeBlock$nextShape
                    activeBlock$nextShape <- sample(1:7,1)
                    activeBlock$block2 <- c(tetrominoes[[isolate(activeBlock$shape)]]$orient1$b2[1],tetrominoes[[isolate(activeBlock$shape)]]$orient1$b2[2])
                    activeBlock$block3 <- c(tetrominoes[[isolate(activeBlock$shape)]]$orient1$b3[1],tetrominoes[[isolate(activeBlock$shape)]]$orient1$b3[2])
                    activeBlock$block4 <- c(tetrominoes[[isolate(activeBlock$shape)]]$orient1$b4[1],tetrominoes[[isolate(activeBlock$shape)]]$orient1$b4[2])
                    activeBlock$previousBlockCell <- NA
                    activeBlock$blockCell <- c(5,25)
                    activeBlock$blockNumber <- isolate(activeBlock$blockNumber + 1)
                    activeBlock$blockColour <- colours[activeBlock$shape]
                    activeBlock$nOrient <- tetrominoes[[isolate(activeBlock$shape)]]$n.orient
                    activeBlock$orient <- 1
                    activeBlock$shapeCounter <- activeBlock$shapeCounter + 1
                    }
                }
            }
        }
    })

    observeEvent(pulseTimer$n,{
        if(pulseTimer$n == 3){
            localFrame <- isolate(frame$df)
            completeRows <- localFrame %>%
                filter(data != "grey") %>%
                group_by(y) %>%
                summarise(count = n()) %>%
                ungroup() %>%
                filter(count == 10) %>%
                arrange(desc(y))
            if(nrow(completeRows) > 0){
                for(n in 1:nrow(completeRows)){
                    a <- completeRows$y[n]
                    localFrame$y[localFrame$y == a] <- -1
                    localFrame$y[localFrame$y > a] <- localFrame$y[localFrame$y > a] - 1
                    localFrame <- localFrame[localFrame$y > 0,]
                    localFrame <- rbind(
                        localFrame
                        ,data.frame(
                            x = 1:10
                            ,y = rep(26,10)
                            ,data = "grey"
                            ,shape = NA
                            ,shapeCounter = NA
                        )
                    )
                }
            }
            frame$df <- localFrame
            score$n <- score$n + 100 * nrow(completeRows)
            tick$pause <- FALSE
            pulseTimer$n <- 0
            pulseTimer$enable <- FALSE
        }
    })

    observeEvent(keypress$a,{
        if(activeBlock$blockCell[1] > 1){
            activeRows <- c(
                activeBlock$blockCell[2]
                ,activeBlock$blockCell[2] + activeBlock$block2[2]
                ,activeBlock$blockCell[2] + activeBlock$block3[2]
                ,activeBlock$blockCell[2] + activeBlock$block4[2]
            )
            activeCols <- c(
                activeBlock$blockCell[1]
                ,activeBlock$blockCell[1] + activeBlock$block2[1]
                ,activeBlock$blockCell[1] + activeBlock$block3[1]
                ,activeBlock$blockCell[1] + activeBlock$block4[1]
            )
            uniqueRows <- unique(activeRows)
            emptySpace <- logical(0)
            for(n in uniqueRows){
                leftMostColumn <- min(activeCols[activeRows == n])
                test <- if(leftMostColumn == 1){
                    FALSE
                } else {
                    if(frame$df$data[frame$df$y == n & frame$df$x == (leftMostColumn-1)] == "grey"){
                        TRUE
                    } else {
                        FALSE
                    }
                }
                emptySpace[length(emptySpace) + 1] <- test
            }
            canMoveLeft <- all(emptySpace)
            if(canMoveLeft){
                activeBlock$previousBlockCell <- activeBlock$blockCell
                activeBlock$blockCell <- c(activeBlock$blockCell[1]-1,activeBlock$blockCell[2])
            }
        }
    })

    observeEvent(keypress$d,{
        if(activeBlock$blockCell[1] < 10){
            activeRows <- c(
                activeBlock$blockCell[2]
                ,activeBlock$blockCell[2] + activeBlock$block2[2]
                ,activeBlock$blockCell[2] + activeBlock$block3[2]
                ,activeBlock$blockCell[2] + activeBlock$block4[2]
            )
            activeCols <- c(
                activeBlock$blockCell[1]
                ,activeBlock$blockCell[1] + activeBlock$block2[1]
                ,activeBlock$blockCell[1] + activeBlock$block3[1]
                ,activeBlock$blockCell[1] + activeBlock$block4[1]
            )
            uniqueRows <- unique(activeRows)
            emptySpace <- logical(0)
            for(n in uniqueRows){
                rightMostColumn <- max(activeCols[activeRows == n])
                test <- if(rightMostColumn == 10){
                    FALSE
                } else {
                    if(frame$df$data[frame$df$y == n & frame$df$x == (rightMostColumn+1)] == "grey"){
                        TRUE
                    } else {
                        FALSE
                    }
                }
                emptySpace[length(emptySpace) + 1] <- test
            }
            canMoveRight <- all(emptySpace)
            if(canMoveRight){
                activeBlock$previousBlockCell <- activeBlock$blockCell
                activeBlock$blockCell <- c(activeBlock$blockCell[1]+1,activeBlock$blockCell[2])
            }
        }
    })

    observeEvent(keypress$s,{
        if(keypress$s > 0){
            activeCols <- c(
                activeBlock$blockCell[1]
                ,activeBlock$blockCell[1] + activeBlock$block2[1]
                ,activeBlock$blockCell[1] + activeBlock$block3[1]
                ,activeBlock$blockCell[1] + activeBlock$block4[1]
            )
            activeRows <- c(
                activeBlock$blockCell[2]
                ,activeBlock$blockCell[2] + activeBlock$block2[2]
                ,activeBlock$blockCell[2] + activeBlock$block3[2]
                ,activeBlock$blockCell[2] + activeBlock$block4[2]
            )
            uniqueCols <- unique(activeCols)
            candidates <- frame$df$y[frame$df$x %in% uniqueCols & frame$df$data != "grey" & frame$df$y < min(activeRows)]
            if(length(candidates) == 0){candidates <- 1}
            highestOccupied <- max(candidates)
            activeBlock$previousBlockCell <- activeBlock$blockCell
            n <- if(activeBlock$blockCell[2] != min(activeRows)){1+activeBlock$blockCell[2]-min(activeRows)}else{1}
            activeBlock$blockCell <- c(activeBlock$blockCell[1],highestOccupied+n)
        }
    })

    # TBC: add rotation commands

    observeEvent(keypress$e,{
        if(activeBlock$shape != 6){
            currentLocation <- activeBlock$blockCell
            previouslocation <- activeBlock$previousBlockCell
            maxOrient <- activeBlock$nOrient
            moveToOrient <- paste0("orient",if(activeBlock$orient == maxOrient){1}else{activeBlock$orient+1})
            holdframe <- frame$df
            previousLocations <- data.frame(
                x = c(activeBlock$blockCell[1]
                      ,activeBlock$blockCell[1] + activeBlock$block2[1]
                      ,activeBlock$blockCell[1] + activeBlock$block3[1]
                      ,activeBlock$blockCell[1] + activeBlock$block4[1])
                ,y = c(activeBlock$blockCell[2]
                       ,activeBlock$blockCell[2] + activeBlock$block2[2]
                       ,activeBlock$blockCell[2] + activeBlock$block3[2]
                       ,activeBlock$blockCell[2] + activeBlock$block4[2])
            )
            newLocations <- data.frame(
                x = c(activeBlock$blockCell[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[1])
                ,y = c(activeBlock$blockCell[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[2])
            )
            free2 <- any(holdframe$data[holdframe$x == newLocations$x[2] & holdframe$y == newLocations$y[2]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[2] & holdframe$y == newLocations$y[2]] == activeBlock$shapeCounter)
            free3 <- any(holdframe$data[holdframe$x == newLocations$x[3] & holdframe$y == newLocations$y[3]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[3] & holdframe$y == newLocations$y[3]] == activeBlock$shapeCounter)
            free4 <- any(holdframe$data[holdframe$x == newLocations$x[4] & holdframe$y == newLocations$y[4]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[4] & holdframe$y == newLocations$y[4]] == activeBlock$shapeCounter)
            if(is.na(free2)){free2 <- FALSE}
            if(is.na(free3)){free3 <- FALSE}
            if(is.na(free4)){free4 <- FALSE}
            #print(paste0("free2: ",free2,",free3: ",free3,",free4: ",free4))
            canRotate <- all(free2,free3,free4)
            if(canRotate){
                activeBlock$block2 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[2])
                activeBlock$block3 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[2])
                activeBlock$block4 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[2])
                activeBlock$orient <- as.integer(gsub("orient","",moveToOrient))
                currentLocations <- data.frame(
                    x = c(activeBlock$blockCell[1]
                          ,activeBlock$blockCell[1] + activeBlock$block2[1]
                          ,activeBlock$blockCell[1] + activeBlock$block3[1]
                          ,activeBlock$blockCell[1] + activeBlock$block4[1])
                    ,y = c(activeBlock$blockCell[2]
                           ,activeBlock$blockCell[2] + activeBlock$block2[2]
                           ,activeBlock$blockCell[2] + activeBlock$block3[2]
                           ,activeBlock$blockCell[2] + activeBlock$block4[2])
                )
                if(!is.na(previouslocation[1])){
                    for(n in 1:4){
                        holdframe$shape[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
                        holdframe$data[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- "grey"
                        holdframe$shapeCounter[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
                    }
                }
                for(n in 1:4){
                    holdframe$shape[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockNumber
                    holdframe$data[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockColour
                    holdframe$shapeCounter[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$shapeCounter
                }
                frame$df <- holdframe
            }
        }
    })

    observeEvent(keypress$q,{
        if(activeBlock$shape != 6){
            currentLocation <- activeBlock$blockCell
            previouslocation <- activeBlock$previousBlockCell
            maxOrient <- activeBlock$nOrient
            moveToOrient <- paste0("orient",if(activeBlock$orient == 1){maxOrient}else{activeBlock$orient-1})
            holdframe <- frame$df
            previousLocations <- data.frame(
                x = c(activeBlock$blockCell[1]
                      ,activeBlock$blockCell[1] + activeBlock$block2[1]
                      ,activeBlock$blockCell[1] + activeBlock$block3[1]
                      ,activeBlock$blockCell[1] + activeBlock$block4[1])
                ,y = c(activeBlock$blockCell[2]
                       ,activeBlock$blockCell[2] + activeBlock$block2[2]
                       ,activeBlock$blockCell[2] + activeBlock$block3[2]
                       ,activeBlock$blockCell[2] + activeBlock$block4[2])
            )
            newLocations <- data.frame(
                x = c(activeBlock$blockCell[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[1]
                      ,activeBlock$blockCell[1] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[1])
                ,y = c(activeBlock$blockCell[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[2]
                       ,activeBlock$blockCell[2] + tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[2])
            )
            free2 <- any(holdframe$data[holdframe$x == newLocations$x[2] & holdframe$y == newLocations$y[2]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[2] & holdframe$y == newLocations$y[2]] == activeBlock$shapeCounter)
            free3 <- any(holdframe$data[holdframe$x == newLocations$x[3] & holdframe$y == newLocations$y[3]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[3] & holdframe$y == newLocations$y[3]] == activeBlock$shapeCounter)
            free4 <- any(holdframe$data[holdframe$x == newLocations$x[4] & holdframe$y == newLocations$y[4]] == "grey",holdframe$shapeCounter[holdframe$x == newLocations$x[4] & holdframe$y == newLocations$y[4]] == activeBlock$shapeCounter)
            if(is.na(free2)){free2 <- FALSE}
            if(is.na(free3)){free3 <- FALSE}
            if(is.na(free4)){free4 <- FALSE}
            #print(paste0("free2: ",free2,",free3: ",free3,",free4: ",free4))
            canRotate <- all(free2,free3,free4)
            if(canRotate){
                activeBlock$block2 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b2[2])
                activeBlock$block3 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b3[2])
                activeBlock$block4 = c(tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[1],tetrominoes[[isolate(activeBlock$shape)]][[moveToOrient]]$b4[2])
                activeBlock$orient <- as.integer(gsub("orient","",moveToOrient))
                currentLocations <- data.frame(
                    x = c(activeBlock$blockCell[1]
                          ,activeBlock$blockCell[1] + activeBlock$block2[1]
                          ,activeBlock$blockCell[1] + activeBlock$block3[1]
                          ,activeBlock$blockCell[1] + activeBlock$block4[1])
                    ,y = c(activeBlock$blockCell[2]
                           ,activeBlock$blockCell[2] + activeBlock$block2[2]
                           ,activeBlock$blockCell[2] + activeBlock$block3[2]
                           ,activeBlock$blockCell[2] + activeBlock$block4[2])
                )
                if(!is.na(previouslocation[1])){
                    for(n in 1:4){
                        holdframe$shape[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
                        holdframe$data[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- "grey"
                        holdframe$shapeCounter[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
                    }
                }
                for(n in 1:4){
                    holdframe$shape[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockNumber
                    holdframe$data[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockColour
                    holdframe$shapeCounter[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$shapeCounter
                }
                frame$df <- holdframe
            }
        }
    })

    output$clockface <- renderText({
        clockTime$time
    })

    observeEvent(activeBlock$blockCell,{
        currentLocation <- activeBlock$blockCell
        previouslocation <- activeBlock$previousBlockCell

        holdframe <- frame$df
        previousLocations <- data.frame(
            x = c(activeBlock$previousBlockCell[1]
                  ,activeBlock$previousBlockCell[1] + activeBlock$block2[1]
                  ,activeBlock$previousBlockCell[1] + activeBlock$block3[1]
                  ,activeBlock$previousBlockCell[1] + activeBlock$block4[1])
            ,y = c(activeBlock$previousBlockCell[2]
                   ,activeBlock$previousBlockCell[2] + activeBlock$block2[2]
                   ,activeBlock$previousBlockCell[2] + activeBlock$block3[2]
                   ,activeBlock$previousBlockCell[2] + activeBlock$block4[2])
        )
        currentLocations <- data.frame(
            x = c(activeBlock$blockCell[1]
                  ,activeBlock$blockCell[1] + activeBlock$block2[1]
                  ,activeBlock$blockCell[1] + activeBlock$block3[1]
                  ,activeBlock$blockCell[1] + activeBlock$block4[1])
            ,y = c(activeBlock$blockCell[2]
                   ,activeBlock$blockCell[2] + activeBlock$block2[2]
                   ,activeBlock$blockCell[2] + activeBlock$block3[2]
                   ,activeBlock$blockCell[2] + activeBlock$block4[2])
        )
        if(!is.na(previouslocation[1])){
            for(n in 1:4){
                holdframe$shape[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
                holdframe$data[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- "grey"
                holdframe$shapeCounter[holdframe$x == previousLocations$x[n] & holdframe$y == previousLocations$y[n]] <- NA
            }
        }
        for(n in 1:4){
            holdframe$shape[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockNumber
            holdframe$data[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$blockColour
            holdframe$shapeCounter[holdframe$x == currentLocations$x[n] & holdframe$y == currentLocations$y[n]] <- activeBlock$shapeCounter
        }
        frame$df <- holdframe
    })


    output$frame <- renderPlot({
        ggplot(frame$df,aes(x, y, fill= data, col = "grey")) +
            geom_tile(show.legend = FALSE) +
            scale_color_identity() +
            scale_y_continuous(breaks = 0:25) +
            scale_fill_identity() +
            coord_cartesian(expand = FALSE,ylim = c(0.5,24.5)) +
            theme(axis.line=element_blank()
                  ,axis.text.x=element_blank()
                  ,axis.text.y=element_blank()
                  ,axis.ticks=element_blank()
                  ,axis.title.x=element_blank()
                  ,axis.title.y=element_blank()
                  ,legend.position="none"
                  ,panel.background=element_blank()
                  ,panel.border=element_blank()
                  ,panel.grid.major=element_blank()
                  ,panel.grid.minor=element_blank()
                  ,plot.background=element_blank()
                  ,plot.margin=unit(c(0,0,0,0),"cm")
            )
    }, height = 2400/3, width = 1000/3)

    output$score <- renderText(paste0("Score: ",score$n))
    output$level <- renderText(paste0("Level ",score$level))
    output$speed <- renderText(clockTime$invalidate)

    observeEvent(input$reset,{
        tick$pause <- TRUE
        frame$df <- data.frame(
                    x = sort(rep(10:1,26))
                    ,y = rep(26:1,10)
                    ,data = "grey"
                    ,shape = NA
                    ,stringsAsFactors = FALSE
                )
        score$n <- 0
        score$level <- 1
        clockTime$time <- 0
        clockTime$invalidate <- 1000
        startingShape <- sample(1:7,1)
        activeBlock$blockNumber = 1
        activeBlock$blockCell = c(5, 25)
        activeBlock$orient = 1
        activeBlock$nOrient = tetrominoes[[startingShape]]$n.orient
        activeBlock$shape = startingShape
        activeBlock$nextShape = sample(1:7,1)
        activeBlock$block2 = c(tetrominoes[[startingShape]]$orient1$b2[1],tetrominoes[[startingShape]]$orient1$b2[2])
        activeBlock$block3 = c(tetrominoes[[startingShape]]$orient1$b3[1],tetrominoes[[startingShape]]$orient1$b3[2])
        activeBlock$block4 = c(tetrominoes[[startingShape]]$orient1$b4[1],tetrominoes[[startingShape]]$orient1$b4[2])
        activeBlock$previousBlockCell = NA
        activeBlock$blockColour = colours[startingShape]
        activeBlock$shapeCounter = 1
        tick$pause <- FALSE
    })

    output$nextBlock <- renderPlot({
        shape <- activeBlock$nextShape
        nextShapeFrame <- data.frame(
            x = rep(1:4,4)
            ,y = sort(rep(1:4,4))
            ,colour = "grey"
            ,stringsAsFactors = FALSE
        )
        N <- if(shape == 6){2}else{3}
        shadeRows <- c(N,N+tetrominoes[[shape]]$orient1$b2[1],N+tetrominoes[[shape]]$orient1$b3[1],N+tetrominoes[[shape]]$orient1$b4[1])
        shadeCols <- c(N,N+tetrominoes[[shape]]$orient1$b2[2],N+tetrominoes[[shape]]$orient1$b3[2],N+tetrominoes[[shape]]$orient1$b4[2])
        for(n in 1:4){
            nextShapeFrame$colour[nextShapeFrame$x == shadeRows[n] & nextShapeFrame$y == shadeCols[n]] <- colours[shape]
        }
        ggplot(nextShapeFrame,aes(x, y, fill= colour, col = "grey")) +
            geom_tile(show.legend = FALSE) +
            scale_color_identity() +
            scale_y_continuous(breaks = 0:3) +
            scale_fill_identity() +
            coord_cartesian(expand = FALSE,ylim = c(0.5,4.5)) +
            theme(axis.line=element_blank()
                  ,axis.text.x=element_blank()
                  ,axis.text.y=element_blank()
                  ,axis.ticks=element_blank()
                  ,axis.title.x=element_blank()
                  ,axis.title.y=element_blank()
                  ,legend.position="none"
                  ,panel.background=element_blank()
                  ,panel.border=element_blank()
                  ,panel.grid.major=element_blank()
                  ,panel.grid.minor=element_blank()
                  ,plot.background=element_blank()
                  ,plot.margin=unit(c(0,0,0,0),"cm")
            )
    }, height = 200, width = 200)
}

# Run the application
shinyApp(ui = ui, server = server)
