library(tcltk)


{ # ---TCLTK AND KEY BINDINGS
    current_key <- ""
    CONTROLLER <- tktoplevel()                                                          # Create a window to capture key presses
    tkwm.geometry(CONTROLLER, "1x1+-2000+-2000")                                        # Move it off screen
    tkwm.title(CONTROLLER, "Key press reader")                                          # Set the title
    tkwm.overrideredirect(CONTROLLER, TRUE)                                             # Remove the window decorations
    tkwm.deiconify(CONTROLLER)                                                          # Deiconify the window
    tkraise(CONTROLLER)                                                                 # Raise the window
    tkfocus("-force", CONTROLLER)                                                       # Force focus on the window
    tkbind(CONTROLLER, "<w>", function(){ current_key <<-"w"})                          # Bind the 'w' key to set current_key to "w"
    tkbind(CONTROLLER, "<a>", function(){ current_key <<-"a"})                          # Bind the 'a' key to set current_key to "a"
    tkbind(CONTROLLER, "<s>", function(){ current_key <<-"s"})                          # Bind the 's' key to set current_key to "s"
    tkbind(CONTROLLER, "<d>", function(){ current_key <<-"d"})                          # Bind the 'd' key to set current_key to "d"
    tkbind(CONTROLLER, "<q>", function(){ current_key <<-"q"})                          # Bind the 'q' key to set current_key to "q"
    tkbind(CONTROLLER, "<Left>", function(){ current_key <<-"left"})                    # Bind the left arrow key to set current_key to "left"
    tkbind(CONTROLLER, "<Right>", function(){ current_key <<-"right"})                  # Bind the right arrow key to set current_key to "right"
    tkbind(CONTROLLER, "<Up>", function(){ current_key <<-"up"})                        # Bind the up arrow key to set current_key to "up"
    tkbind(CONTROLLER, "<Down>", function(){ current_key <<-"down"})                    # Bind the down arrow key to set current_key to "down"
}

{ # ---FUNCTIONS---
    wrap_colour <- function(char, fg = NULL, bg = NULL) {
        if(is.null(fg) && is.null(bg)) {
            return(char)
        }
        else if(is.null(bg)) {
            return(paste0("\033[38;2;", fg[[1]], ";", fg[[2]], ";", fg[[3]], "m", char, "\033[0m"))
        }
        else {
            return(paste0("\033[48;2;", bg[[1]], ";", bg[[2]], ";", bg[[3]], "m\033[38;2;", fg[[1]], ";", fg[[2]], ";", fg[[3]], "m", char, "\033[0m"))
        }
    }

    place_room <- function(row, col) {
        world_map[row:(row+SINGLE_ROOM_H-1), col:(col+SINGLE_ROOM_W-1)] <<- sample(rooms_selection_list, 1)[[1]]
    }

    check_input <- function() {
        tcl("update", "idletasks") # Clear any pending events

        switch(
            current_key,
            "w" = {player_move(-1, 0)},
            "s" = {player_move(1, 0)},
            "d" = {player_move(0, 1)},
            "a" = {player_move(0, -1)},
            "left" = {slash_sword(0, -1)},
            "right" = {slash_sword(0, 1)},
            "up" = {slash_sword(-1, 0)},
            "down" = {slash_sword(1, 0)},
            "q" = {running <<- FALSE}
        )
        current_key <<- " "
    }

    player_move <- function(y_offset, x_offset) {
        if(!(world_map[player_y + y_offset, player_x + x_offset] %in% collide_objects)) {
            player_x <<- player_x + x_offset
            player_y <<- player_y + y_offset
        }
    }

    slash_sword <- function(y_offset, x_offset) {
        sword_slash <<- c(y_offset, x_offset)
        for(enemy in enemies) {
            if(enemy$y == player_y + sword_slash[1] && enemy$x == player_x + sword_slash[2]) {
                enemies <<- enemies[-which(sapply(enemies, function(x) x$y == enemy$y && x$x == enemy$x))]
                world_map[enemy$y, enemy$x] <<- " "
                break
            }
        }
        sword_visibility <<- 1
    }

    print_side_data <- function() {
        # side data - printed at column 15
        cat("\033[H\033[15C")
        cat("FPS: ", round(1/current_time_diff, 0), "\n")
        cat("\033[15C")
    }

    draw_world <- function() {
        # Cut the world_map matrix around the player
        viewport <- world_map[(player_y-OFFSET):(player_y+OFFSET), (player_x-OFFSET):(player_x+OFFSET)]

        # Place the player in the center of the viewport
        viewport[OFFSET+1, OFFSET+1] <- "@"
        if(sword_slash[1] != 0 || sword_slash[2] != 0) {
            viewport[sword_slash[1] + OFFSET + 1, sword_slash[2] + OFFSET + 1] <- "/"
            sword_visibility <<- sword_visibility - 1
            if(sword_visibility == 0) {
                sword_slash <<- c(0, 0)
            }
        }
    
        # Apply colours to the viewport from the colour palette
        coloured_viewport <- matrix(COLOUR_PALETTE[viewport], nrow = OFFSET*2+1, ncol=OFFSET*2+1)

        # Convert the coloured viewport to a single string
        row_strings <- apply(coloured_viewport, 1, paste, collapse = "")
        final_frame <- paste(row_strings, collapse = '\n')

        # Move the cursor to the top-left corner, delay and print the frame
        print_side_data()
        cat("\033[H")
        cat(final_frame)
        cat('\n')
    }

    main_loop <- function(){
        # Hide the cursor, clear the screen and move the cursor to the top-left corner
        cat("\033[?25l\033[2J\033[H") 

        # Show the cursor, reset colours and destroy the controller when the function exits or game crashes
        on.exit({
            cat("\033[?25h\033[0m")
            tkdestroy(CONTROLLER)
        })
        
        begin_time <<- Sys.time()

        # Main game loop
        while(running){
            check_input()
            draw_world()

            end_time <<- Sys.time()
            current_time_diff <<- as.numeric(difftime(end_time, begin_time, units = "secs"))

            # Wait for the next tick - We use loop instead of Sys.sleep since tcltk interrupts it, causing code to run untimely
            while(current_time_diff < TARGET_TICK_TIME) {
                end_time <<- Sys.time()
                current_time_diff <<- as.numeric(difftime(end_time, begin_time, units = "secs"))
            }
            begin_time <<- Sys.time()
        }
    }
}

{ # ---GLOBAL VALUES---
    { # ---GAME STATE---
        running <- TRUE
        TARGET_TICK_TIME <- 0.05
        begin_time <- end_time <- Sys.time()
        current_time_diff <- 0

        COLOUR_PALETTE <- c(                                                        # foreground / background   # name
            "##" = wrap_colour("#", fg = c(100, 100, 100)),                         # dark gray / none          # world_map border
            "#" = wrap_colour("#", fg = c(100, 100, 100)),                          # dark gray / none          # room border
            " " = " ",                                                              # none / none               # empty space
            "@" = wrap_colour("@", fg = c(0, 255, 0)),                              # green / none              # player
            "!" = wrap_colour("!", fg = c(255, 255, 0)),                            # yellow / none             # random drawing
            "x" = wrap_colour("x", fg = c(255, 0, 0)),                              # red / none                # basic enemy
            "X" = wrap_colour("x", fg = c(255, 0, 0), bg = c(35, 35, 35)),          # red / dark grey           # mid enemy
            "B" = wrap_colour("X", fg = c(255, 0, 0), bg = c(0, 0, 0)),             # red / black               # high enemy
            "/" = wrap_colour("/", fg = c(255, 255, 255))                           # white / none              # sword slash
        )

        #COLOUR_PALETTE <- c(                                                        # foreground / background   # name
        #    "##" = wrap_colour("#"),                         # dark gray / none          # world_map border
        #    "#" = wrap_colour("#"),                          # dark gray / none          # room border
        #    " " = " ",                                                              # none / none               # empty space
        #    "@" = wrap_colour("@"),                              # green / none              # player
        #    "!" = wrap_colour("!"),                            # yellow / none             # random drawing
        #    "x" = wrap_colour("x"),                              # red / none                # basic enemy
        #    "X" = wrap_colour("X"),          # red / dark grey           # mid enemy
        #    "B" = wrap_colour("B"),              # red / black               # high enemy
        #    "/" = wrap_colour("/")                         # white / none              # sword slash
        #)
        collide_objects <- c("#", "##", "x", "X", "B")
    }

    { # ---ROOMS AND MAP---
        { # CONSTANTS
            SINGLE_ROOM_H <- 10
            SINGLE_ROOM_W <- 10
            OFFSET <- 5
        }

        { # world_map and rooms_selection_list
            world_map <- matrix("##", nrow = 100, ncol = 100) #█

            { # rooms selection creation
                room_preassets <- list(
                    room_1 = "
                        ##########/
                        #........./
                        #.....!.../
                        #..!.X!.../
                        #........./
                        #........./
                        #....x..../
                        #.x......./
                        #........./
                        #.........",
                    room_2 = "
                        ##########/
                        #........./
                        #.x......./
                        #.......x./
                        #........./
                        #....x..../
                        #........./
                        #........./
                        #........./
                        #.........",
                    room_3 = "
                        ##########/
                        #........./
                        #........./
                        #........./
                        #..!...!../
                        #....B..../
                        #..!...!../
                        #...!!!.../
                        #........./
                        #........."
                )

                rooms_selection_list <- list()

                for (room_template in room_preassets) {
                    # Step by step processing using |> pipe
                    room_template <- room_template |>
                        gsub(pattern = "[[:space:]]", replacement = "") |> # 1. Removes all spaces, tabs and newlines
                        gsub(pattern = "\\.", replacement = " ")        # 2. Replaces dots with spaces (dot in regex is \\.)
                    
                    # Split by newlines
                    lines <- strsplit(room_template, "/")[[1]]
                    
                    # Change to matrix
                    room_matrix <- do.call( rbind, strsplit(lines, ""))

                    # Add doors
                    basic_room_up <- basic_room_left <- room_matrix
                    basic_room_up[1, SINGLE_ROOM_W/2+1] <- " "
                    basic_room_left[SINGLE_ROOM_H/2+1, 1] <- " "

                    # Add to selection list
                    rooms_selection_list[[length(rooms_selection_list) + 1]] <- basic_room_up
                    rooms_selection_list[[length(rooms_selection_list) + 1]] <- basic_room_left
                }

            }
        }

        { # Filling the world with rooms
            for(y in 0:2){
                for(x in 0:2){
                    place_room(1+SINGLE_ROOM_H*y+OFFSET, 1+SINGLE_ROOM_W*x+OFFSET)
                }
            }
        }                

        { # Fix wrongly placed doors
            # We move doors that are placed in walls to the correct position on the right/left/bottom/top of the room respectively
            
            for(y in seq(1+OFFSET+SINGLE_ROOM_H/2, 1+OFFSET+SINGLE_ROOM_H*2.5, SINGLE_ROOM_H)){
                if(world_map[y, 1+OFFSET] == " "){
                    world_map[y, 1+OFFSET] <- "#"
                    if(world_map[y, 1+OFFSET+SINGLE_ROOM_W] == " "){
                        world_map[y+SINGLE_ROOM_H/2, 1+OFFSET+SINGLE_ROOM_W/2] <- " "
                    }
                    else{
                        world_map[y, 1+OFFSET+SINGLE_ROOM_W] <- " "
                    }
                }
            }
            
            for(x in seq(1+OFFSET+SINGLE_ROOM_W/2, 1+OFFSET+SINGLE_ROOM_W*2.5, SINGLE_ROOM_W)){
                if(world_map[1+OFFSET, x] == " "){
                    world_map[1+OFFSET, x] <- "#"
                    if(world_map[1+OFFSET+SINGLE_ROOM_H, x] == " "){
                        if(world_map[1+OFFSET+SINGLE_ROOM_H/2, x+SINGLE_ROOM_W/2] == "##"){
                            world_map[1+OFFSET+SINGLE_ROOM_H/2, x-SINGLE_ROOM_W/2] <- " "
                        }
                        else{
                            world_map[1+OFFSET+SINGLE_ROOM_H/2, x+SINGLE_ROOM_W/2] <- " "
                        }
                    }
                    else{
                        world_map[1+OFFSET+SINGLE_ROOM_H, x] <- " "
                    }
                }
                
                if(world_map[1+OFFSET+SINGLE_ROOM_H*3, x] == " "){
                    world_map[1+OFFSET+SINGLE_ROOM_H*3, x] <- "#"
                    if(world_map[1+OFFSET+SINGLE_ROOM_H*2, x] == " "){
                        world_map[1+OFFSET+SINGLE_ROOM_H*2.5, x+SINGLE_ROOM_W/2] <- " "
                    }
                    else{
                        world_map[1+OFFSET+SINGLE_ROOM_H*2, x] <- " "
                    }
                }
            }
        }
    }

    { # ---NPCs---
        { # get enemies list from world map
            enemies <- list()
            for(y in (OFFSET+1):(nrow(world_map)-OFFSET)){
                for(x in (OFFSET+1):(ncol(world_map)-OFFSET)){
                    if(world_map[y, x] == "x"){
                        enemies[[length(enemies)+1]] <- list(type = "x", y = y, x = x)
                    }
                    else if(world_map[y, x] == "X"){
                        enemies[[length(enemies)+1]] <- list(type = "X", y = y, x = x)
                    }
                }
            }
        }
    }

    { # ---PLAYER---
        player_y <- 10
        player_x <- 10

        sword_visibility <- 0
        sword_slash <- c(0, 0)
    }
}
 


main_loop()
