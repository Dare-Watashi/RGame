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
    tkbind(CONTROLLER, "<p>", function(){ current_key <<-"p"})                          # Bind the 'p' key to set current_key to "p"
}

{ # ---FUNCTIONS---

    { # ---COLOUR FUNCTIONS---
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
    }

    { # ---WORLD MAP FUNCTIONS---
        place_room <- function(row, col) {
            world_map[row:(row+SINGLE_ROOM_H-1), col:(col+SINGLE_ROOM_W-1)] <<- sample(rooms_selection_list, 1)[[1]]
        }

        fill_world_with_rooms <- function() {
            for(y in 0:2){
                for(x in 0:2){
                    place_room(1+SINGLE_ROOM_H*y+OFFSET, 1+SINGLE_ROOM_W*x+OFFSET)
                }
            }
        }

        fix_wrongly_placed_doors <- function() {
            # We move doors that are placed in walls to the correct position on the right/left/bottom/top of the room respectively
            
            for(y in seq(1+OFFSET+SINGLE_ROOM_H/2, 1+OFFSET+SINGLE_ROOM_H*2.5, SINGLE_ROOM_H)){
                if(world_map[y, 1+OFFSET] == " "){
                    world_map[y, 1+OFFSET] <<- "#"
                    if(world_map[y, 1+OFFSET+SINGLE_ROOM_W] == " "){
                        world_map[y+SINGLE_ROOM_H/2, 1+OFFSET+SINGLE_ROOM_W/2] <<- " "
                    }
                    else{
                        world_map[y, 1+OFFSET+SINGLE_ROOM_W] <<- " "
                    }
                }
            }
            
            for(x in seq(1+OFFSET+SINGLE_ROOM_W/2, 1+OFFSET+SINGLE_ROOM_W*2.5, SINGLE_ROOM_W)){
                if(world_map[1+OFFSET, x] == " "){
                    world_map[1+OFFSET, x] <<- "#"
                    if(world_map[1+OFFSET+SINGLE_ROOM_H, x] == " "){
                        if(world_map[1+OFFSET+SINGLE_ROOM_H/2, x+SINGLE_ROOM_W/2] == "##"){
                            world_map[1+OFFSET+SINGLE_ROOM_H/2, x-SINGLE_ROOM_W/2] <<- " "
                        }
                        else{
                            world_map[1+OFFSET+SINGLE_ROOM_H/2, x+SINGLE_ROOM_W/2] <<- " "
                        }
                    }
                    else{
                        world_map[1+OFFSET+SINGLE_ROOM_H, x] <<- " "
                    }
                }
                
                if(world_map[1+OFFSET+SINGLE_ROOM_H*3, x] == " "){
                    world_map[1+OFFSET+SINGLE_ROOM_H*3, x] <<- "#"
                    if(world_map[1+OFFSET+SINGLE_ROOM_H*2, x] == " "){
                        world_map[1+OFFSET+SINGLE_ROOM_H*2.5, x+SINGLE_ROOM_W/2] <<- " "
                    }
                    else{
                        world_map[1+OFFSET+SINGLE_ROOM_H*2, x] <<- " "
                    }
                }
            }
        }
    }
    
    { # ---INPUT FUNCTIONS---
        check_input <- function() {
            tcl("update", "idletasks") # Clear any pending events

            # key events
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
                "p" = {{player$hp <<- player$hp - 10; print_player_hp <<- TRUE}},
                "q" = {running <<- FALSE}
            )
            current_key <<- " "
        }
    }

    { # ---ENEMY FUNCTIONS---
        get_enemies_from_world_map <- function() {
            for(y in (OFFSET+1):(nrow(world_map)-OFFSET)){
                for(x in (OFFSET+1):(ncol(world_map)-OFFSET)){
                    if(world_map[y, x] %in% c("x", "X", "B")){
                        enemies[[length(enemies)+1]] <<- list(type = world_map[y, x], y = y, x = x)
                        world_map[y, x] <<- " "
                    }
                }
            }
        }
    }

    { # ---PLAYER FUNCTIONS---
        player_move <- function(y_offset, x_offset) {
            # check if there is collide object at new position or an enemy
            if(!(world_map[player$y + y_offset, player$x + x_offset] %in% collide_objects) && 
               !(any(sapply(enemies, function(enemy) enemy$y == player$y + y_offset && enemy$x == player$x + x_offset)))
            ) {
                player$x <<- player$x + x_offset
                player$y <<- player$y + y_offset
            }
        }

        slash_sword <- function(y_offset, x_offset) {
            sword_slash <<- c(y_offset, x_offset)
            for(enemy in enemies) {
                if(enemy$y == player$y + sword_slash[1] && enemy$x == player$x + sword_slash[2]) {
                    enemies <<- enemies[-which(sapply(enemies, function(x) x$y == enemy$y && x$x == enemy$x))]
                    break
                }
            }
            sword_visibility <<- TRUE
        }
    }

    { # ---SIDE DATA FUNCTIONS---
        get_player_hp_bar <- function() {
            hp_bar <- paste(rep(COLOUR_PALETTE["█"], player$hp/5), collapse = "")
            hp_bar <- paste(hp_bar, paste(rep(COLOUR_PALETTE["░"], player$max_hp/5 - player$hp/5), collapse = ""), sep = "")
            return(hp_bar)
        }

        print_side_data <- function() {
            # side data - printed at column 15
            cat("\033[H\033[15C")
            cat("FPS: ", round(1/current_time_diff, 0), "\n")
            if(print_player_hp) {
                cat("\033[H\033[25C")
                # coloured hp bar
                cat("Player HP: ", get_player_hp_bar(), "\n")
                print_player_hp <<- FALSE
            }
        }
    }

    { # ---DRAW FUNCTIONS---
        get_viewport <- function(world_map) {
            return(world_map[(player$y-OFFSET):(player$y+OFFSET), (player$x-OFFSET):(player$x+OFFSET)])
        }

        draw_player <- function(viewport) {
            viewport[OFFSET+1, OFFSET+1] <- "@"
            return(viewport)
        }

        draw_sword <- function(viewport) {
            if(sword_visibility) {
                viewport[sword_slash[1] + OFFSET + 1, sword_slash[2] + OFFSET + 1] <- "/"
                sword_visibility <<- FALSE
            }
            return(viewport)
        }

        draw_enemies <- function(viewport) {
            # draw only enemies visible in the viewport (within OFFSET range)
            for(enemy in enemies[]) {
                if(abs(enemy$y - player$y) <= OFFSET && abs(enemy$x - player$x) <= OFFSET) {
                    viewport[enemy$y - player$y + OFFSET + 1, enemy$x - player$x + OFFSET + 1] <- enemy$type
                }
            }
            return(viewport)
        }

        colour_viewport <- function(viewport) {
            return(matrix(COLOUR_PALETTE[viewport], nrow = OFFSET*2+1, ncol=OFFSET*2+1))
        }

        draw_world <- function() {
            viewport <- get_viewport(world_map)
            viewport <- draw_player(viewport)
            viewport <- draw_sword(viewport)
            viewport <- draw_enemies(viewport)
            coloured_viewport <- colour_viewport(viewport)

            # Convert the coloured viewport to a single string
            row_strings <- apply(coloured_viewport, 1, paste, collapse = "")
            final_frame <- paste(row_strings, collapse = '\n')

            # Move the cursor to the top-left corner, delay and print the frame
            print_side_data()
            cat("\033[H")
            cat(final_frame)
            cat('\n')
        }
    }

    

    main_loop <- function(){
        # Hide the cursor, clear the screen and move the cursor to the top-left corner
        cat("\033[?25l\033[2J\033[H") 
        fill_world_with_rooms()
        fix_wrongly_placed_doors()
        get_enemies_from_world_map()

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

{ # ---GLOBAL DATA---
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
            "/" = wrap_colour("/", fg = c(255, 255, 255)),                          # white / none              # sword slash
            "█" = wrap_colour("█", fg = c(255, 0, 0)),                              # red / none                # hp bar
            "░" = wrap_colour("░", fg = c(100, 0, 0)),                              # dark red / none           # hp bar empty
            "1" = wrap_colour("1", fg = c(255, 255, 255)),                          # white / none              # debug text
            "2" = wrap_colour("2", fg = c(255, 255, 255)),                          # white / none              # debug text
            "3" = wrap_colour("3", fg = c(255, 255, 255)),                          # white / none              # debug text
            "4" = wrap_colour("4", fg = c(255, 255, 255)),                          # white / none              # debug text
            "5" = wrap_colour("5", fg = c(255, 255, 255)),                          # white / none              # debug text
            "6" = wrap_colour("6", fg = c(255, 255, 255)),                          # white / none              # debug text
            "7" = wrap_colour("7", fg = c(255, 255, 255))                           # white / none              # debug text
        )

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
    }

    { # ---NPCs---
        enemies <- list()
    }

    { # ---PLAYER---
        player <- list(
            y = 10,
            x = 10,
            hp = 100,
            max_hp = 100,
            atk = 15
        )
        print_player_hp <- TRUE

        sword_visibility <- FALSE
        sword_slash <- c(0, 0)
    }
}



main_loop()
