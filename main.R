library(tcltk)


{ # ---TCLTK AND KEY BINGINGS
    current_key <- ""
    CONTROLLER <- tktoplevel()
    tkwm.geometry(CONTROLLER, "200x50")
    tkwm.title(CONTROLLER, "Key press reader")
    tkpack(tklabel(CONTROLLER, text="Keep this window selected\nto play the game"))
    tkbind(CONTROLLER, "<w>", function(){ current_key <<-"w"})
    tkbind(CONTROLLER, "<a>", function(){ current_key <<-"a"})
    tkbind(CONTROLLER, "<s>", function(){ current_key <<-"s"})
    tkbind(CONTROLLER, "<d>", function(){ current_key <<-"d"})
    tkbind(CONTROLLER, "<q>", function(){ current_key <<-"q"})
}

{ # ---GLOBAL VALUES---
    { # ---GAME STATE---
        running <- TRUE
    }

    { # ---ROOMS AND MAP---
        { # CONSTANTS
            SINGLE_ROOM_H <- 10
            SINGLE_ROOM_W <- 10
            OFFSET <- 5
            COLOUR_PALETTE <- c(
                "##" = "\033[38;5;240m#\033[0m",
                "#" = "\033[38;5;240m#\033[0m",
                " " = " ",
                "@" = "\033[32m@\033[0m",
                "!" = "\033[31m!\033[0m"
            )
        }

        { # world_map and rooms_selection_list
            world_map <- matrix("##", nrow = 100, ncol = 100) #█

            { # rooms selection creation
                room_preassets <- list(
                    room_1 = "
                        ##########/
                        #........./
                        #.....!.../
                        #..!..!.../
                        #........./
                        #........./
                        #........./
                        #........./
                        #........./
                        #.........",
                    room_2 = "
                        ##########/
                        #........./
                        #........./
                        #........./
                        #........./
                        #........./
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
                        #........./
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

    { # ---PLAYER---
        player_y <- 10
        player_x <- 10
    }
}

{ # ---FUNCTIONS---
    place_room <- function(row, col) {
        world_map[row:(row+SINGLE_ROOM_H-1), col:(col+SINGLE_ROOM_W-1)] <<- sample(rooms_selection_list, 1)[[1]]
    }

    check_input <- function() {
        switch(
            current_key,
            "w" = {player_move(-1, 0)},
            "s" = {player_move(1, 0)},
            "d" = {player_move(0, 1)},
            "a" = {player_move(0, -1)},
            "q" = {running <<- FALSE}
        )
        current_key <<- " "
    }

    player_move <- function(y_offset, x_offset) {
        if(world_map[player_y + y_offset, player_x + x_offset] != "#" && world_map[player_y + y_offset, player_x + x_offset] != "##") {
            player_x <<- player_x + x_offset
            player_y <<- player_y + y_offset
        }
    }

    draw_world <- function() {
        # Cut the world_map matrix around the player
        viewport <- world_map[(player_y-OFFSET):(player_y+OFFSET), (player_x-OFFSET):(player_x+OFFSET)]
        # Place the player in the center of the viewport
        viewport[OFFSET+1, OFFSET+1] <- "@"
        # Apply colours to the viewport from the colour palette
        coloured_viewport <- matrix(COLOUR_PALETTE[viewport], nrow = OFFSET*2+1, ncol=OFFSET*2+1)

        # Convert the coloured viewport to a single string
        row_strings <- apply(coloured_viewport, 1, paste, collapse = "")
        final_frame <- paste(row_strings, collapse = '\n')

        # Move the cursor to the top-left corner, delay and print the frame
        cat("\033[H")
        Sys.sleep(0.02)
        cat(final_frame)
        cat('\n')
    }

    main_loop <- function(){
        # Hide the cursor, clear the screen and move the cursor to the top-left corner
        cat("\033[?25l\033[2J\033[H") 

        # Show the cursor and destroy the controller when the function exits or game crashes
        on.exit({
            cat("\033[?25h")
            tkdestroy(CONTROLLER)
        })

        # Main game loop
        while(running){
            check_input()
            draw_world()
        }
    }
}



main_loop()
