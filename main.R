library(tcltk)

# Ctrl+shift+[ to fold all brackets
# Ctrl+shift+] to unfold all brackets


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
        # We create a matrix of size MAP_ROOM_COUNT_Y * MAP_ROOM_COUNT_X and fill with number 25. 
        # Then, we randomly choose one cell, set its number to 0 and starting from there,
        # there is 50% chance to set the number of the adjacent cell to one higher then the previous one.
        # We repeat that action for every cell that has number lower then 24, remembering that we do not overwrite cells that already have a number.

        unlock_adjacent_cells <- function(row, col) {
            if(row > 1 && map_preview_matrix[row-1, col] == 25 && runif(1) < 0.5) {
                map_preview_matrix[row-1, col] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row-1, col)
            }
            if(row < MAP_ROOM_COUNT_Y && map_preview_matrix[row+1, col] == 25 && runif(1) < 0.5) {
                map_preview_matrix[row+1, col] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row+1, col)
            }
            if(col > 1 && map_preview_matrix[row, col-1] == 25 && runif(1) < 0.5) {
                map_preview_matrix[row, col-1] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row, col-1)
            }
            if(col < MAP_ROOM_COUNT_X && map_preview_matrix[row, col+1] == 25 && runif(1) < 0.5) {
                map_preview_matrix[row, col+1] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row, col+1)
            }
        }

        create_map_preview_matrix <- function() {
            start_y <<- sample(1:MAP_ROOM_COUNT_Y, 1)
            start_x <<- sample(1:MAP_ROOM_COUNT_X, 1)
            map_preview_matrix[start_y, start_x] <<- 0

            unlock_adjacent_cells(start_y, start_x)
        }


        place_room <- function(row, col) {
            world_map[(row-(SINGLE_ROOM_H%/%2)):(row+(SINGLE_ROOM_H%/%2)), (col-(SINGLE_ROOM_W%/%2)):(col+(SINGLE_ROOM_W%/%2))] <<- sample(rooms_selection_list, 1)[[1]]
        }

        fill_world_with_rooms <- function() {
            room_place_offset_y <- OFFSET + 1 + SINGLE_ROOM_H%/%2
            room_place_offset_x <- OFFSET + 1 + SINGLE_ROOM_W%/%2

            for(y in 0:(MAP_ROOM_COUNT_Y-1)){
                for(x in 0:(MAP_ROOM_COUNT_X-1)){
                    if(map_preview_matrix[y+1, x+1] != 25) {
                        place_room( room_place_offset_y + (SINGLE_ROOM_H-1)*y, room_place_offset_x + (SINGLE_ROOM_W-1)*x)
                        player$y <<- room_place_offset_y + (SINGLE_ROOM_H-1)*y
                        player$x <<- room_place_offset_x + (SINGLE_ROOM_W-1)*x
                        #world_map[room_place_offset_y + (SINGLE_ROOM_H-1)*y, room_place_offset_x + (SINGLE_ROOM_W-1)*x] <<- map_preview_matrix[y+1][x+1]
                    }
                }
            }
        }

        place_doors <- function(row, col) {
            # Provided that row,col are coordinates of a centre of a room, if there is a room adjacent to it which value in world_preview_matrix is 1 higher, place a door between them between them
            # world_preview_matrix is a sketch of rooms placement where each room is marked with a number of it's order of creation
            # world_map is full size map where each room has size SINGLE_ROOM_H and SINGLE_ROOM_W
            if(row > 1 && map_preview_matrix[row-1, col] == map_preview_matrix[row, col] + 1) {
                # Place door above
                world_map[OFFSET + row * (SINGLE_ROOM_H-1) - SINGLE_ROOM_H + 2, OFFSET + col * (SINGLE_ROOM_W-1) - SINGLE_ROOM_W%/%2 + 1] <<- "[]"

                place_doors(row-1, col)
            }
            if(row < MAP_ROOM_COUNT_Y && map_preview_matrix[row+1, col] == map_preview_matrix[row, col] + 1) {
                # Place door below
                world_map[OFFSET + row * (SINGLE_ROOM_H-1) + 1, OFFSET + col * (SINGLE_ROOM_W-1) - SINGLE_ROOM_W%/%2 + 1] <<- "[]"

                place_doors(row+1, col)
            }
            if(col > 1 && map_preview_matrix[row, col-1] == map_preview_matrix[row, col] + 1) {
                # Place door left
                world_map[OFFSET + row * (SINGLE_ROOM_H-1) - SINGLE_ROOM_H%/%2 + 1, OFFSET + col * (SINGLE_ROOM_W-1) - SINGLE_ROOM_W + 2] <<- "[]"

                place_doors(row, col-1)
            }
            if(col < MAP_ROOM_COUNT_X && map_preview_matrix[row, col+1] == map_preview_matrix[row, col] + 1) {
                # Place door right
                world_map[OFFSET + row * (SINGLE_ROOM_H-1) - SINGLE_ROOM_H%/%2 + 1, OFFSET + col * (SINGLE_ROOM_W-1) + 1] <<- "[]"

                place_doors(row, col+1)
            }
        }

        # Drawing walls that are not next to walkable space is too heavy and not really needed
        clear_unneeded_walls <- function(){
            for(x in 1:ncol(world_map)){
                for(y in 1:nrow(world_map)){
                    if(world_map[y, x] == "#"){
                        if(world_map[y-1, x] != " " && world_map[y+1, x] != " " && world_map[y, x-1] != " " && world_map[y, x+1] != " "){
                            world_map[y, x] <<- "##"
                        }
                    }
                }
            }
        }

        # Create a circular mask for the player's field of view
        create_circle_mask <- function() {
            y_grid <- row(matrix(0, nrow = OFFSET*2+1, ncol = OFFSET*2+1))
            x_grid <- col(matrix(0, nrow = OFFSET*2+1, ncol = OFFSET*2+1))
            dist_squared <- (x_grid - OFFSET - 1)^2 + (y_grid - OFFSET - 1)^2
            circle_mask <- dist_squared <= light_radius^2
            return(circle_mask)
        }

        # Get indices of cells that are within the player's field of view
        get_ray_indices <- function(y, x) {
            dist <- max(abs(y - (OFFSET + 1)), abs(x - (OFFSET + 1)))
            
            if(dist <= 1) {
                return(return(integer(0)))
            }

            y_pts <- round(seq((OFFSET + 1), y, length.out = dist + 1) + 0.01)
            x_pts <- round(seq((OFFSET + 1), x, length.out = dist + 1) + 0.01)

            y_between <- y_pts[2:(dist)]
            x_between <- x_pts[2:(dist)]

            indices <- (x_between - 1) * (OFFSET*2 + 1) + y_between
            
            return(indices)
        }

        # Precalculate ray casting indices for all possible directions
        precalculate_ray_indices <- function() {
            ray_indices <- vector("list", (OFFSET*2 + 1) * (OFFSET*2 + 1))
            for(x in 1:(OFFSET*2 + 1)) {
                for(y in 1:(OFFSET*2 + 1)) {
                    idx <- (x - 1) * (OFFSET*2 + 1) + y
                    ray_indices[[idx]] <- get_ray_indices(y, x)
                }
            }
            return(ray_indices)
        }


        assemble_world_map <- function() {
            create_map_preview_matrix()
            fill_world_with_rooms()
            place_doors(start_y, start_x)
            get_enemies_from_world_map()
            clear_unneeded_walls()
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
                        enemies[nrow(enemies)+1,] <<- data.frame(symbol = world_map[y, x], y = y, x = x, move_cooldown = 0, max_move_cooldown = 5, hp = 100, max_hp = 100, atk = 1)
                        world_map[y, x] <<- " "
                        enemies_hp_to_show <<- c(enemies_hp_to_show, FALSE)
                    }
                }
            }
        }

        update_enemies <- function() {
            for(i in seq_len(nrow(enemies))) {
                if(enemies[i, "move_cooldown"] > 0) {
                    enemies[i, "move_cooldown"] <<- enemies[i, "move_cooldown"] - 1
                    next
                }
                if(dist_map[enemies[i, "y"], enemies[i, "x"]] <= 7) {
                    if(dist_map[enemies[i, "y"]-1, enemies[i, "x"]] == dist_map[enemies[i, "y"], enemies[i, "x"]] - 1 &&
                        dist_map[enemies[i, "y"]-1, enemies[i, "x"]] != 0 && enemies_map[enemies[i, "y"]-1, enemies[i, "x"]] == 0) {
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- 0

                        enemies[i, "y"] <<- enemies[i, "y"] - 1
                        enemies[i, "move_cooldown"] <<- enemies[i, "max_move_cooldown"]
                        
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- i
                    }
                    else if(dist_map[enemies[i, "y"]+1, enemies[i, "x"]] == dist_map[enemies[i, "y"], enemies[i, "x"]] - 1 &&
                        dist_map[enemies[i, "y"]+1, enemies[i, "x"]] != 0 && enemies_map[enemies[i, "y"]+1, enemies[i, "x"]] == 0) {
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- 0
                        
                        enemies[i, "y"] <<- enemies[i, "y"] + 1
                        enemies[i, "move_cooldown"] <<- enemies[i, "max_move_cooldown"]
                        
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- i
                    }
                    else if(dist_map[enemies[i, "y"], enemies[i, "x"]-1] == dist_map[enemies[i, "y"], enemies[i, "x"]] - 1 &&
                        dist_map[enemies[i, "y"], enemies[i, "x"]-1] != 0 && enemies_map[enemies[i, "y"], enemies[i, "x"]-1] == 0) {
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- 0

                        enemies[i, "x"] <<- enemies[i, "x"] - 1
                        enemies[i, "move_cooldown"] <<- enemies[i, "max_move_cooldown"]
                        
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- i
                    }
                    else if(dist_map[enemies[i, "y"], enemies[i, "x"]+1] == dist_map[enemies[i, "y"], enemies[i, "x"]] - 1 &&
                        dist_map[enemies[i, "y"], enemies[i, "x"]+1] != 0 && enemies_map[enemies[i, "y"], enemies[i, "x"]+1] == 0) {
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- 0
                        
                        enemies[i, "x"] <<- enemies[i, "x"] + 1
                        enemies[i, "move_cooldown"] <<- enemies[i, "max_move_cooldown"]
                        
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- i
                    }
                }
            }
        }
    }
        

    { # ---PLAYER FUNCTIONS---
        # player sense aura - distance from player to all cells in 7 cells radius for Enemy pathfinding
        update_player_sense_aura <- function(){
            dist_map <<- matrix(Inf, nrow = nrow(world_map), ncol = ncol(world_map))
            dist_map[player$y, player$x] <<- 0
            for(i in 1:20){
                up <<- rbind(rep(Inf, ncol(world_map)), dist_map[-nrow(dist_map), ])
                down <<- rbind(dist_map[-1, ], rep(Inf, ncol(world_map)))
                left <<- cbind(rep(Inf, nrow(world_map)), dist_map[, -ncol(dist_map)])
                right <<- cbind(dist_map[, -1], rep(Inf, nrow(world_map)))
                
                dist_map <<- pmin(dist_map, up + 1, down + 1, left + 1, right + 1)

                dist_map[world_map %in% COLLIDE_OBJECTS] <<- Inf
            }

            #new_dist_map <- matrix(Inf, nrow = nrow(world_map), ncol = ncol(world_map))
            #new_dist_map[(player$y-OFFSET+1):(player$y+OFFSET-1), (player$x-OFFSET+1):(player$x+OFFSET-1)] <- dist_map[(player$y-OFFSET+1):(player$y+OFFSET-1), (player$x-OFFSET+1):(player$x+OFFSET-1)]
            #dist_map <<- new_dist_map
        }

        player_move <- function(y_offset, x_offset) {
            # check if there is collide object at new position or an enemy
            if(!(world_map[player$y + y_offset, player$x + x_offset] %in% COLLIDE_OBJECTS) && 
               !(any(enemies$y == player$y + y_offset & enemies$x == player$x + x_offset))
            ) {
                player$x <<- player$x + x_offset
                player$y <<- player$y + y_offset
                
                update_player_sense_aura()
            }
            else{
                if(world_map[player$y + y_offset, player$x + x_offset] == "[]") {
                    world_map[player$y + y_offset, player$x + x_offset] <<- "]"
                
                    update_player_sense_aura()
                }
            }
        }

        slash_sword <- function(y_offset, x_offset) {
            sword_slash <<- c(y_offset, x_offset)
            for(i in 1:(nrow(enemies))) {
                if(is.na(enemies[i, "y"]) || is.na(enemies[i, "x"])) {
                    next
                }
                if(enemies[i, "y"] == player$y + sword_slash[1] && enemies[i, "x"] == player$x + sword_slash[2]) {
                    enemies[i, "hp"] <<- enemies[i, "hp"] - player$atk
                    if(enemies[i, "hp"] <= 0) {
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] = 0
                    }
                    enemies_hp_to_show[i] <<- TRUE
                    print_enemy_hp <<- TRUE
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
        get_enemy_hp_bar <- function(i) {
            hp_bar <- ""
            cat(enemies[i, "hp"])
            if(enemies[i, "hp"] > 0){
                hp_bar <- paste(rep(COLOUR_PALETTE["█"], enemies[i, "hp"]%/%5), collapse = "")
                hp_bar <- paste(hp_bar, paste(rep(COLOUR_PALETTE["░"], enemies[i, "max_hp"]%/%5 - enemies[i, "hp"]%/%5), collapse = ""), sep = "")
                return(hp_bar)
            }
            else{
                enemies <<- enemies[-i, ]
                enemies_hp_to_show[i:(length(enemies_hp_to_show)-1)] <<- enemies_hp_to_show[(i+1):length(enemies_hp_to_show)]
                enemies_hp_to_show[length(enemies_hp_to_show)] <<- FALSE
                return(NA)
            }
        }

        print_side_data <- function() {
            # side data - printed at column 15
            cat("\033[H\033[25C")
            cat("FPS: ", round(1/current_time_diff, 0), "\n")
            if(print_player_hp) {
                cat("\033[H\033[35C")
                # coloured hp bar
                cat("Player HP: ", get_player_hp_bar(), "\n")
                print_player_hp <<- FALSE
            }
            if(print_enemy_hp) {
                for(i in 1:length(enemies_hp_to_show)) {
                    if(enemies_hp_to_show[i]) {
                        # coloured hp bar
                        hp_bar <- get_enemy_hp_bar(i)
                        if(!is.na(hp_bar)) {
                            cat("\033[35C")
                            cat("Enemy HP: ", hp_bar, "\n")
                        }
                        else{
                            # enemy is dead, take next
                            cat("\033[35C")
                            cat("Enemy HP: ", "DEAD                 ", "\n")
                            clear_screen <<- TRUE
                            i <- i - 1
                        }
                    }
                }
                print_enemy_hp <<- FALSE
            }
            #cat("\033[H\033[45C")
            #cat("Debug time: ", difftime(end_debug_time, start_debug_time, units = "sec")*1000, "ms\n")
            #cat("\033[45C")
            #cat("Max debug time: ", max_time, "ms\n")
            #cat("\033[45C")
            #cat("Min debug time: ", min_time, "ms\n")
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
            
            is_visible <- abs(enemies$y - player$y) <= OFFSET & abs(enemies$x - player$x) <= OFFSET

            if(!any(is_visible)) {
                return(viewport)
            }

            visible_enemies <- enemies[is_visible, ]
            visible_enemies_y <- visible_enemies$y - player$y + OFFSET + 1
            visible_enemies_x <- visible_enemies$x - player$x + OFFSET + 1

            viewport[cbind(visible_enemies_y, visible_enemies_x)] <- visible_enemies$symbol
            
            return(viewport)
        }

        cut_viewport <- function(viewport) {
            is_light_blocking <- viewport %in% c("##", "#", "[]")

            is_shadowed <- vapply(ray_list, function(ray){
                any(is_light_blocking[ray])
            }, logical(1))

            viewport[is_shadowed] <- "shadow"
            viewport[!circle_mask] <- "shadow"
            return(viewport)
        }

        colour_viewport <- function(viewport, colours_enabled) {
            if(colours_enabled) {
                return(matrix(COLOUR_PALETTE[viewport], nrow = OFFSET*2+1, ncol=OFFSET*2+1))
            }
            return(viewport)
        }

        draw_world <- function() {
            viewport <- get_viewport(world_map)
            viewport <- draw_enemies(viewport)
            viewport <- draw_sword(viewport)
            viewport <- draw_player(viewport)
            viewport <- cut_viewport(viewport)
            viewport <- colour_viewport(viewport, TRUE)


            # Convert the coloured viewport to a single string - v2.0   ~0.1ms - ~0.5ms
            # cbind adds a newline character to each row
            # t() transposes the matrix so rows become columns
            # this is becouse paste() prints column by column by default, so now t() causes rows to become columns
            final_frame <- paste(t(cbind(viewport, "\n")), collapse = "")
            
            if(clear_screen){
                cat("\033[2J")
                clear_screen <<- FALSE
                print_player_hp <<- TRUE
            }

            # Move the cursor to the top-left corner and print the frame
            print_side_data()
            cat("\033[H")
            cat(final_frame)
            # coloured wall ~55ms - ~744ms
            # uncoloured wall ~18ms - ~452ms    - current
            # GPU accelerated terminal ~19ms - ~552ms
            
        }
    }

    main_loop <- function(){
        # Hide the cursor, clear the screen and move the cursor to the top-left corner
        cat("\033[?25l\033[2J\033[H") 
        assemble_world_map()

        # Show the cursor, reset colours and destroy the controller when the function exits or game crashes
        on.exit({
            cat("\033[?25h\033[0m")
            tkdestroy(CONTROLLER)
        })
        
        begin_time <<- Sys.time()

        # clear screen
        cat("\033[?25l\033[2J\033[H")

        # Main game loop
        while(running){
            check_input()
            update_enemies()

            # colour-wrap function \RGB - ~25ms - ~550ms
            # implicit colour \16c - ~20ms - ~378ms
            draw_world()

            #start_debug_time <<- Sys.time()
            #end_debug_time <<- Sys.time()
            #max_time <<- max(max_time, difftime(end_debug_time, start_debug_time, units = "sec")*1000)
            #min_time <<- min(min_time, difftime(end_debug_time, start_debug_time, units = "sec")*1000)

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
        start_debug_time <- Sys.time()
        end_debug_time <- Sys.time()
        max_time <- 0
        min_time <- Inf

        begin_time <- end_time <- Sys.time()
        current_time_diff <- 0

        #COLOUR_PALETTE <- c(                                                        # foreground / background   # name
        #    "##" = wrap_colour(" ", fg = c(100, 100, 100)),                         # dark gray / none          # world_map border
        #    "#" = "#", #wrap_colour("#", fg = c(100, 100, 100)),                          # dark gray / none          # room border
        #    " " = " ",                                                              # none / none               # empty space
        #    "@" = wrap_colour("@", fg = c(0, 255, 0)),                              # green / none              # player
        #    "!" = wrap_colour("!", fg = c(255, 255, 0)),                            # yellow / none             # random drawing
        #    "x" = wrap_colour("x", fg = c(150, 0, 0)),                              # red / none                # basic enemy
        #    "X" = wrap_colour("x", fg = c(255, 0, 0)),          # red / dark grey           # mid enemy
        #    "B" = wrap_colour("X", fg = c(255, 0, 0)),             # red / black               # high enemy
        #    "/" = wrap_colour("/", fg = c(255, 255, 255)),                          # white / none              # sword slash
        #    "█" = wrap_colour("█", fg = c(255, 0, 0)),                              # red / none                # hp bar
        #    "░" = wrap_colour("░", fg = c(100, 0, 0)),                              # dark red / none           # hp bar empty
        #    "1" = wrap_colour("1", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "2" = wrap_colour("2", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "3" = wrap_colour("3", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "4" = wrap_colour("4", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "5" = wrap_colour("5", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "6" = wrap_colour("6", fg = c(255, 255, 255)),                          # white / none              # debug text
        #    "7" = wrap_colour("7", fg = c(255, 255, 255))                           # white / none              # debug text
        #)
        
        
        COLOUR_PALETTE = c(
            "#" = "#",      # clear
            "##" = " ",     # clear
            " " = ".",      # clear
            "[]" = "]",    # clear
            "]" = " ",      # clear
            "shadow" = " ", # shadow
            "@" = "\033[32m@\033[0m",      # green
            "!" = "\033[33m!\033[0m",      # yellow
            "x" = "\033[31mx\033[0m",      # red
            "X" = "\033[31mx\033[0m",      # red
            "B" = "\033[31mX\033[0m",      # red
            "/" = "\033[37m/\033[0m",      # white
            "█" = "\033[38;2;255;0;0m█\033[0m",      # red
            "░" = "\033[38;2;255;0;0m░\033[0m",
            "1" = "1",
            "2" = "2",
            "3" = "3",
            "4" = "4",
            "5" = "5",
            "6" = "6",
            "7" = "7"
        )
        
        clear_screen <- FALSE

        COLLIDE_OBJECTS <- c("#", "##", "x", "X", "B", "[]", "@")
        #COLLIDE_OBJECTS <- c()
    }

    { # ---ROOMS AND MAP---
        { # CONSTANTS
            SINGLE_ROOM_H <- 21
            SINGLE_ROOM_W <- 21
            OFFSET <- 10
            MAP_ROOM_COUNT_Y <- 5
            MAP_ROOM_COUNT_X <- 5
            map_preview_matrix <- matrix(25, nrow = MAP_ROOM_COUNT_Y, ncol = MAP_ROOM_COUNT_X)
            start_y <- 0
            start_x <- 0
        }

        { # world_map and rooms_selection_list
            world_map <- matrix("##", nrow = MAP_ROOM_COUNT_Y*SINGLE_ROOM_H+OFFSET*2, ncol = MAP_ROOM_COUNT_X*SINGLE_ROOM_W+OFFSET*2) #█

            light_radius <- 7
            circle_mask <- create_circle_mask()
            ray_list <- precalculate_ray_indices()

            { # rooms selection creation
                room_preassets <- list(
                       #1234567890123456789
                    room_1 = "
                        .................../
                        .....!............./
                        ..!.X!............./
                        .................../
                        .................../
                        ....x............../
                        .x................./
                        .................../
                        .................../
                        .................../
                        .....!............./
                        ..!.X!............./
                        .................../
                        .................../
                        ....x............../
                        .x................./
                        .................../
                        .................../
                        ...................",
                    room_2 = "
                        .................../
                        .x................./
                        .......x.........../
                        .................../
                        ....x............../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .x................./
                        .......x.........../
                        .................../
                        ....x............../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../",
                    room_3 = "
                        .................../
                        .................../
                        .................../
                        ..!...!............/
                        ....B............../
                        ..!...!............/
                        ...!!!............./
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        ..!...!............/
                        ....B............../
                        ..!...!............/
                        ...!!!............./
                        .................../
                        .................../
                        ...................",
                    room_4 = "
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        ...................",
                    room_5 = "
                        .................../
                        .................../
                        ....#............../
                        ...###............./
                        ..#####............/
                        ...###............./
                        ....#............../
                        .................../
                        ...........##....../
                        ..........####...../
                        .........######..../
                        ........########.../
                        .......##########../
                        .......##########../
                        ........########.../
                        .........######..../
                        ..........####...../
                        ...........##....../
                        ...................",
                    room_6 = "
                        .................../
                        .................../
                        ...#############.../
                        ...#...........#.../
                        ...#...........#.../
                        ...#..#######..#.../
                        ...#..#.....#..#.../
                        ...#..#.....#..#.../
                        ...#..#.....#..#.../
                        .................../
                        .................../
                        ...#############.../
                        .................../
                        .................../
                        ...#############.../
                        .................../
                        .................../
                        ...#############.../
                        ..................."
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
                    room_matrix <- matrix("#", nrow = SINGLE_ROOM_H, ncol = SINGLE_ROOM_W)
                    interior_matrix <- do.call( rbind, strsplit(lines, ""))

                    room_matrix[2:(SINGLE_ROOM_H-1), 2:(SINGLE_ROOM_W-1)] <- interior_matrix

                    # Add to selection list
                    rooms_selection_list[[length(rooms_selection_list) + 1]] <- room_matrix
                }

            }
        }
    }

    { # ---NPCs---
        enemies <- data.frame(
            symbol = character(),
            y = integer(),
            x = integer(),
            move_cooldown = integer(),
            max_move_cooldown = integer(),
            hp = integer(),
            max_hp = integer(),
            atk = integer()
        )

        enemies_map <- matrix(0, nrow = nrow(world_map), ncol = ncol(world_map))

        enemies_hp_to_show <- c()
        print_enemy_hp <- FALSE
    }

    { # ---PLAYER---
        player <- list(
            y = SINGLE_ROOM_H * start_y + OFFSET + SINGLE_ROOM_H/2,
            x = SINGLE_ROOM_W * start_x + OFFSET + SINGLE_ROOM_W/2,
            hp = 100,
            max_hp = 100,
            atk = 15
        )
        print_player_hp <- TRUE

        dist_map <- matrix(Inf, nrow = nrow(world_map), ncol = ncol(world_map))

        sword_visibility <- FALSE
        sword_slash <- c(0, 0)
    }
}



main_loop()
