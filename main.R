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
    
    # We activate keys on held down and additionally remove them on lifting finger
    tkbind(CONTROLLER, "<KeyPress-w>", function(){ current_key <<-"w"})                 # Bind the 'w' key to set current_key to "w"
    tkbind(CONTROLLER, "<KeyPress-a>", function(){ current_key <<-"a"})                 # Bind the 'a' key to set current_key to "a"
    tkbind(CONTROLLER, "<KeyPress-s>", function(){ current_key <<-"s"})                 # Bind the 's' key to set current_key to "s"
    tkbind(CONTROLLER, "<KeyPress-d>", function(){ current_key <<-"d"})                 # Bind the 'd' key to set current_key to "d"
    tkbind(CONTROLLER, "<KeyPress-q>", function(){ current_key <<-"q"})                 # Bind the 'q' key to set current_key to "q"
    tkbind(CONTROLLER, "<KeyPress-Left>", function(){ current_key <<-"left"})           # Bind the left arrow key to set current_key to "left"
    tkbind(CONTROLLER, "<KeyPress-Right>", function(){ current_key <<-"right"})         # Bind the right arrow key to set current_key to "right"
    tkbind(CONTROLLER, "<KeyPress-Up>", function(){ current_key <<-"up"})               # Bind the up arrow key to set current_key to "up"
    tkbind(CONTROLLER, "<KeyPress-Down>", function(){ current_key <<-"down"})           # Bind the down arrow key to set current_key to "down"
    tkbind(CONTROLLER, "<KeyPress-p>", function(){ current_key <<-"p"})                 # Bind the 'p' key to set current_key to "p"
    tkbind(CONTROLLER, "<KeyPress-r>", function(){ current_key <<-"r"})                 # Bind the 'r' key to set current_key to "r"
    tkbind(CONTROLLER, "<KeyPress-Return>", function(){ current_key <<-"enter"})                 # Bind the 'r' key to set current_key to "r"

    # clearing
    tkbind(CONTROLLER, "<KeyRelease-w>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-a>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-s>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-d>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-q>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-Left>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-Right>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-Up>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-Down>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-p>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-r>", function(){ current_key <<- " "})
    tkbind(CONTROLLER, "<KeyRelease-Return>", function(){ current_key <<- " "})
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
            if(row > 1 && map_preview_matrix[row-1, col] == 25 && (runif(1) < 0.5 || !first_room_placed)) {
                map_preview_matrix[row-1, col] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row-1, col)
                first_room_placed <<- TRUE
            }
            if(row < MAP_ROOM_COUNT_Y && map_preview_matrix[row+1, col] == 25 && (runif(1) < 0.5 || !first_room_placed)) {
                map_preview_matrix[row+1, col] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row+1, col)
                first_room_placed <<- TRUE
            }
            if(col > 1 && map_preview_matrix[row, col-1] == 25 && (runif(1) < 0.5 || !first_room_placed)) {
                map_preview_matrix[row, col-1] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row, col-1)
                first_room_placed <<- TRUE
            }
            if(col < MAP_ROOM_COUNT_X && map_preview_matrix[row, col+1] == 25 && (runif(1) < 0.5 || !first_room_placed)) {
                map_preview_matrix[row, col+1] <<- map_preview_matrix[row, col] + 1
                unlock_adjacent_cells(row, col+1)
                first_room_placed <<- TRUE
            }
        }

        create_map_preview_matrix <- function() {
            start_y <<- sample(1:MAP_ROOM_COUNT_Y, 1)
            start_x <<- sample(1:MAP_ROOM_COUNT_X, 1)
            map_preview_matrix[start_y, start_x] <<- 0

            unlock_adjacent_cells(start_y, start_x)
        }


        place_room <- function(row, col, room = NA) {
            if(any(is.na(room))){
                room <- sample(rooms_selection_list, 1)[[1]]
                world_map[(row-(SINGLE_ROOM_H%/%2)):(row+(SINGLE_ROOM_H%/%2)), (col-(SINGLE_ROOM_W%/%2)):(col+(SINGLE_ROOM_W%/%2))] <<- room
            }
            else{
                world_map[(row-(SINGLE_ROOM_H%/%2)):(row+(SINGLE_ROOM_H%/%2)), (col-(SINGLE_ROOM_W%/%2)):(col+(SINGLE_ROOM_W%/%2))] <<- room
            }
        }

        fill_world_with_rooms <- function() {
            room_place_offset_y <- OFFSET + 1 + SINGLE_ROOM_H%/%2
            room_place_offset_x <- OFFSET + 1 + SINGLE_ROOM_W%/%2

            room_s <- min(map_preview_matrix)
            room_e <- max(map_preview_matrix[map_preview_matrix != 25])
            #room_e <- min(map_preview_matrix) + 1
            for(y in 0:(MAP_ROOM_COUNT_Y-1)){
                for(x in 0:(MAP_ROOM_COUNT_X-1)){
                    if(map_preview_matrix[y+1, x+1] != 25) {
                        if(map_preview_matrix[y+1, x+1] == room_s) {
                            place_room( room_place_offset_y + (SINGLE_ROOM_H-1)*y, room_place_offset_x + (SINGLE_ROOM_W-1)*x, special_rooms_list[2][[1]])
                            player$y <<- room_place_offset_y + (SINGLE_ROOM_H-1)*y-2
                            player$x <<- room_place_offset_x + (SINGLE_ROOM_W-1)*x
                        } else if(map_preview_matrix[y+1, x+1] == room_e) {
                            place_room( room_place_offset_y + (SINGLE_ROOM_H-1)*y, room_place_offset_x + (SINGLE_ROOM_W-1)*x, special_rooms_list[1][[1]])
                        }
                        else{
                            place_room( room_place_offset_y + (SINGLE_ROOM_H-1)*y, room_place_offset_x + (SINGLE_ROOM_W-1)*x)
                        }
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
                "left" = {slash_sword(0, -1); current_key <<- " "},
                "right" = {slash_sword(0, 1); current_key <<- " "},
                "up" = {slash_sword(-1, 0); current_key <<- " "},
                "down" = {slash_sword(1, 0); current_key <<- " "},
                "r" = {regen_hp()},
                "q" = {running <<- FALSE},
                "enter" = {debug_loop(); current_key <<- " "}
            )
            # current_key <<- " "
        }
    }

    { # ---ENEMY FUNCTIONS---

        get_enemy_data <- function(type, x, y) {
            switch(type,
                "x" = data.frame(symbol = world_map[y, x], y = y, x = x, move_cooldown = 0, max_move_cooldown = 5, hp = 100, max_hp = 100, atk = 1, awareness = 5),
                "X" = data.frame(symbol = world_map[y, x], y = y, x = x, move_cooldown = 0, max_move_cooldown = 5, hp = 150, max_hp = 150, atk = 2, awareness = 15),
                "B" = data.frame(symbol = world_map[y, x], y = y, x = x, move_cooldown = 0, max_move_cooldown = 10, hp = 200, max_hp = 200, atk = 3, awareness = 25)
            )
        }

        get_enemies_from_world_map <- function() {
            enemies <<- enemies[0, ] # Clear the dataframe while preserving columns
            enemies_hp_to_show <<- c()
            
            for(y in (OFFSET+1):(nrow(world_map)-OFFSET)){
                for(x in (OFFSET+1):(ncol(world_map)-OFFSET)){
                    if(world_map[y, x] %in% c("x", "X", "B")){
                        enemies[nrow(enemies)+1,] <<- get_enemy_data(world_map[y, x], x, y)
                        world_map[y, x] <<- " "
                        enemies_hp_to_show <<- c(enemies_hp_to_show, -1)
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
            
            

            enemies_attack()
        }

        remove_dead_enemies <- function(){
            if(any(enemies[, "hp"] <= 0)) {
                enemies_hp_to_show <<- enemies_hp_to_show[enemies_hp_to_show != 0]
                enemies_hp_to_show[enemies[, "hp"] <= 0] <<- 0
                enemies <<- enemies[enemies[, "hp"] > 0, ]
            }
        }

        enemies_attack <- function() {
            for(i in enemies_map[dist_map == 1 & enemies_map != 0]) {
                if(is.na(enemies[i, "atk"])) next
                if(enemies[i, "move_cooldown"] == 0) {
                    player$hp <<- player$hp - enemies[i, "atk"]
                    b_print_player_hp <<- TRUE
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

        regen_hp <- function() {
            if(player$awareness >= 5 && player$hp < player$max_hp){
                player$awareness <<- player$awareness - 5
                player$hp <<- min(player$hp + 5, player$max_hp); 
                b_print_player_hp <<- TRUE
                b_print_player_awareness <<- TRUE
            }
        }

        player_move <- function(y_offset, x_offset) {
            # check if there is collide object at new position or an enemy
            if(!(world_map[player$y + y_offset, player$x + x_offset] %in% COLLIDE_OBJECTS) && 
               !(any(enemies$y == player$y + y_offset & enemies$x == player$x + x_offset))
            ) {
                player$x <<- player$x + x_offset
                player$y <<- player$y + y_offset
                
                update_player_sense_aura()
                check_collect_loot()
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
                        enemies_map[enemies[i, "y"], enemies[i, "x"]] <<- 0
                        player$awareness <<- player$awareness + enemies[i, "awareness"]
                    }
                    enemies_hp_to_show[i] <<- TRUE
                    print_enemy_hp <<- TRUE
                    break
                }
            }
            sword_visibility <<- TRUE
        }

        check_collect_loot <- function(){
            # awareness
            if(world_map[player$y, player$x] == "a"){
                player$awareness <<- player$awareness + 10
                world_map[player$y, player$x] <<- " "
                b_print_player_awareness <<- TRUE
            }
            else if(world_map[player$y, player$x] == "A"){
                player$awareness <<- player$awareness + 20
                world_map[player$y, player$x] <<- " "
                b_print_player_awareness <<- TRUE
            }
        }

        check_game_over <- function() {
            if(player$hp <= 0) {
                return("HP LOSS")
            }
            if(player$sanity <= 0) {
                return("SANITY LOSS")
            }
            if(player$awareness >= 100) {
                return("AWARENESS OVERLOAD")
            }
            return(FALSE)
        }

        check_map_passed <- function(viewport) {
            if(world_map[player$y, player$x] == "_"){
                print_enemy_hp <<- FALSE
                b_print_player_hp <<- TRUE
                return(TRUE)
            }
            return(FALSE)
        }
    }

    { # ---SIDE DATA FUNCTIONS---
        get_player_hp_bar <- function() {
            hp_bar <- paste(rep(COLOUR_PALETTE["█"], player$hp/5), collapse = "")
            hp_bar <- paste(hp_bar, paste(rep(COLOUR_PALETTE["░"], player$max_hp/5 - player$hp/5), collapse = ""), sep = "")
            return(hp_bar)
        }
        get_player_sanity_bar <- function() {
            sanity_bar <- paste(rep(COLOUR_PALETTE["█s"], player$sanity/5), collapse = "")
            sanity_bar <- paste(sanity_bar, paste(rep(COLOUR_PALETTE["░s"], player$max_sanity/5 - player$sanity/5), collapse = ""), sep = "")
            return(sanity_bar)
        }
        get_player_awareness_bar <- function() {
            awareness_bar <- paste(rep(COLOUR_PALETTE["█a"], player$awareness/5), collapse = "")
            awareness_bar <- paste(awareness_bar, paste(rep(COLOUR_PALETTE["░a"], player$max_awareness/5 - player$awareness/5), collapse = ""), sep = "")
            return(awareness_bar)

        }

        get_enemy_hp_bar <- function(i) {
            hp_bar <- ""
            if((i > 0) && (!is.na(enemies[i, "hp"])) && (enemies[i, "hp"] > 0)){
                cat(enemies[i, "hp"])
                hp_bar <- paste(rep(COLOUR_PALETTE["█"], enemies[i, "hp"]%/%5), collapse = "")
                hp_bar <- paste(hp_bar, paste(rep(COLOUR_PALETTE["░"], enemies[i, "max_hp"]%/%5 - enemies[i, "hp"]%/%5), collapse = ""), sep = "")
                return(hp_bar)
            }
            else{
                clear_screen <<- TRUE
                enemies_hp_to_show[i] <<- 0
                return("DEAD                        ")
            }
        }

        print_side_data <- function() {
            # print debug data - printed at column 70
            cat("\033[H\033[70C")
            cat("FPS: ", round(1/current_time_diff, 0), "\n")

            # player and enemies data - printed at column 15
            if(b_print_player_hp) {
                cat("\033[H\033[15C")
                # coloured hp bar
                cat("Player HP: ", get_player_hp_bar(), "\n")
                b_print_player_hp <<- FALSE
            } else cat("\n")
            if(b_print_player_sanity){
                cat("\033[H\n\033[15C")
                # coloured sanity bar
                cat("Player Sanity: ", get_player_sanity_bar(), "\n")
                b_print_player_sanity <<- FALSE
            } else cat("\n")
            if(b_print_player_awareness){
                cat("\033[H\n\n\033[15C")
                # coloured awareness bar
                cat("Player Awareness: ", get_player_awareness_bar(), "\n")
                b_print_player_awareness <<- FALSE
            } else cat("\n")
     
            if(print_enemy_hp) {
                for(i in length(enemies_hp_to_show):1) {
                    if(enemies_hp_to_show[i] != -1) {
                        hp_bar <- get_enemy_hp_bar(i)
                        cat("\033[35C")
                        cat("Enemy HP: ", hp_bar, "\n")
                    }
                    if(enemies_hp_to_show[i] == 0){
                        enemies_hp_to_show[i] <<- enemies_hp_to_show[-i]
                        enemies[i, ] <<- enemies[-i, ]
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
                b_print_player_hp <<- TRUE
                b_print_player_sanity <<- TRUE
                b_print_player_awareness <<- TRUE
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

    { # ---DEBUG FUNCTIONS---

        debug_show_map <- function(){
            for(i in OFFSET:(nrow(world_map)-OFFSET)){
                for(j in OFFSET:(ncol(world_map)-OFFSET)){
                    cat(COLOUR_PALETTE[world_map[i, j]])
                }
                cat("\n")
            }
        }

        debug_show_rooms <- function(){
            for(i in 1:nrow(map_preview_matrix)){
                for(j in 1:ncol(map_preview_matrix)){
                    cat(map_preview_matrix[i, j], "\t")
                }
                cat("\n")
            }
        }

        debug_add_object <- function(){
            type <- readline(prompt = "\ttype (wall [w], floor [f], enemy [x, X, B], door [[], ]], awareness chest [a, A]): ")
            
            # checking the input without trailing whitespace
            if(trimws(type) %in% c("w", "f", "x", "X", "B", "[]", "]", "a", "A")){
                posy <- readline(prompt = "\tposy (relative to player): ")
                posx <- readline(prompt = "\tposx (relative to player): ")
                switch(trimws(type),
                    "w" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "#"},
                    "f" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- " "},
                    "x" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "x"},
                    "X" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "X"},
                    "B" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "B"},
                    "[]" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "]"},
                    "[" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- " "},
                    "a" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "a"},
                    "A" = {world_map[player$y + as.numeric(posx), player$x + as.numeric(posy)] <<- "A"}
                )
            }
        }

        execute_command <- function(cmd){
            switch(cmd,
                "quit" = {debugging <<- FALSE},
                "quit game" = {running <<- FALSE; debugging <<- FALSE},
                "clear" = {cat("\033[2J\033[H")},
                "set hp" = {player$hp <<- as.numeric(readline(prompt = "amount = "))},
                "set sn" = {player$sanity <<- as.numeric(readline(prompt = "amount = "))},
                "set aw" = {player$awareness <<- as.numeric(readline(prompt = "amount = "))},
                "show map" = {debug_show_map()},
                "show rooms" = {debug_show_rooms()},
                "add object" = {debug_add_object()},
                NULL
            )
        }

        debug_loop <- function(){
            # clear screen, show cursor and return to top left courner
            cat("\033[2J\033[?25h\033[H")
            bringToTop(which = -1) # moves console to focus

            debugging <<- TRUE
            while(debugging){
                cat("Debug mode\n")
                print("Available commands: quit, quit game, clear, set hp, set sn, set aw, show map, show rooms")
                cmd <- readline(prompt="> ")
                execute_command(cmd)
            }

            b_print_enemy_hp <<- TRUE
            b_print_player_hp <<- TRUE
            b_print_player_sanity <<- TRUE
            b_print_player_awareness <<- TRUE
            
            # refocus on tlck window
            tkfocus("-force", CONTROLLER)           # Force focus on the window

            # hide cursour, clear the screen and place cursor at top-left
            cat("\033[?25l\033[2J\033[H")

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

            if(check_map_passed()){
                cat("\033[?25l\033[2J\033[H") 
                assemble_world_map()
                cat("\033[?25l\033[2J\033[H")
                next;
            }

            update_enemies()

            # colour-wrap function \RGB - ~25ms - ~550ms
            # implicit colour \16c - ~20ms - ~378ms
            #remove_dead_enemies()
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
            
            game_over <- check_game_over()
            if(!is.logical(game_over)) {
                running <- FALSE
                cat("\033[2J\033[H")
                cat(paste0("Game Over! You died of ", game_over, "\n"))
            }
        }
    }
}

{ # ---GLOBAL DATA---
    { # ---GAME STATE---
        running <- TRUE
        debugging <- FALSE
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
            "_" = "_",      # clear
            "‾" = "‾",      # clear
            "shadow" = " ", # shadow
            "@" = "\033[32m@\033[0m",      # green
            "!" = "\033[33m!\033[0m",      # yellow
            "x" = "\033[31mx\033[0m",      # red
            "X" = "\033[31mx\033[0m",      # red
            "B" = "\033[31mX\033[0m",      # red
            "/" = "\033[37m/\033[0m",      # white
            "█" = "\033[38;2;255;0;0m█\033[0m",      # red
            "░" = "\033[38;2;255;0;0m░\033[0m",
            "█s" = "\033[38;2;100;0;0m█\033[0m",      # dark red
            "░s" = "\033[38;2;100;0;0m░\033[0m",
            "█a" = "\033[38;2;255;255;0m█\033[0m",      # yellow
            "░a" = "\033[38;2;255;255;0m░\033[0m",
            "a" = "\033[38;2;255;255;0ma\033[0m",
            "A" = "\033[38;2;255;255;0mA\033[0m",
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
        { # CONSTANTS and world variables
            SINGLE_ROOM_H <- 21
            SINGLE_ROOM_W <- 21
            OFFSET <- 10
            MAP_ROOM_COUNT_Y <- 5
            MAP_ROOM_COUNT_X <- 5
            map_preview_matrix <- matrix(25, nrow = MAP_ROOM_COUNT_Y, ncol = MAP_ROOM_COUNT_X)
            start_y <- 0
            start_x <- 0
            first_room_placed <- FALSE
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
                        ...#..#..a..#..#.../
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
                        ...................",
                    room_7 = "
                        .................. /
                        .................../
                        .#########........./
                        .#...a...#........./
                        .#.......#........./
                        .#...X...#........./
                        .#.......#####...../
                        .#...........#...../
                        .#######..X..#...../
                        .......#.....#...../
                        .......#.....#...../
                        .......#####.#...../
                        .............#...../
                        .......#######...../
                        .......#.........../
                        .......#...X......./
                        .......#.........../
                        .......#########.../
                        ...................",
                    room_8 = "
                        .................../
                        .................../
                        .................../
                        .......##.##......./
                        .......#...#......./
                        .......#...#......./
                        ...#####...#####.../
                        ...#...........#.../
                        ...#...........#.../
                        .........B........./
                        ...#...........#.../
                        ...#...........#.../
                        ...#####...#####.../
                        .......#...#......./
                        .......#...#......./
                        .......##.##......./
                        .................../
                        .................../
                        ...................",
                    room_9 = "
                        .................../
                        .................../
                        .................../
                        ......########...../
                        ......#..a...#...../
                        ......#..X...#...../
                        ......#......#...../
                        ......####.###...../
                        .........#........./
                        ......x..#..x....../
                        .........#........./
                        .....###.###......./
                        .....#.....#......./
                        .....#..X..#......./
                        .....#..a..#......./
                        .....#######......./
                        .................../
                        .................../
                        ...................",
                    room_10 = "
                        .................../
                        ..#####............/
                        ..#..a#............/
                        ..#.#######......../
                        ..#.......#......../
                        ..#####...#......../
                        ......#...#......../
                        ......#...######.../
                        ......#........#.../
                        ......######...#.../
                        ...........#...#.../
                        .......#####...#.../
                        ...............#.../
                        .......#########.../
                        .................../
                        .................../
                        .................../
                        .................../
                        ...................",
                    room_11 = "
                        .................../
                        .................../
                        .........x........./
                        .......#####......./
                        ......##.a.##....../
                        .....##.....##...../
                        ....##.......##..../
                        ...##....B....##.../
                        ....##.......##..../
                        .....##.....##...../
                        ......##...##....../
                        .......#...#......./
                        .................../
                        .................../
                        .................../
                        .........x........./
                        .................../
                        .................../
                        ...................",
                    room_12 = "
                        .................../
                        .................../
                        ...#############.../
                        ...#...........#.../
                        ...#..###.###..#.../
                        ...#..#.....#..#.../
                        ...#..#..x..#..#.../
                        ...#..#.....#..#.../
                        ...#..#######..#.../
                        ...#...........#.../
                        ...######.######.../
                        .................../
                        .....#########...../
                        .....#.......#...../
                        .....#...B...#...../
                        .....#.......#...../
                        .....####.####...../
                        .................../
                        ...................",
                    room_13 = "
                        .................../
                        .................../
                        ..################./
                        .................#./
                        ..##############.#./
                        ..#............#.#./
                        ..#.##########.#.#./
                        ..#.#........#.#.#./
                        ..#.#.######.#.#.#./
                        ..#.#.#....#.#.#.#./
                        ..#.#.#.#A.#.#.#.#./
                        ..#.#.#.####.#.#.#./
                        ..#.#.#......#.#.#./
                        ..#.#.########.#.#./
                        ..#.#..........#.#./
                        ..#.############.#./
                        ..#..............#./
                        ..################./
                        ...................",
                    room_14 = "
                        #########.#########/
                        #.......#.........#/
                        #.#####.#.#######.#/
                        #.#...#.#.....#...#/
                        #.#.#.#.#####.#.###/
                        #...#.#.....#.#...#/
                        #####.#####.#.###.#/
                        #.....#.....#...#.#/
                        #.#####.#######.#.#/
                        ..#.....#.......#../
                        #.#.#####.#######.#/
                        #.#.......#.......#/
                        #.#########.#######/
                        #.........#.......#/
                        #######.#.#####.#.#/
                        #.......#.....#.#.#/
                        #.###########.#.#.#/
                        #.............#...#/
                        #########.#########",
                    room_15 = "
                        .................../
                        ###################/
                        .................../
                        ###########.#######/
                        .................../
                        ###############.###/
                        .................../
                        ###.###############/
                        .................../
                        .#################./
                        .................../
                        ##############.####/
                        .................../
                        ##########.########/
                        .................../
                        ##################./
                        .................../
                        #######.###########/
                        ..................."
                )

                special_rooms <- list(
                    room_end = "
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        ........###......../
                        .......#...#......./
                        ......#.._..#....../
                        ......#.....#....../
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
                    room_start = "
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        ......#.....#....../
                        ......#..‾..#....../
                        .......#...#......./
                        ........###......../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        .................../
                        ..................."
                )
                
                rooms_selection_list <- list()
                special_rooms_list <- list()

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

                for (room_template in special_rooms) {
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
                    special_rooms_list[[length(special_rooms_list) + 1]] <- room_matrix
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
            atk = integer(),
            awareness = integer()
        )

        enemies_map <- matrix(0, nrow = nrow(world_map), ncol = ncol(world_map))

        enemies_hp_to_show <- c()
        b_print_enemy_hp <- FALSE
    }

    { # ---PLAYER---
        player <- list(
            y = SINGLE_ROOM_H * start_y + OFFSET + SINGLE_ROOM_H/2,
            x = SINGLE_ROOM_W * start_x + OFFSET + SINGLE_ROOM_W/2,
            hp = 100,
            max_hp = 100,
            sanity = 100,
            max_sanity = 100,
            awareness = 0,
            max_awareness = 100,
            atk = 15
        )
        b_print_player_hp <- TRUE
        b_print_player_sanity <- TRUE
        b_print_player_awareness <- TRUE

        dist_map <- matrix(Inf, nrow = nrow(world_map), ncol = ncol(world_map))

        sword_visibility <- FALSE
        sword_slash <- c(0, 0)
    }
}



main_loop()
