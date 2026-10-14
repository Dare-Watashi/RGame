print_rgb_range <- function() {
	for (channel in c("red", "green", "blue", "grayscale")) {
		cat(channel, ": ")
		for (value in 0:255) {
			rgb <- switch(channel,
				red = c(value, 0, 0),
				green = c(0, value, 0),
				blue = c(0, 0, value),
				grayscale = rep(value, 3)
			)
			cat(sprintf("\033[48;2;%d;%d;%dm  \033[0m", rgb[1], rgb[2], rgb[3]))
		}
		cat("\n")
	}
}

print_rgb_range()

cat("\033[2J\033[H\n\n\n\n\n") # Clear the screen and move the cursor to the top-left corner


# ansi code for brown
cat("\033[38;2;165;42;42m  This text is brown\033[0m")
cat("\033[48;2;165;42;42m  This text is brown\033[0m")
