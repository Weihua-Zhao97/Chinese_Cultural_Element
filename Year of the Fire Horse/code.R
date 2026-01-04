library(ggplot2)
library(gganimate)
library(dplyr) 

# Horse body data
h_body <- data.frame(
  x = c(0.6, 0, 1.7, 1.9, 3.6, 6.1, 6.5, 5.5, 3.2, 2, 1.6),  
  y = c(2.7, 3.3, 5.2, 5.9, 4, 3.7, 3.4, 1.5, 1.5, 3, 3.5)   
)

# Four legs data
leg_fr <- data.frame(
  x = c(6.5, 7.2, 6.4, 5.5),
  y = c(3.4, 0, 0, 1.5)
)

leg_fl <- data.frame(
  x = c(5.5, 6.4, 6.2, 5.3),
  y = c(1.5, 0, 0, 1.4)
)

leg_rr <- data.frame(
  x = c(3.2, 2.7, 1.8, 2.0),
  y = c(1.5, 0, 0, 3.0)
)

leg_rl <- data.frame(
  x = c(2.0, 1.8, 1.6, 2.2),
  y = c(3.0, 0, 0, 1.5)
)

# Mane and saddle
b <- data.frame(
  x = c(2.5, 2.8, 4.1, 3.6),
  y = c(5.1, 5.6, 4.4, 4)
)

t1 <- data.frame(
  x = c(6.1, 7.8, 8.7, 6.2),
  y = c(3.7, 7, 6.2, 3.6)
)

t2 <- data.frame(
  x = c(6.2, 8.7, 9.5, 6.35),
  y = c(3.6, 6.2, 5.3, 3.5)
)

t3 <- data.frame(
  x = c(6.35, 9.5, 10, 6.5),
  y = c(3.5, 5.3, 4.2, 3.4)
)

saddle <- data.frame(
  x = c(4, 5.3, 
        seq(5.3, 4, length.out = 50)[-1], 
        4),  
  y = c(4, 3.85, 
        3.8 + 0.8 * (seq(5.3, 4, length.out = 50)[-1] - 4.65)^2 - 1.2, 
        3.9)
)

# Leg rotation function
transform_leg <- function(leg_data, pivot_x, pivot_y, angle) {
  leg_data <- leg_data %>% na.omit()
  if(nrow(leg_data) < 3) return(leg_data) 
  
  rad <- angle * pi/180 * 8 
  rad <- ifelse(is.na(rad), 0, rad) 
  
  x_shifted <- leg_data$x - pivot_x
  y_shifted <- leg_data$y - pivot_y
  
  x_rot <- x_shifted * cos(rad) - y_shifted * sin(rad)
  y_rot <- x_shifted * sin(rad) + y_shifted * cos(rad)
  
  res <- data.frame(
    x = pmax(x_rot + pivot_x, 0.1), 
    y = pmax(y_rot + pivot_y, 0.1)  
  )
  
  res <- res[!duplicated(res), ] 
  return(res)
}

# Create 2026 Fire Horse animation data
create_2026_fire_horse_animation <- function(n_frames = 40) {
  all_data <- list()
  
  for (frame in 1:n_frames) {
    # Horse leg animation
    angle_fr <- 0.15 * sin(2 * pi * (frame - 1)/n_frames)   
    angle_fl <- -0.15 * sin(2 * pi * (frame - 1)/n_frames)  
    angle_rr <- -0.15 * sin(2 * pi * (frame - 1)/n_frames)  
    angle_rl <- 0.15 * sin(2 * pi * (frame - 1)/n_frames)   
    
    leg_fr_rot <- transform_leg(leg_fr, pivot_x = 6.5, pivot_y = 3.4, angle = angle_fr)
    leg_fl_rot <- transform_leg(leg_fl, pivot_x = 5.5, pivot_y = 1.5, angle = angle_fl)
    leg_rr_rot <- transform_leg(leg_rr, pivot_x = 3.2, pivot_y = 1.5, angle = angle_rr)
    leg_rl_rot <- transform_leg(leg_rl, pivot_x = 2.0, pivot_y = 3.0, angle = angle_rl)
    
    # Fire effect data
    fire_alpha <- 0.6 + 0.4 * sin(frame * 0.3)  # Fire flickering effect
    
    # Fire particles (around horse body)
    n_fire_particles <- 20
    fire_particles <- data.frame(
      x = runif(n_fire_particles, 1, 9),
      y = runif(n_fire_particles, 0.5, 6),
      size = runif(n_fire_particles, 0.5, 3),
      alpha = fire_alpha * runif(n_fire_particles, 0.3, 0.9),
      frame = frame
    )
    
    # 2026 text animation
    if (frame <= 20) {
      # Text gradually appears
      text_alpha <- frame / 20
      text_size <- 6 + (frame / 20) * 4
      text_y <- 8.5 - (frame / 20) * 1  # Move from top to bottom
    } else {
      # Stay visible
      text_alpha <- 1
      text_size <- 10
      text_y <- 7.5
    }
    
    # Horse color gradient (becomes redder over time, symbolizing fire horse)
    horse_color_red <- min(1.0, 0.6 + (frame / n_frames) * 0.4)  # Increase red component
    horse_color_green <- max(0.4, 0.7 - (frame / n_frames) * 0.3)  # Decrease green component
    horse_color_blue <- max(0.3, 0.5 - (frame / n_frames) * 0.2)  # Decrease blue component
    
    horse_color <- rgb(horse_color_red, horse_color_green, horse_color_blue)
    
    all_data[[frame]] <- list(
      body = data.frame(h_body, part = "body", frame = frame),
      leg_fr = data.frame(leg_fr_rot, part = "leg_fr", frame = frame),
      leg_fl = data.frame(leg_fl_rot, part = "leg_fl", frame = frame),
      leg_rr = data.frame(leg_rr_rot, part = "leg_rr", frame = frame),
      leg_rl = data.frame(leg_rl_rot, part = "leg_rl", frame = frame),
      b = data.frame(b, part = "mane1", frame = frame),
      t1 = data.frame(t1, part = "mane2", frame = frame),
      t2 = data.frame(t2, part = "mane3", frame = frame),
      t3 = data.frame(t3, part = "mane4", frame = frame),
      saddle = data.frame(saddle, part = "saddle", frame = frame),
      eye = data.frame(x = 1.5, y = 4.5, frame = frame),
      fire = fire_particles,
      text_2026 = data.frame(
        x = 5,
        y = text_y,
        label = "2026\nYear of the Fire Horse",
        alpha = text_alpha,
        size = text_size,
        frame = frame
      ),
      horse_color = horse_color
    )
  }
  
  return(all_data)
}

# Generate animation data
animation_data <- create_2026_fire_horse_animation(40)

# Extract data
all_body <- do.call(rbind, lapply(animation_data, function(f) f$body))
all_leg_fr <- do.call(rbind, lapply(animation_data, function(f) f$leg_fr))
all_leg_fl <- do.call(rbind, lapply(animation_data, function(f) f$leg_fl))
all_leg_rr <- do.call(rbind, lapply(animation_data, function(f) f$leg_rr))
all_leg_rl <- do.call(rbind, lapply(animation_data, function(f) f$leg_rl))
all_b <- do.call(rbind, lapply(animation_data, function(f) f$b))
all_t1 <- do.call(rbind, lapply(animation_data, function(f) f$t1))
all_t2 <- do.call(rbind, lapply(animation_data, function(f) f$t2))
all_t3 <- do.call(rbind, lapply(animation_data, function(f) f$t3))
all_saddle <- do.call(rbind, lapply(animation_data, function(f) f$saddle))
all_eye <- do.call(rbind, lapply(animation_data, function(f) f$eye))
all_fire <- do.call(rbind, lapply(animation_data, function(f) f$fire))
all_text <- do.call(rbind, lapply(animation_data, function(f) f$text_2026))

# Extract dynamic horse colors
horse_colors <- sapply(animation_data, function(f) f$horse_color)

# Create 2026 Fire Horse animation
fire_horse_anim <- ggplot() +
  # Background
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#AC2B2C", color = NA),
    plot.background = element_rect(fill = "#AC2B2C", color = NA)
  ) +
  
  # Fire particle effects
  geom_point(data = all_fire,
             aes(x = x, y = y, size = size, alpha = alpha),
             color = "#FF4500", shape = 16) +
  scale_size_identity() +
  scale_alpha_identity() +
  
  # Horse body (using dynamic colors)
  geom_polygon(data = all_body,
               aes(x = x, y = y, group = frame),
               fill = rep(horse_colors, each = nrow(h_body)),
               color = NA) +
  
  # Horse mane (light color remains unchanged)
  geom_polygon(data = all_b, 
               aes(x = x, y = y, group = frame), 
               fill = "#F6EBDF", color = NA) +
  geom_polygon(data = all_t1, 
               aes(x = x, y = y, group = frame), 
               fill = "#F6EBDF", color = NA) +
  geom_polygon(data = all_t2, 
               aes(x = x, y = y, group = frame), 
               fill = "#F9F5EF", color = NA) +
  geom_polygon(data = all_t3, 
               aes(x = x, y = y, group = frame), 
               fill = "#F6EBDF", color = NA) +
  
  # Saddle (dark color remains unchanged)
  geom_polygon(data = all_saddle, 
               aes(x = x, y = y, group = frame), 
               fill = "#5A3F33", color = NA) +
  
  # Eye
  geom_point(data = all_eye, 
             aes(x = x, y = y), 
             size = 2, color = "black", fill = "black", shape = 21) +
  
  # Four legs (use same dynamic color as body)
  geom_polygon(data = all_leg_fr, 
               aes(x = x, y = y, group = frame), 
               fill = rep(horse_colors, each = nrow(leg_fr)), color = NA) +
  geom_polygon(data = all_leg_fl, 
               aes(x = x, y = y, group = frame), 
               fill = rep(horse_colors, each = nrow(leg_fl)), color = NA) +
  geom_polygon(data = all_leg_rr, 
               aes(x = x, y = y, group = frame), 
               fill = rep(horse_colors, each = nrow(leg_rr)), color = NA) +
  geom_polygon(data = all_leg_rl, 
               aes(x = x, y = y, group = frame), 
               fill = rep(horse_colors, each = nrow(leg_rl)), color = NA) +
  
  # 2026 Fire Horse text - SIMPLE GOLD COLOR
  geom_text(data = all_text,
            aes(x = x, y = y, label = label, 
                alpha = alpha, size = size),
            color = "gold", 
            fontface = "bold",
            hjust = 0.5,
            vjust = 0.5,
            lineheight = 0.8) +
  scale_alpha_identity() +
  scale_size_identity() +
  coord_fixed(ratio = 1) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 9)) +
  
  # Animation
  transition_states(frame, transition_length = 0.3, state_length = 0.1) +
  ease_aes("linear")

# Render 2026 Fire Horse animation
animate(fire_horse_anim,
        nframes = 40,
        fps = 10,
        width = 800,
        height = 600,
        renderer = gifski_renderer(),
        detail = 5)
                       
anim_save("2026_fire_horse_year.gif", animation = last_animation())
