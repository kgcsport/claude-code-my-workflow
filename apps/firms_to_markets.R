# ============================================================
# 2 panels (Firm | Market), 2 phases (Short run then Long run)
# Demand shocks: demand shifts in SR; supply adjusts in LR back to baseline P_be0
# Fixed-cost shocks: ATC shifts in SR; supply adjusts in LR to new break-even
# Outputs 4 gifs in ./gifs:
#  demand_out_2panel.gif, demand_in_2panel.gif, fixedcost_up_2panel.gif, fixedcost_down_2panel.gif
# ============================================================

library(tidyverse)
library(gganimate)
library(gifski)
library(magick)

# ---------------------------
# 0) Baseline firm primitives
# ---------------------------
q <- seq(0.5, 12, length.out = 150)  # reduced from 400 — still smooth curves

AVC_base <- 2 + (q - 3)^2 / 6
MC_base  <- 0.5 * (q^2 - 4*q + 7)

F0 <- 12                               # baseline fixed cost (AFC=F/q)
AFC_from_F <- function(F, q) F / q
ATC_from_F <- function(F, q) AVC_base + AFC_from_F(F, q)

# shutdown (min AVC), independent of fixed cost
p_shutdown <- min(AVC_base)

# baseline break-even
p_be0 <- min(ATC_from_F(F0, q))

# lookup for joins (avoids vector recycling bugs)
base_lookup <- tibble(q = q, AVC_base = AVC_base, MC_base = MC_base)

interp <- function(x, xgrid, ygrid) approx(xgrid, ygrid, xout = x, rule = 2)$y

# numeric q*(P): choose q minimizing |MC-P|
q_star_from_P <- function(P, MC_vec = MC_base) q[which.min(abs(MC_vec - P))]

# ---------------------------
# 1) Industry primitives
# Demand: P = A - dQ
# Supply (reduced form): P = a + bQ  (entry/exit shifts a)
# ---------------------------
A0 <- 20
d  <- 2
b  <- 0.6

# baseline supply intercept a0 so equilibrium price equals baseline break-even p_be0
# P* = (b*A + d*a)/(b + d)
a0 <- (p_be0*(b + d) - b*A0) / d

eq_from_Aa <- function(A, a) {
  Q <- (A - a) / (b + d)
  P <- (b*A + d*a) / (b + d)
  c(Q = Q, P = P)
}

# supply intercept to hit a target price p_target for a given demand intercept A
a_for_price <- function(p_target, A) (p_target*(b + d) - b*A) / d

p_be_from_F <- function(F) min(ATC_from_F(F, q))

# Initial equilibrium (for ghost markers)
eq0 <- eq_from_Aa(A0, a0)
Q0_eq <- eq0[["Q"]]
P0_eq <- eq0[["P"]]

# ---------------------------
# 2) Build SR + LR paths (both panels move together)
#    OPTIMIZED: vectorized — no rowwise()
# ---------------------------
make_paths <- function(scenario,
                       n_sr = 30, n_lr = 50,
                       lambda = 0.12) {

  # Defaults
  A_end <- A0
  F_end <- F0

  if (scenario == "demand_out") {
    A_end <- A0 + 6
  } else if (scenario == "demand_in") {
    A_end <- A0 - 6
  } else if (scenario == "fixedcost_up") {
    F_end <- 18
  } else if (scenario == "fixedcost_down") {
    F_end <- 6
  } else {
    stop("Unknown scenario")
  }

  # ---- Phase SR: shock + immediate response simultaneously ----
  A_vec <- if (scenario %in% c("demand_out","demand_in")) seq(A0, A_end, length.out = n_sr) else rep(A0, n_sr)
  F_vec <- if (scenario %in% c("fixedcost_up","fixedcost_down")) seq(F0, F_end, length.out = n_sr) else rep(F0, n_sr)
  a_vec <- rep(a0, n_sr)

  # Vectorized equilibrium: Q_eq = (A - a) / (b + d), P = (b*A + d*a) / (b + d)
  Q_eq_vec <- (A_vec - a_vec) / (b + d)
  P_vec    <- (b * A_vec + d * a_vec) / (b + d)

  # Vectorized break-even: sapply since p_be_from_F uses min() over q grid
  p_be_vec <- sapply(F_vec, p_be_from_F)

  sr <- tibble(
    frame = 1:n_sr,
    A = A_vec, a = a_vec, F = F_vec,
    p_be = p_be_vec, Q_eq = Q_eq_vec, P = P_vec
  )

  # ---- Phase LR: entry/exit (supply shifts) + profit restoration simultaneously ----
  A_hold <- if (scenario %in% c("demand_out","demand_in")) A_end else A0
  F_hold <- if (scenario %in% c("fixedcost_up","fixedcost_down")) F_end else F0

  p_target <- if (scenario %in% c("demand_out","demand_in")) p_be0 else p_be_from_F(F_hold)

  a_target <- a_for_price(p_target, A_hold)

  a_path <- numeric(n_lr)
  a_path[1] <- a0
  for (i in 2:n_lr) a_path[i] <- a_path[i-1] + lambda*(a_target - a_path[i-1])

  # Vectorized equilibrium for LR
  Q_eq_lr <- (A_hold - a_path) / (b + d)
  P_lr    <- (b * A_hold + d * a_path) / (b + d)
  p_be_lr <- rep(p_be_from_F(F_hold), n_lr)  # F is constant in LR

  lr <- tibble(
    frame = 1:n_lr,
    A = A_hold, F = F_hold, a = a_path,
    p_be = p_be_lr, Q_eq = Q_eq_lr, P = P_lr
  )

  list(sr = sr, lr = lr)
}

# ---------------------------
# 3) Firm curves + firm stats per frame
#    OPTIMIZED: static curves computed once, only price-dependent elements vary
# ---------------------------
firm_curves_long <- function(df) {
  # Check if F is constant across all frames (demand scenarios)
  F_vals <- unique(df$F)
  if (length(F_vals) == 1) {
    # Static curves — compute once, replicate per frame
    single <- tibble(q = q) %>%
      mutate(
        AFC = F_vals / q,
        AVC = AVC_base,
        ATC = AVC + AFC,
        MC  = MC_base
      ) %>%
      pivot_longer(cols = c(AFC, AVC, ATC, MC),
                   names_to = "curve_name", values_to = "y")

    # Cross with frames
    crossing(frame = df$frame, single) %>%
      mutate(curve_name = factor(curve_name, levels = c("AFC","AVC","ATC","MC")))
  } else {
    # F varies per frame — must compute per frame
    df %>%
      crossing(q = q) %>%
      left_join(base_lookup, by = "q") %>%
      mutate(
        AFC = F / q,
        AVC = AVC_base,
        MC  = MC_base,
        ATC = AVC + AFC
      ) %>%
      select(frame, q, AFC, AVC, ATC, MC) %>%
      pivot_longer(cols = c(AFC, AVC, ATC, MC),
                   names_to = "curve_name", values_to = "y") %>%
      mutate(curve_name = factor(curve_name, levels = c("AFC","AVC","ATC","MC")))
  }
}

firm_stats <- function(df) {
  # Vectorized: use sapply for q_star and interp
  P_vec <- df$P
  F_vec <- df$F

  q_star_vec <- sapply(P_vec, function(P) {
    if (P >= p_shutdown) q_star_from_P(P) else 0
  })

  AFC_star_vec <- ifelse(q_star_vec > 0, F_vec / q_star_vec, NA_real_)
  AVC_star_vec <- ifelse(q_star_vec > 0,
    sapply(q_star_vec, function(qs) if (qs > 0) interp(qs, q, AVC_base) else NA_real_),
    NA_real_
  )
  ATC_star_vec <- ifelse(q_star_vec > 0, AVC_star_vec + AFC_star_vec, NA_real_)
  total_profit_vec <- ifelse(q_star_vec > 0, (P_vec - ATC_star_vec) * q_star_vec, NA_real_)
  is_profit_vec <- ifelse(q_star_vec > 0 & P_vec >= ATC_star_vec, TRUE, FALSE)
  profit_color_vec <- ifelse(is_profit_vec, "#2ca02c", "#d62728")

  df %>%
    mutate(
      q_star = q_star_vec,
      AFC_star = AFC_star_vec,
      AVC_star = AVC_star_vec,
      ATC_star = ATC_star_vec,
      total_profit = total_profit_vec,
      is_profit = is_profit_vec,
      profit_color = profit_color_vec,
      xmin = 0, xmax = q_star,
      ymin = if_else(is_profit, ATC_star, P),
      ymax = if_else(is_profit, P, ATC_star)
    )
}

# ---------------------------
# 4) Market lines per frame
#    OPTIMIZED: only 2 points per line (they're linear)
# ---------------------------
industry_lines_long <- function(df) {
  Qgrid <- c(0, 12)  # reduced from 240 — straight lines need only endpoints
  df %>%
    crossing(Q = Qgrid) %>%
    mutate(
      Pdemand = A - d*Q,
      Psupply = a + b*Q
    )
}

# ---------------------------
# 5) Plot builders (one phase at a time)
# ---------------------------
plot_firm_phase <- function(df, title, phase_label) {
  curves <- firm_curves_long(df)
  st <- firm_stats(df)

  ggplot() +
    geom_line(
      data = curves,
      aes(q, y, color = curve_name, group = interaction(curve_name, frame)),
      linewidth = 1.1
    ) +
    scale_color_manual(values = c("MC"="#008000","ATC"="purple","AVC"="#e31a1c","AFC"="#005f73")) +
    coord_cartesian(xlim = c(0, 12), ylim = c(0, 20), expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.25,0.82)) +
    labs(x = "Firm output q", y = "Cost / Price", color = NULL, title = title) +
    # Price line
    geom_hline(data = st, aes(yintercept = P), linewidth = 1.0) +
    # "P = MR" label on the price line
    geom_text(
      data = st,
      aes(x = 11.5, y = P + 0.6, label = "P = MR"),
      inherit.aes = FALSE, size = 3.2, hjust = 1
    ) +
    geom_vline(data = st, aes(xintercept = q_star), linetype = "dashed", alpha = 0.7) +
    # Profit/loss rectangle with color
    geom_rect(
      data = st %>% filter(q_star > 0 & !is.na(ymin) & !is.na(ymax)),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = profit_color),
      inherit.aes = FALSE, alpha = 0.3
    ) +
    scale_fill_identity() +
    # Profit/loss text label inside rectangle
    geom_text(
      data = st %>% filter(q_star > 0 & !is.na(ymin) & !is.na(ymax) & abs(ymax - ymin) > 0.3),
      aes(x = xmax / 2, y = (ymin + ymax) / 2,
          label = if_else(is_profit, "Profit", "Loss")),
      inherit.aes = FALSE, size = 3.5, fontface = "bold", color = "gray20"
    ) +
    geom_point(
      data = st %>% filter(q_star > 0),
      aes(x = q_star, y = P), size = 2.2
    ) +
    # Phase label
    annotate("text", x = 0.4, y = 19.2, hjust = 0, vjust = 1, size = 3.2,
             label = phase_label) +
    transition_manual(frame)
}

plot_market_phase <- function(df, title, phase_label,
                              is_lr = FALSE, a0_init = a0) {
  ind <- industry_lines_long(df)

  p <- ggplot() +
    # Demand curve (blue)
    geom_line(data = ind, aes(Q, Pdemand, group = frame),
              linewidth = 1.1, color = "#1f77b4") +
    # Supply curve (orange)
    geom_line(data = ind, aes(Q, Psupply, group = frame),
              linewidth = 1.1, color = "#ff7f0e") +
    # "D" label near the x-intercept of demand
    geom_text(
      data = df,
      aes(x = pmin(A / d - 0.5, 11), y = 1.5, label = "D"),
      inherit.aes = FALSE, size = 4.5, fontface = "bold",
      color = "#1f77b4"
    ) +
    # "S" label near the top-right of supply
    geom_text(
      data = df,
      aes(x = 11, y = pmin(a + b * 11 + 0.6, 19), label = "S"),
      inherit.aes = FALSE, size = 4.5, fontface = "bold",
      color = "#ff7f0e"
    ) +
    # Break-even price dashed line
    geom_hline(data = df, aes(yintercept = p_be), linetype = "dashed", alpha = 0.6) +
    # Equilibrium point
    geom_point(data = df, aes(Q_eq, P), size = 2.4) +
    # Initial equilibrium ghost marker (hollow circle)
    annotate("point", x = Q0_eq, y = P0_eq, size = 3, shape = 1,
             color = "gray50", stroke = 1.2) +
    coord_cartesian(xlim = c(0, 12), ylim = c(0, 20), expand = FALSE) +
    theme_minimal(base_size = 14) +
    labs(x = "Market quantity Q", y = "Price", title = title) +
    # Phase label
    annotate("text", x = 0.4, y = 19.2, hjust = 0, vjust = 1, size = 3.2,
             label = phase_label) +
    geom_text(data=df, aes(x=11.8, y=0.8, label = paste0("t=", frame)), hjust=1, vjust=0, size=3, alpha=0.6) +
    transition_manual(frame)

  # In LR phase, show the initial supply line as a ghost reference
  if (is_lr) {
    p <- p +
      annotate("segment",
        x = 0, xend = 12,
        y = a0_init, yend = a0_init + b * 12,
        linewidth = 0.8, color = "#ff7f0e", alpha = 0.3, linetype = "dashed")
  }

  p
}

render_gif <- function(gganim, nframes, fps = 20, width = 800, height = 650) {
  animate(gganim, nframes = nframes, fps = fps, width = width, height = height,
          renderer = gifski_renderer())
}

# OPTIMIZED: batch append with lapply instead of growing vector
side_by_side <- function(gif_left, gif_right) {
  im1 <- image_read(gif_left)
  im2 <- image_read(gif_right)

  n <- length(im1)

  # If right panel has 1 frame (or fewer than left), repeat it to match n
  if (length(im2) < n) {
    im2 <- rep(im2, length.out = n)
  } else {
    im2 <- im2[1:n]
  }

  frames <- lapply(seq_len(n), function(i) {
    image_append(c(im1[i], im2[i]), stack = FALSE)
  })
  do.call(c, frames)
}


# ---------------------------
# 6) Master: make one scenario gif
#    OPTIMIZED: pause phase reuses last SR frames instead of re-rendering
# ---------------------------
make_scenario_gif <- function(scenario, outdir = "gifs", fps = 20,
                              n_pause = 20) {

  paths <- make_paths(scenario)

  pretty <- switch(
    scenario,
    demand_out     = "Demand shifts OUT",
    demand_in      = "Demand shifts IN",
    fixedcost_up   = "Fixed cost UP",
    fixedcost_down = "Fixed cost DOWN"
  )

  # Scenario-specific phase labels
  sr_label <- switch(
    scenario,
    demand_out     = "Short Run: Demand increases \u2192 Price rises \u2192 Firm earns profit",
    demand_in      = "Short Run: Demand decreases \u2192 Price falls \u2192 Firm incurs losses",
    fixedcost_up   = "Short Run: Fixed costs rise \u2192 ATC up \u2192 Firm incurs losses",
    fixedcost_down = "Short Run: Fixed costs fall \u2192 ATC down \u2192 Firm earns profit"
  )

  lr_label <- switch(
    scenario,
    demand_out     = "Long Run: Firms enter \u2192 Supply shifts right \u2192 Price falls to zero profit",
    demand_in      = "Long Run: Firms exit \u2192 Supply shifts left \u2192 Price rises to zero profit",
    fixedcost_up   = "Long Run: Firms exit \u2192 Supply shifts left \u2192 Price rises to zero profit",
    fixedcost_down = "Long Run: Firms enter \u2192 Supply shifts right \u2192 Price falls to zero profit"
  )

  # --- Short run phase gifs ---
  pF_sr <- plot_firm_phase(paths$sr, paste0(pretty, " \u2014 Firm"),   sr_label)
  pM_sr <- plot_market_phase(paths$sr, paste0(pretty, " \u2014 Market"), sr_label,
                              is_lr = FALSE)

  gF_sr <- render_gif(pF_sr, nframes = nrow(paths$sr), fps = fps)
  gM_sr <- render_gif(pM_sr, nframes = nrow(paths$sr), fps = fps)

  seg_sr <- side_by_side(gF_sr, gM_sr)

  # --- Pause frames: replicate last SR frame (no re-rendering!) ---
  last_frame <- seg_sr[length(seg_sr)]
  seg_pause <- rep(last_frame, n_pause)

  # --- Long run phase gifs ---
  # Supply was at a0 throughout SR; ghost line shows this starting position
  pF_lr <- plot_firm_phase(paths$lr, paste0(pretty, " \u2014 Firm"),   lr_label)
  pM_lr <- plot_market_phase(paths$lr, paste0(pretty, " \u2014 Market"), lr_label,
                              is_lr = TRUE, a0_init = a0)

  gF_lr <- render_gif(pF_lr, nframes = nrow(paths$lr), fps = fps)
  gM_lr <- render_gif(pM_lr, nframes = nrow(paths$lr), fps = fps)

  seg_lr <- side_by_side(gF_lr, gM_lr)

  # Concatenate SR + Pause + LR in time, set delay at write
  full <- c(seg_sr, seg_pause, seg_lr)

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  outfile <- normalizePath(file.path(outdir, paste0(scenario, "_2panel.gif")), mustWork = FALSE)
  print(outfile)

  image_write_gif(full, path = outfile, delay = 1 / fps)
  outfile
}

# ---------------------------
# 7) Run all four
# ---------------------------
files <- c(
  make_scenario_gif("demand_out"),
  make_scenario_gif("demand_in"),
  make_scenario_gif("fixedcost_up"),
  make_scenario_gif("fixedcost_down")
)

files
