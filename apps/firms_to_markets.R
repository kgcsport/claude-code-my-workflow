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
q <- seq(0.5, 12, length.out = 400)

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
# ---------------------------
make_paths <- function(scenario,
                       n_sr = 60, n_lr = 90,
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
  # Demand shock: A ramps, a fixed, F fixed
  # Fixed-cost shock: F ramps, A fixed, a fixed (market price unchanged SR)
  sr <- tibble(frame = 1:n_sr) %>%
    mutate(
      A = if (scenario %in% c("demand_out","demand_in")) seq(A0, A_end, length.out = n_sr) else A0,
      a = a0,
      F = if (scenario %in% c("fixedcost_up","fixedcost_down")) seq(F0, F_end, length.out = n_sr) else F0
    ) %>%
    rowwise() %>%
    mutate(
      p_be = p_be_from_F(F),
      Q_eq = eq_from_Aa(A, a)[["Q"]],
      P    = eq_from_Aa(A, a)[["P"]]
    ) %>%
    ungroup()

  # ---- Phase LR: entry/exit (supply shifts) + profit restoration simultaneously ----
  A_hold <- if (scenario %in% c("demand_out","demand_in")) A_end else A0
  F_hold <- if (scenario %in% c("fixedcost_up","fixedcost_down")) F_end else F0

  # Target price in LR:
  # - demand shocks: restore P to baseline break-even p_be0 (costs unchanged)
  # - fixed-cost shocks: restore P to NEW break-even p_be(F_end)
  p_target <- if (scenario %in% c("demand_out","demand_in")) p_be0 else p_be_from_F(F_hold)

  a_target <- a_for_price(p_target, A_hold)

  a_path <- numeric(n_lr)
  a_path[1] <- a0
  for (i in 2:n_lr) a_path[i] <- a_path[i-1] + lambda*(a_target - a_path[i-1])

  lr <- tibble(frame = 1:n_lr) %>%
    mutate(A = A_hold, F = F_hold, a = a_path) %>%
    rowwise() %>%
    mutate(
      p_be = p_be_from_F(F),
      Q_eq = eq_from_Aa(A, a)[["Q"]],
      P    = eq_from_Aa(A, a)[["P"]]
    ) %>%
    ungroup()

  list(sr = sr, lr = lr)
}

# ---------------------------
# 3) Firm curves + firm stats per frame
# ---------------------------
firm_curves_long <- function(df) {
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

firm_stats <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      q_star = if_else(P >= p_shutdown, q_star_from_P(P), 0),
      AFC_star = if_else(q_star > 0, F / q_star, NA_real_),
      AVC_star = if_else(q_star > 0, interp(q_star, q, AVC_base), NA_real_),
      ATC_star = if_else(q_star > 0, AVC_star + AFC_star, NA_real_),
      total_profit = if_else(q_star > 0, (P - ATC_star) * q_star, NA_real_),
      is_profit = if_else(q_star > 0 & P >= ATC_star, TRUE, FALSE),
      profit_color = if_else(is_profit, "#2ca02c", "#d62728"),
      xmin = 0, xmax = q_star,
      ymin = if_else(is_profit, ATC_star, P),
      ymax = if_else(is_profit, P, ATC_star)
    ) %>%
    ungroup()
}

# ---------------------------
# 4) Market lines per frame
# ---------------------------
industry_lines_long <- function(df) {
  Qgrid <- seq(0, 12, length.out = 240)
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

# Frame-by-frame horizontal append (this is your "working" approach)
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

  combo <- image_append(c(im1[1], im2[1]), stack = FALSE)
  if (n > 1) for (i in 2:n) combo <- c(combo, image_append(c(im1[i], im2[i]), stack = FALSE))
  combo
}


# ---------------------------
# 6) Master: make one scenario gif
# ---------------------------
make_scenario_gif <- function(scenario, outdir = "gifs", fps = 20,
                              n_pause = 30) {

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

  pause_label <- "Pause \u2014 observe short-run outcome"

  # --- Short run phase gifs ---
  pF_sr <- plot_firm_phase(paths$sr, paste0(pretty, " \u2014 Firm"),   sr_label)
  pM_sr <- plot_market_phase(paths$sr, paste0(pretty, " \u2014 Market"), sr_label,
                              is_lr = FALSE)

  gF_sr <- render_gif(pF_sr, nframes = nrow(paths$sr), fps = fps)
  gM_sr <- render_gif(pM_sr, nframes = nrow(paths$sr), fps = fps)

  seg_sr <- side_by_side(gF_sr, gM_sr)

  # --- Pause frames: repeat final SR state ---
  sr_last <- paths$sr %>% slice(n())
  pause_df <- bind_rows(replicate(n_pause, sr_last, simplify = FALSE)) %>%
    mutate(frame = 1:n_pause)

  pF_pause <- plot_firm_phase(pause_df, paste0(pretty, " \u2014 Firm"),   pause_label)
  pM_pause <- plot_market_phase(pause_df, paste0(pretty, " \u2014 Market"), pause_label,
                                 is_lr = FALSE)

  gF_pause <- render_gif(pF_pause, nframes = n_pause, fps = fps)
  gM_pause <- render_gif(pM_pause, nframes = n_pause, fps = fps)

  seg_pause <- side_by_side(gF_pause, gM_pause)

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
