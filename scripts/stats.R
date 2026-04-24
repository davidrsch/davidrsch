ICON_ISSUE <- "M8 1.5a6.5 6.5 0 1 0 0 13 6.5 6.5 0 0 0 0-13ZM0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8Zm9 3a1 1 0 1 1-2 0 1 1 0 0 1 2 0Zm-.25-6.25a.75.75 0 0 0-1.5 0v3.5a.75.75 0 0 0 1.5 0v-3.5Z"
ICON_COMMIT <- "M10.5 7.75a2.5 2.5 0 1 1-5 0 2.5 2.5 0 0 1 5 0Zm1.43.75a4.002 4.002 0 0 1-7.86 0H.75a.75.75 0 1 1 0-1.5h3.32a4.001 4.001 0 0 1 7.86 0h3.32a.75.75 0 1 1 0 1.5h-3.32Z"
ICON_REPO <- "M2 2.5A2.5 2.5 0 0 1 4.5 0h8.75a.75.75 0 0 1 .75.75v12.5a.75.75 0 0 1-.75.75h-2.5a.75.75 0 0 1 0-1.5h1.75v-2h-8a1 1 0 0 0-.714 1.7.75.75 0 1 1-1.072 1.05A2.495 2.495 0 0 1 2 11.5Zm10.5-1h-8a1 1 0 0 0-1 1v6.708A2.486 2.486 0 0 1 4.5 9h8ZM5 12.25a.25.25 0 0 1 .25-.25h3.5a.25.25 0 0 1 .25.25v3.25a.25.25 0 0 1-.4.2l-1.45-1.087a.249.249 0 0 0-.3 0L5.4 15.7a.25.25 0 0 1-.4-.2Z"
ICON_PR <- "M1.5 3.25a2.25 2.25 0 1 1 3 2.122v5.256a2.251 2.251 0 1 1-1.5 0V5.372A2.25 2.25 0 0 1 1.5 3.25Zm7.362-.054a.75.75 0 0 1-.225 1.036l-2.072 1.346a.75.75 0 1 1-.815-1.256l1.397-.907-.091-.059H6.25A1.75 1.75 0 0 0 4.5 5.045v4.327a2.25 2.25 0 1 1-1.5 0V5.045A3.25 3.25 0 0 1 6.25 1.81h.806l.091-.059-1.397-.907a.75.75 0 0 1 .815-1.256l2.072 1.346a.75.75 0 0 1 .225 1.036ZM4.5 10.75a.75.75 0 1 0-1.5 0 .75.75 0 0 0 1.5 0Z"

render_stats_general <- function(user_stats) {
  SVG_W <- 405
  h <- 140
  
  elements <- c(
    rect_el(0, 0, SVG_W, h, rx=8, class="card"),
    text_el(20, 24, "Global Statistics", class="clr-primary txt-bold", size=14),
    sprintf('<line x1="20" y1="36" x2="385" y2="36" class="divider"/>'),
    
    # Left Column
    icon_el(20, 52, ICON_STAR, "var(--yellow)"),
    text_el(45, 64, "Total Stars Earned:", class="clr-primary", size=12),
    text_el(180, 64, as.character(user_stats$total_stars), class="clr-primary txt-bold", size=13),
    
    icon_el(20, 84, ICON_COMMIT, "var(--green)"),
    text_el(45, 96, "Commits (Last Year):", class="clr-primary", size=12),
    text_el(180, 96, as.character(user_stats$total_commits), class="clr-primary txt-bold", size=13),
    
    icon_el(20, 116, ICON_PR, "var(--accent)"),
    text_el(45, 128, "Total PRs (Last Year):", class="clr-primary", size=12),
    text_el(180, 128, as.character(user_stats$total_prs), class="clr-primary txt-bold", size=13),
    
    # Right Column
    icon_el(215, 52, ICON_ISSUE, "var(--red)"),
    text_el(240, 64, "Total Issues:", class="clr-primary", size=12),
    text_el(355, 64, as.character(user_stats$total_issues), class="clr-primary txt-bold", size=13),
    
    icon_el(215, 84, ICON_REPO, "var(--secondary)"),
    text_el(240, 96, "Contributed To:", class="clr-primary", size=12),
    text_el(355, 96, as.character(user_stats$total_repos_contributed), class="clr-primary txt-bold", size=13)
  )
  
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n</svg>\n',
    SVG_W, h, SVG_W, h, SVG_CSS, paste(elements, collapse = "\n  ")
  )
}

render_stats_languages <- function(user_stats) {
  SVG_W <- 405
  h <- 140
  
  langs <- user_stats$languages
  if (length(langs) > 6) langs <- langs[1:6]
  
  total_size <- sum(sapply(langs, function(x) x$size))
  if (total_size == 0) total_size <- 1 # prevent div by zero
  
  elements <- c(
    rect_el(0, 0, SVG_W, h, rx=8, class="card"),
    text_el(20, 24, "Most Used Languages", class="clr-primary txt-bold", size=14)
  )
  
  # Horizontal Bar
  bar_x <- 20
  bar_w <- 365
  bar_y <- 40
  bar_h <- 8
  
  cur_x <- bar_x
  for (i in seq_along(langs)) {
    w <- max((langs[[i]]$size / total_size) * bar_w, 2)
    rx <- if (i == 1 || i == length(langs)) 4 else 0
    elements <- c(elements, sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="%s" rx="%d"/>', 
                  cur_x, bar_y, w, bar_h, langs[[i]]$color, rx))
    cur_x <- cur_x + w
  }
  
  # List
  lx <- 20
  ly <- 76
  for (i in seq_along(langs)) {
    if (i == 4) { lx <- 215; ly <- 76 }
    pct <- round((langs[[i]]$size / total_size) * 100, 1)
    
    elements <- c(elements, 
      sprintf('<circle cx="%g" cy="%g" r="4" fill="%s"/>', lx + 4, ly - 4, langs[[i]]$color),
      text_el(lx + 16, ly, langs[[i]]$name, class="clr-primary txt-bold", size=12),
      text_el(lx + 115, ly, sprintf("%.1f%%", pct), class="clr-muted", size=12)
    )
    ly <- ly + 22
  }
  
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n</svg>\n',
    SVG_W, h, SVG_W, h, SVG_CSS, paste(elements, collapse = "\n  ")
  )
}

render_stats_activity <- function(user_stats) {
  SVG_W <- 820
  h <- 180
  
  # Extract calendar data
  weeks <- user_stats$contribution_calendar$weeks
  days <- do.call(c, lapply(weeks, function(w) w$contributionDays))
  
  counts <- sapply(days, function(d) d$contributionCount)
  max_count <- max(counts, 1)
  
  # Smooth out 365 days into weekly averages or just plot all 365 points
  # Area chart dimensions
  chart_w <- 780
  chart_h <- 80
  chart_x <- 20
  chart_y <- 160
  
  dx <- chart_w / max(length(counts) - 1, 1)
  
  pts <- sprintf("%g,%g", chart_x, chart_y)
  for (i in seq_along(counts)) {
    x <- chart_x + (i - 1) * dx
    y <- chart_y - (counts[i] / max_count) * chart_h
    pts <- paste0(pts, sprintf(" %g,%g", x, y))
  }
  pts <- paste0(pts, sprintf(" %g,%g", chart_x + chart_w, chart_y))
  
  line_pts <- ""
  for (i in seq_along(counts)) {
    x <- chart_x + (i - 1) * dx
    y <- chart_y - (counts[i] / max_count) * chart_h
    line_pts <- paste0(line_pts, sprintf("%g,%g ", x, y))
  }
  
  # Calculate joined years
  joined_date <- as.Date(user_stats$createdAt)
  years_active <- round(as.numeric(difftime(Sys.Date(), joined_date, units="days")) / 365, 1)
  
  elements <- c(
    rect_el(0, 0, SVG_W, h, rx=8, class="card"),
    text_el(20, 24, "Contribution History & Activity", class="clr-primary txt-bold", size=14),
    sprintf('<line x1="20" y1="36" x2="800" y2="36" class="divider"/>'),
    
    text_el(20, 60, "Years Active:", class="clr-muted", size=12),
    text_el(100, 60, sprintf("%.1f", years_active), class="clr-primary txt-bold", size=12),
    
    text_el(160, 60, "Commits (Last Year):", class="clr-muted", size=12),
    text_el(290, 60, as.character(user_stats$total_commits), class="clr-primary txt-bold", size=12),
    
    text_el(360, 60, "Highest Daily:", class="clr-muted", size=12),
    text_el(450, 60, as.character(max_count), class="clr-primary txt-bold", size=12),
    
    # Area chart
    sprintf('<polygon points="%s" fill="var(--pill)" opacity="0.6"/>', pts),
    # Line
    sprintf('<polyline points="%s" fill="none" stroke="var(--accent)" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>', trimws(line_pts))
  )
  
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n</svg>\n',
    SVG_W, h, SVG_W, h, SVG_CSS, paste(elements, collapse = "\n  ")
  )
}
