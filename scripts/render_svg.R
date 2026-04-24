suppressPackageStartupMessages(library(jsonlite))

# --------------------------------------------------------------------------- #
# Helpers                                                                      #
# --------------------------------------------------------------------------- #
`%||%` <- function(x, y) if (!is.null(x)) x else y

xml_esc <- function(s) {
  s <- gsub("&",  "&amp;",  s, fixed = TRUE)
  s <- gsub("<",  "&lt;",   s, fixed = TRUE)
  s <- gsub(">",  "&gt;",   s, fixed = TRUE)
  s <- gsub('"',  "&quot;", s, fixed = TRUE)
  s
}

trunc_str <- function(s, chars = 48) {
  if (nchar(s) > chars) paste0(substr(s, 1, chars - 1), "\u2026") else s
}

# Advanced text wrapping for SVG
wrap_text <- function(txt, max_chars = 55, max_lines = 4) {
  if (is.null(txt) || nchar(txt) == 0) return(character(0))
  words <- strsplit(txt, "\\s+")[[1]]
  lines <- character()
  curr_line <- ""
  for (w in words) {
    if (nchar(curr_line) + nchar(w) + 1 <= max_chars) {
      curr_line <- if (nchar(curr_line) == 0) w else paste(curr_line, w)
    } else {
      lines <- c(lines, curr_line)
      curr_line <- w
      if (length(lines) >= max_lines) {
        lines[max_lines] <- paste0(substr(lines[max_lines], 1, max_chars - 3), "...")
        return(lines[1:max_lines])
      }
    }
  }
  if (nchar(curr_line) > 0 && length(lines) < max_lines) {
    lines <- c(lines, curr_line)
  }
  lines
}

days_since <- function(pushed_at) {
  if (nchar(pushed_at) == 0) return(NA_integer_)
  t <- as.POSIXct(pushed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  as.integer(difftime(Sys.time(), t, units = "days"))
}

pulse_color_class <- function(days) {
  if (is.na(days))    return("clr-muted")
  if (days <= 7)      return("clr-green")
  if (days <= 30)     return("clr-yellow")
  return("clr-red")
}

pulse_label <- function(days) {
  if (is.na(days)) return("Unknown")
  if (days == 0)   return("Today")
  if (days == 1)   return("Yesterday")
  if (days < 30)   return(paste0(days, "d ago"))
  if (days < 365)  return(paste0(days %/% 30, "mo ago"))
  return(paste0(days %/% 365, "yr ago"))
}

# --------------------------------------------------------------------------- #
# SVG primitives                                                               #
# --------------------------------------------------------------------------- #
rect_el <- function(x, y, w, h, rx = 0, class = NULL, fill = NULL, extra = "") {
  cls  <- if (!is.null(class)) sprintf(' class="%s"', class) else ""
  fil  <- if (!is.null(fill))  sprintf(' fill="%s"',  fill)  else ""
  sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="%g"%s%s%s/>',
          x, y, w, h, rx, cls, fil, if (nchar(extra)) paste0(" ", extra) else "")
}

text_el <- function(x, y, txt, class = NULL, anchor = "start", size = NULL, extra = "") {
  cls  <- if (!is.null(class)) sprintf(' class="%s"', class) else ""
  anc  <- sprintf(' text-anchor="%s"', anchor)
  sz   <- if (!is.null(size)) sprintf(' font-size="%g"', size) else ""
  sprintf('<text x="%g" y="%g"%s%s%s%s>%s</text>',
          x, y, cls, anc, sz, if (nchar(extra)) paste0(" ", extra) else "",
          xml_esc(txt))
}

image_el <- function(x, y, w, h, b64, extra = "") {
  if (is.null(b64) || nchar(b64) == 0) return("")
  sprintf('<image x="%g" y="%g" width="%g" height="%g" href="%s" preserveAspectRatio="xMidYMid slice"%s/>',
          x, y, w, h, b64, if(nchar(extra)) paste0(" ", extra) else "")
}

icon_el <- function(x, y, d, fill = "var(--secondary)") {
  sprintf('<svg x="%g" y="%g" width="16" height="16" viewBox="0 0 16 16"><path fill="%s" d="%s"/></svg>', x, y, fill, d)
}

ICON_STAR <- "M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.75.75 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25Zm0 2.445L6.615 5.5a.75.75 0 0 1-.564.41l-3.097.45 2.24 2.184a.75.75 0 0 1 .216.664l-.528 3.084 2.769-1.456a.75.75 0 0 1 .698 0l2.77 1.456-.53-3.084a.75.75 0 0 1 .216-.664l2.24-2.183-3.096-.45a.75.75 0 0 1-.564-.41L8 2.694Z"
ICON_FORK <- "M5 5.372v.878c0 .414.336.75.75.75h4.5a.75.75 0 0 0 .75-.75v-.878a2.25 2.25 0 1 1 1.5 0v.878a2.25 2.25 0 0 1-2.25 2.25h-1.5v2.128a2.251 2.251 0 1 1-1.5 0V8.5h-1.5A2.25 2.25 0 0 1 3.5 6.25v-.878a2.25 2.25 0 1 1 1.5 0ZM5 3.25a.75.75 0 1 0-1.5 0 .75.75 0 0 0 1.5 0Zm6.75.75a.75.75 0 1 0 0-1.5.75.75 0 0 0 0 1.5Zm-3 8.5a.75.75 0 1 0-1.5 0 .75.75 0 0 0 1.5 0Z"
ICON_PEOPLE <- "M2 5.5a3.5 3.5 0 1 1 5.898 2.549 5.508 5.508 0 0 1 3.034 4.084.75.75 0 1 1-1.482.235 4 4 0 0 0-7.9 0 .75.75 0 0 1-1.482-.236A5.507 5.507 0 0 1 3.102 8.05 3.493 3.493 0 0 1 2 5.5ZM11 4a3.001 3.001 0 0 1 2.22 5.018 5.01 5.01 0 0 1 2.56 3.012.749.749 0 0 1-.885.954.752.752 0 0 1-.549-.514 3.507 3.507 0 0 0-2.522-2.372.75.75 0 0 1-.574-.73v-.352a.75.75 0 0 1 .416-.672A1.5 1.5 0 0 0 11 5.5.75.75 0 0 1 11 4Zm-5.5-.5a2 2 0 1 0-.001 3.999A2 2 0 0 0 5.5 3.5Z"

# --------------------------------------------------------------------------- #
# Sparkline                                                                    #
# --------------------------------------------------------------------------- #
sparkline <- function(counts, x0, y0, w, h, class = "spark-line") {
  counts <- as.numeric(counts)
  n      <- length(counts)
  mx     <- max(counts, 1)
  pts <- vapply(seq_len(n), function(i) {
    px <- x0 + (i - 1) / (n - 1) * w
    py <- y0 + h - (counts[i] / mx) * h
    sprintf("%g,%g", px, py)
  }, character(1))
  sprintf('<polyline class="%s" points="%s" fill="none" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>',
          class, paste(pts, collapse = " "))
}

# --------------------------------------------------------------------------- #
# Standard repo card (405 x 195)                                              #
# --------------------------------------------------------------------------- #
CARD_W  <- 405
CARD_H  <- 195
CARD_RX <- 8
PAD     <- 18
SPARK_W <- 85
SPARK_H <- 32

repo_card_inner <- function(repo, cx, cy) {
  d     <- days_since(repo$pushed_at)
  pcc   <- pulse_color_class(d)
  plbl  <- pulse_label(d)

  spark_x <- cx + CARD_W - PAD - SPARK_W
  spark_y <- cy + CARD_H - PAD - SPARK_H - 10

  lines <- c(
    # Background
    rect_el(cx, cy, CARD_W, CARD_H, rx = CARD_RX, class = "card")
  )
  
  # Logo (top right)
  has_logo <- !is.null(repo$logo_b64) && nchar(repo$logo_b64) > 0
  if (has_logo) {
    lines <- c(lines, image_el(cx + CARD_W - PAD - 36, cy + PAD, 36, 36, repo$logo_b64, 'clip-path="url(#logo-clip)"'))
  }

  # Title
  title_w <- if(has_logo) 28 else 40
  lines <- c(lines, text_el(cx + PAD, cy + 32, trunc_str(repo$display, title_w), class = "clr-primary txt-bold", size = 15))

  # Description (Wrapped)
  desc_lines <- wrap_text(repo$desc, max_chars = if(has_logo) 48 else 58, max_lines = 4)
  dy <- 55
  for (l in desc_lines) {
    lines <- c(lines, text_el(cx + PAD, cy + dy, l, class = "clr-secondary", size = 13))
    dy <- dy + 18
  }

  # Sparkline
  lines <- c(lines,
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="4" class="spark-bg"/>', spark_x - 4, spark_y - 4, SPARK_W + 8, SPARK_H + 8),
    sparkline(repo$weekly_commits, spark_x, spark_y, SPARK_W, SPARK_H)
  )

  # Stats row (Bottom)
  stat_y <- cy + CARD_H - PAD + 2
  lines <- c(lines,
    # Lang indicator
    sprintf('<circle cx="%g" cy="%g" r="4" fill="%s"/>', cx + PAD + 4, stat_y - 4, xml_esc(repo$primary_color)),
    text_el(cx + PAD + 12, stat_y, repo$primary_language, class = "clr-muted", size = 12),
    
    # Contributors
    icon_el(cx + PAD + 65, stat_y - 12, ICON_PEOPLE, "var(--muted)"),
    text_el(cx + PAD + 85, stat_y, as.character(repo$contributor_count), class = "clr-secondary", size = 12),

    # Stars
    icon_el(cx + PAD + 115, stat_y - 12, ICON_STAR, "var(--yellow)"),
    text_el(cx + PAD + 135, stat_y, format(repo$stars, big.mark = ","), class = "clr-secondary", size = 12),
    
    # Forks
    icon_el(cx + PAD + 165, stat_y - 12, ICON_FORK, "var(--secondary)"),
    text_el(cx + PAD + 185, stat_y, format(repo$forks, big.mark = ","), class = "clr-secondary", size = 12),
    
    # Pulse
    sprintf('<circle cx="%g" cy="%g" r="4" class="%s"/>', cx + PAD + 215, stat_y - 4, pcc),
    text_el(cx + PAD + 224, stat_y, plbl, class = "clr-muted", size = 11)
  )
  
  paste(lines, collapse = "\n  ")
}

# --------------------------------------------------------------------------- #
# Translation table row                                                       #
# --------------------------------------------------------------------------- #
TRANS_ROW_H <- 48
TRANS_W     <- 860

render_translation_card <- function(repo) {
  SVG_W <- 860
  h <- 48
  ry <- 0

  d    <- days_since(repo$pushed_at)
  pcc  <- pulse_color_class(d)
  plbl <- pulse_label(d)

  elements <- c(
    rect_el(0, 0, SVG_W, h, rx=6, class="card"),
    
    # Icon (Render invisible placeholder if missing, so layout holds)
    if (!is.null(repo$logo_b64) && nchar(repo$logo_b64) > 0) {
      image_el(20, ry + 8, 24, 32, repo$logo_b64)
    } else {
      "" # empty if no logo
    },
    
    # Title
    text_el(60, ry + 28, trunc_str(repo$display, 45), class = "clr-primary txt-bold", size = 13),
    
    # Contributors
    icon_el(450, ry + 16, ICON_PEOPLE, "var(--muted)"),
    text_el(470, ry + 28, as.character(repo$contributor_count), class = "clr-secondary", size = 12),

    # Stars
    icon_el(520, ry + 16, ICON_STAR, "var(--yellow)"),
    text_el(540, ry + 28, as.character(repo$stars), class = "clr-secondary", size = 12),
    
    # Forks
    icon_el(590, ry + 16, ICON_FORK, "var(--secondary)"),
    text_el(610, ry + 28, as.character(repo$forks), class = "clr-secondary", size = 12),
    
    # Pulse
    sprintf('<circle cx="%g" cy="%g" r="4" class="%s"/>', 660, ry + 24, pcc),
    text_el(670, ry + 28, plbl, class = "clr-muted", size = 12),
    
    # Sparkline
    sparkline(repo$weekly_commits, 740, ry + 12, 80, 24)
  )

  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n</svg>\n',
    SVG_W, h, SVG_W, h, SVG_CSS, paste(elements, collapse = "\n  ")
  )
}

render_translation_header <- function() {
  SVG_W <- 860
  h <- 32
  
  elements <- c(
    text_el(20,  22, "Icon",       class = "clr-muted txt-bold", size = 12),
    text_el(60,  22, "Book Translation", class = "clr-muted txt-bold", size = 12),
    icon_el(450, 10, ICON_PEOPLE, "var(--muted)"),
    icon_el(520, 10, ICON_STAR, "var(--yellow)"),
    icon_el(590, 10, ICON_FORK, "var(--secondary)"),
    text_el(660, 22, "Updated",    class = "clr-muted txt-bold", size = 12),
    text_el(740, 22, "12w commits",class = "clr-muted txt-bold", size = 12)
  )
  
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n</svg>\n',
    SVG_W, h, SVG_W, h, SVG_CSS, paste(elements, collapse = "\n  ")
  )
}

# --------------------------------------------------------------------------- #
# CSS                                                                          #
# --------------------------------------------------------------------------- #
SVG_CSS <- '
<style>
  :root {
    --bg:        #f6f8fa;
    --card:      #ffffff;
    --border:    #d0d7de;
    --primary:   #1f2328;
    --secondary: #636c76;
    --muted:     #8c959f;
    --accent:    #0969da;
    --green:     #1a7f37;
    --yellow:    #9a6700;
    --red:       #cf222e;
    --hover:     #f3f4f6;
    --spark-bg:  #f0f3f6;
    --spark:     #0969da;
    --pill:      #eaf1fb;
    --pill-txt:  #0969da;
    --div:       #d0d7de;
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --bg:        #0d1117;
      --card:      #161b22;
      --border:    #30363d;
      --primary:   #e6edf3;
      --secondary: #848d97;
      --muted:     #7d8590;
      --accent:    #2f81f7;
      --green:     #3fb950;
      --yellow:    #d29922;
      --red:       #f85149;
      --hover:     #21262d;
      --spark-bg:  #1c2128;
      --spark:     #58a6ff;
      --pill:      #1c2d3f;
      --pill-txt:  #58a6ff;
      --div:       #30363d;
    }
  }
  
  .bg          { fill: var(--bg); }
  .card        { fill: var(--card); stroke: var(--border); stroke-width: 1; }
  .lang-pill   { fill: var(--pill); }
  .spark-bg    { fill: var(--spark-bg); }
  .spark-line  { stroke: var(--spark); }
  .divider     { stroke: var(--div); stroke-width: 1; }
  .clr-primary   { fill: var(--primary); }
  .clr-secondary { fill: var(--secondary); }
  .clr-muted     { fill: var(--muted); }
  .clr-accent    { fill: var(--accent); }
  .clr-green     { fill: var(--green); }
  .clr-yellow    { fill: var(--yellow); }
  .clr-red       { fill: var(--red); }
  .txt-bold    { font-weight: 600; }
  svg text     { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif; }
</style>
'

# --------------------------------------------------------------------------- #
# Main Render functions                                                        #
# --------------------------------------------------------------------------- #

render_single_card <- function(repo) {
  content <- repo_card_inner(repo, 0, 0)
  
  defs <- '
  <defs>
    <clipPath id="logo-clip">
      <rect x="0" y="0" width="100%" height="100%" rx="6"/>
    </clipPath>
  </defs>
  '
  
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">\n%s\n%s\n%s\n</svg>\n',
    CARD_W, CARD_H, CARD_W, CARD_H, defs, SVG_CSS, content
  )
}



source("scripts/stats.R")

# --------------------------------------------------------------------------- #
# Execute                                                                      #
# --------------------------------------------------------------------------- #
cat("Reading data.json...\n")
data <- fromJSON("scripts/data.json", simplifyVector = FALSE)

dir.create("assets", showWarnings = FALSE)

cat("Generating Global Stats SVGs...\n")
if (!is.null(data$user_stats)) {
  writeLines(render_stats_general(data$user_stats), "assets/stats_general.svg", useBytes = TRUE)
  writeLines(render_stats_languages(data$user_stats), "assets/stats_languages.svg", useBytes = TRUE)
  writeLines(render_stats_activity(data$user_stats), "assets/stats_activity.svg", useBytes = TRUE)
  cat("Wrote assets/stats_*.svg\n")
}

for (g in data$groups) {
  if (g$id == "translations") {
    svg_hdr <- render_translation_header()
    writeLines(svg_hdr, "assets/translations_header.svg", useBytes = TRUE)
    cat("Wrote assets/translations_header.svg\n")
    
    for (repo in g$repos) {
      svg <- render_translation_card(repo)
      filename <- sprintf("assets/trans_%s.svg", repo$name)
      writeLines(svg, filename, useBytes = TRUE)
      cat(sprintf("Wrote %s\n", filename))
    }
  } else {
    for (repo in g$repos) {
      svg <- render_single_card(repo)
      filename <- sprintf("assets/%s.svg", repo$name)
      writeLines(svg, filename, useBytes = TRUE)
      cat(sprintf("Wrote %s\n", filename))
    }
  }
}

cat("Finished generating all SVGs.\n")
