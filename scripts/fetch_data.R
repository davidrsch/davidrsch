suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

source("scripts/config.R")

# --------------------------------------------------------------------------- #
# Helpers                                                                      #
# --------------------------------------------------------------------------- #
`%||%` <- function(x, y) if (!is.null(x)) x else y

make_alias <- function(name) gsub("[^A-Za-z0-9_]", "_", name)

null_safe <- function(x, default) if (is.null(x) || length(x) == 0) default else x

# Helper to fetch an image and convert to base64
fetch_base64_image <- function(url) {
  if (is.null(url) || nchar(url) == 0) return("")
  
  is_svg <- grepl("\\.svg$", url, ignore.case = TRUE)
  mime <- if (is_svg) "image/svg+xml" else "image/png"
  
  tryCatch({
    resp <- request(url) |>
      req_timeout(10) |>
      req_perform()
    raw_data <- resp_body_raw(resp)
    b64 <- jsonlite::base64_enc(raw_data)
    sprintf("data:%s;base64,%s", mime, b64)
  }, error = function(e) {
    warning("Failed to fetch image: ", url, "\nError: ", conditionMessage(e))
    ""
  })
}

# Helper to fetch total contributor count using REST API
fetch_contributor_count <- function(owner, repo, token) {
  url <- sprintf("https://api.github.com/repos/%s/%s/contributors?per_page=100", owner, repo)
  tryCatch({
    resp <- request(url) |>
      req_headers(Authorization = paste("bearer", token)) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    
    if (resp_status(resp) == 200) {
      contribs <- resp_body_json(resp, simplifyVector = FALSE)
      length(contribs)
    } else {
      0L
    }
  }, error = function(e) {
    warning("Failed to fetch contributors for: ", repo)
    0L
  })
}

# --------------------------------------------------------------------------- #
# Build batched GraphQL query                                                  #
# --------------------------------------------------------------------------- #
all_repos <- do.call(c, lapply(GROUPS, function(g) g$repos))

fragment <- '
fragment RepoFields on Repository {
  name
  description
  stargazerCount
  forkCount
  pushedAt
  primaryLanguage { name color }
  languages(first: 10, orderBy: {field: SIZE, direction: DESC}) {
    totalSize
    edges { size node { name color } }
  }
  defaultBranchRef {
    target {
      ... on Commit {
        history(first: 100) {
          nodes { committedDate }
        }
      }
    }
  }
}'

repo_blocks <- sapply(all_repos, function(r) {
  sprintf('  %s: repository(owner: "%s", name: "%s") { ...RepoFields }',
          make_alias(r$name), OWNER, r$name)
})

user_block <- sprintf('
  user_stats: user(login: "%s") {
    name
    createdAt
    repositories(first: 100, ownerAffiliations: OWNER, isFork: false) {
      nodes {
        stargazerCount
        languages(first: 10, orderBy: {field: SIZE, direction: DESC}) {
          edges { size node { name color } }
        }
      }
    }
    contributionsCollection {
      totalCommitContributions
      totalIssueContributions
      totalPullRequestContributions
      totalRepositoriesWithContributedCommits
      contributionCalendar {
        totalContributions
        weeks {
          contributionDays {
            contributionCount
            date
          }
        }
      }
    }
  }
', OWNER)

query <- paste0(
  "{\n",
  user_block, "\n",
  paste(repo_blocks, collapse = "\n"),
  "\n}\n",
  fragment
)

# --------------------------------------------------------------------------- #
# Execute query                                                                #
# --------------------------------------------------------------------------- #
token <- Sys.getenv("GITHUB_TOKEN")
if (nchar(token) == 0) {
  tryCatch({
    token <- trimws(system("gh auth token", intern = TRUE))
  }, error = function(e) {
    stop("GITHUB_TOKEN environment variable is not set and gh cli failed.")
  })
}

cat("Fetching data from GitHub GraphQL API...\n")

resp <- request("https://api.github.com/graphql") |>
  req_headers(
    Authorization   = paste("bearer", token),
    "Content-Type"  = "application/json"
  ) |>
  req_body_json(list(query = query)) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()

raw <- resp_body_json(resp, simplifyVector = FALSE)

if (!is.null(raw$errors)) {
  cat("GraphQL errors:\n")
  print(raw$errors)
  stop("GraphQL query failed.")
}

gql_data <- raw$data

# --------------------------------------------------------------------------- #
# Process weekly commit buckets (12 weeks, oldest → newest)                   #
# --------------------------------------------------------------------------- #
compute_weekly <- function(nodes) {
  counts <- integer(12)
  if (length(nodes) == 0) return(counts)
  now <- as.numeric(Sys.time())
  window <- 12 * 7 * 86400
  for (n in nodes) {
    t <- as.numeric(as.POSIXct(n$committedDate,
                                format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    diff <- now - t
    if (diff < 0 || diff > window) next
    wk <- min(as.integer(diff %/% (7 * 86400)) + 1L, 12L)
    counts[wk] <- counts[wk] + 1L
  }
  rev(counts)
}

# --------------------------------------------------------------------------- #
# Process global user stats                                                    #
# --------------------------------------------------------------------------- #
u <- gql_data$user_stats
user_stats_processed <- list(
  name = u$name,
  createdAt = u$createdAt,
  total_stars = sum(sapply(u$repositories$nodes, function(x) null_safe(x$stargazerCount, 0))),
  total_commits = u$contributionsCollection$totalCommitContributions,
  total_issues = u$contributionsCollection$totalIssueContributions,
  total_prs = u$contributionsCollection$totalPullRequestContributions,
  total_repos_contributed = u$contributionsCollection$totalRepositoriesWithContributedCommits,
  contribution_calendar = u$contributionsCollection$contributionCalendar
)

# calculate total languages across ONLY the featured repos in config.R
lang_map <- new.env()
for (group in GROUPS) {
  for (repo_cfg in group$repos) {
    alias <- make_alias(repo_cfg$name)
    rd <- gql_data[[alias]]
    if (is.null(rd)) next
    
    edges <- rd$languages$edges %||% list()
    for (e in edges) {
      nm <- e$node$name
      sz <- e$size
      col <- null_safe(e$node$color, "#ccc")
      if (is.null(lang_map[[nm]])) {
        lang_map[[nm]] <- list(size = sz, color = col)
      } else {
        lang_map[[nm]]$size <- lang_map[[nm]]$size + sz
      }
    }
  }
}
lang_list <- lapply(ls(lang_map), function(nm) {
  list(name = nm, size = lang_map[[nm]]$size, color = lang_map[[nm]]$color)
})
# sort by size descending
lang_list <- lang_list[order(sapply(lang_list, function(x) x$size), decreasing = TRUE)]
user_stats_processed$languages <- lang_list

# --------------------------------------------------------------------------- #
# Build structured output                                                      #
# --------------------------------------------------------------------------- #
result <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  user_stats = user_stats_processed,
  groups = lapply(GROUPS, function(g) {
    cat("Processing group:", g$title, "\n")
    list(
      id    = g$id,
      title = g$title,
      desc  = g$desc,
      repos = lapply(g$repos, function(r) {
        cat("  -", r$name, "\n")
        alias <- make_alias(r$name)
        rd    <- gql_data[[alias]]

        logo_b64 <- fetch_base64_image(r$logo_url)
        contrib_count <- fetch_contributor_count(OWNER, r$name, token)

        if (is.null(rd)) {
          warning("No data for: ", r$name)
          return(list(
            name = r$name, display = r$display %||% r$name,
            desc = r$custom_desc %||% (r$desc %||% ""),
            logo_b64 = logo_b64, contributor_count = contrib_count,
            error = TRUE,
            stars = 0L, forks = 0L, pushed_at = "",
            primary_language = "Unknown", primary_color = "#ccc",
            languages = list(), weekly_commits = as.list(integer(12))
          ))
        }

        total_sz <- null_safe(rd$languages$totalSize, 1)
        langs <- lapply(rd$languages$edges, function(e) {
          list(
            name  = e$node$name,
            color = null_safe(e$node$color, "#ccc"),
            pct   = round(e$size / total_sz * 100, 1)
          )
        })

        nodes   <- rd$defaultBranchRef$target$history$nodes %||% list()
        weekly  <- compute_weekly(nodes)

        list(
          name             = r$name,
          display          = r$display %||% rd$name,
          desc             = r$custom_desc %||% (r$desc %||% null_safe(rd$description, "")),
          logo_b64         = logo_b64,
          contributor_count = contrib_count,
          stars            = null_safe(rd$stargazerCount, 0L),
          forks            = null_safe(rd$forkCount, 0L),
          pushed_at        = null_safe(rd$pushedAt, ""),
          primary_language = if (!is.null(rd$primaryLanguage)) rd$primaryLanguage$name else "Unknown",
          primary_color    = if (!is.null(rd$primaryLanguage)) null_safe(rd$primaryLanguage$color, "#ccc") else "#ccc",
          languages        = langs,
          weekly_commits   = as.list(weekly)
        )
      })
    )
  })
)

dir.create("assets", showWarnings = FALSE)
write_json(result, "scripts/data.json", auto_unbox = TRUE, pretty = TRUE)
cat("Data written to scripts/data.json\n")
