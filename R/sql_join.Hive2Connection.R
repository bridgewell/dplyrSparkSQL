# The following functions are borrowed from https://github.com/piersharding/dplyrimpaladb

common_by <- function(by = NULL, x, y) {
  if (is.list(by)) return(by)

  if (!is.null(by)) {
    x <- names(by) %||% by
    y <- unname(by)

    # If x partially named, assume unnamed are the same in both tables
    x[x == ""] <- y[x == ""]

    return(list(x = x, y = y))
  }

  by <- intersect(tbl_vars(x), tbl_vars(y))
  if (length(by) == 0) {
    stop("No common variables. Please specify `by` param.", call. = FALSE)
  }
  message("Joining by: ", capture.output(dput(by)))

  list(
    x = by,
    y = by
  )
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

unique_name <- local({
  i <- 0

  function() {
    i <<- i + 1
    paste0("_W", i)
  }
})

unique_names <- function(x_names, y_names, by, x_suffix = ".x", y_suffix = ".y") {
  common <- setdiff(intersect(x_names, y_names), by)
  if (length(common) == 0) return(NULL)

  x_match <- match(common, x_names)
  x_new <- x_names
  x_new[x_match] <- paste0(x_names[x_match], x_suffix)

  y_match <- match(common, y_names)
  y_new <- y_names
  y_new[y_match] <- paste0(y_names[y_match], y_suffix)

  list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
}

double_escape <- function(x) {
  structure(x, class = c("sql", "sql", "character"))
}

names_to_as <- function(x, con = NULL) {
  names <- names2(x)
  as <- ifelse(names == '', '', paste0(' AS ', sql_escape_ident(con, names)))

  paste0(x, as)
}

sql_vector <- function(x, parens = NA, collapse = " ", con = NULL) {
  if (is.na(parens)) {
    parens <- length(x) > 1L
  }

  x <- names_to_as(x, con = con)
  x <- paste(x, collapse = collapse)
  if (parens) x <- paste0("(", x, ")")
  sql(x)
}

#'@references \url{https://github.com/wush978/dplyrimpaladb/blob/dplyrSparkSQL-reference/R/src-impaladb.r#L323}
#'@export
sql_join.Hive2Connection <- function(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
    left = sql("LEFT"),
    inner = sql("INNER"),
    right = sql("RIGHT"),
    full = sql("FULL"),
    stop("Unknown join type:", type, call. = FALSE)
  )
  by <- common_by(by, x, y)
#   using <- all(by$x == by$y)

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select)
  uniques <- unique_names(x_names, y_names, by$x[by$x == by$y])

  if (is.null(uniques)) {
    sel_vars <- c(x_names, y_names)
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])

    sel_vars <- unique(c(uniques$x, uniques$y))
  }

  left <- unique_name()
  right <- unique_name()
  on <- sql_vector(paste0(
      paste(sql_quote(left, "`"), sql_quote(by$x, "`"), sep="."),
      " = ",
      paste(sql_quote(right, "`"), sql_quote(by$y, "`"), sep=".")),
    collapse = " AND ", parens = TRUE)
  cond <- build_sql("ON ", on, con = con)
  cols <- unique(c(x_names, y_names))
  col_names <- lapply(cols, function (col) {if (length(grep(col, x=x_names)) >= 1) { left } else if (length(grep(col, x=y_names)) >= 1) { right } else NULL})

  # set the alias attribute for result columns
  pieces <- mapply(function(x, alias) {
      return(paste(sql_quote(alias, "`"), sql_quote(x, "`"), sep="."))
    }, as.character(cols), col_names)
  fields <- do.call(function(...) {paste(..., sep=", ")}, as.list(pieces))

  # double escape so that list of fields do not get quoted
  fields <- double_escape(fields)

  from <- build_sql(
    'SELECT ', fields, ' FROM ',
    sql_subquery(con, x$query$sql, left), "\n\n",
    join, " JOIN \n\n" ,
    sql_subquery(con, y$query$sql, right), "\n\n",
    cond, con = con
  )
  attr(from, "vars") <- lapply(sel_vars, as.name)
  from
}
