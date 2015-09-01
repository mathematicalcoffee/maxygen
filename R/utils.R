
roxygen_function <- function(name) {
  get(name, envir = asNamespace("roxygen2"), inherits = FALSE)
}

trim <- function(x) {
  sub("\\s$", "", sub("^\\s*", "", x))
}

ws_to_empty <- function(x) {
  sub("^\\s*$", "", x)
}

parse_manual_link <- function(xml) {
  txt <- xml_text(xml)

  ## package name might be missing. If not missing it must start with a
  ## letter and must be at least two characters long
  reg <- regexpr(
    "^((?<p>[a-zA-Z][a-zA-Z0-9\\.]+)|)::(?<f>[a-zA-Z\\.][a-zA-Z\\.0-9]*)$",
    txt,
    perl = TRUE
  )
  if (reg != 1L) return(NULL)

  pkg <- if (attr(reg, "capture.start")[,"p"] == 0) {
    ""
  } else {
    substring(
      txt,
      attr(reg, "capture.start")[,"p"],
      attr(reg, "capture.start")[,"p"] + attr(reg, "capture.length")[,"p"] - 1
    )
  }

  func <- substring(
    txt,
    attr(reg, "capture.start")[,"f"],
    attr(reg, "capture.start")[,"f"] + attr(reg, "capture.length")[,"f"] - 1
  )

  list(pkg = pkg, func = func)
}

# Rather crude maths handling (regex).
#
# Inline maths is $expression$, and must be contained within one line.
# Display maths is $$expression$$ which must be on one line, OR a paragraph
# on its own, provided it begins and ends with '$$'.
# 
# Doesn't handle complex things like $f(x) = x \text{ for $x > 0$}$.
# Ideally you will pass this over to MathJaX or some proper parser (that
#  tokenises and collates properly).
# Can't be preceded or followed immediately by a letter (just to try stop things
#  like foo$bar$baz from being interpreted as maths... yes. kludgy.)
#
# For inline math (from Pandoc):
# > The opening $ must have a non-space character immediately to its right,
#   while the closing $ must have a non-space character immediately to its left,
#   and must not be followed immediately by a digit.
# (Can also escape with backslash)
#
# Inline maths becomes `\eqn{stuff}`, and display is `\deqn{stuff}`.
#
# Since the maths support is not in commonmark and it is possible for maths
#  expressions to be parsed with markdown (e.g. $a *b* c$ looks like the 'b' is emphasized),
#  we 'protect' them before passing to commonmark by putting them all in inline code
#  with $expr$ --> `inlineMath\rexpr` or `displayMath\rexpr` (hoping that this markup will never turn
#  up in normal usage) and then handle them in the code handler.
protect_maths <- function (text) {
    # grab display maths
    text <- gsub("(?<![\\w$])\\$\\$((?:[^$\\\\]+|\\.|\\$(?!\\$))+)\\$\\$(?![\\w$])",
                 "`displayMath\r\\1`",
                 perl=T,
                 text)
    # inline maths
    # grep("(?<!\\$)\\$(?=[^\\s$])(\\\\.|[^$\r\n]+)(?<=\\S)\\$(?!\\$)",
    #      c('$succeed$', '$ fail$', '$\tfail$', '$fail$$', '$$', '$$fail$','$\\$succeed$'),
    #      perl=T, value=T)
    text <- gsub("(?<![\\w$])\\$((?=[^\\s$])(\\\\.|[^\\\\$\r\n]+)+(?<=\\S))\\$(?![\\w$])",
                 "`inlineMath\r\\1`",
                 perl=T,
                 text)
    return(text)
}

# here `text` is the insides of an inline code block.
# if `inlineMath\r expr
replace_maths <- function (text) {
    o.text <- text
    text <- sub('^inlineMath\r(.+)$', '\\eqn{\\1}', text)
    if (text == o.text)
        text <- sub('^displayMath\r(.+)$', '\\deqn{\\1}', text)
    return(text)
}
