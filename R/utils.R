
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
# Doesn't handle complex things like $f(x) = x \u{007F}ext{ for $x > 0$}$.
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
#  with $expr$ --> `inlineMath\u{007F}expr` or `displayMath\u{007F}expr` (hoping that this markup will never turn
#  up in normal usage) and then handle them in the code handler.
protect_maths <- function (text) {
    # grab display maths. $$ {stuff} $$, can have internal dollar signs if escaped, or single internal dollar signs.
    text <- gsub("(?<![\\w$\\\\])\\$\\$((?:[^$\\\\]+|\\\\.|\\$(?!\\$))+)\\$\\$(?![\\w$])",
                 "`displayMath\u{007F}\\1`",
                 perl=T,
                 text)
    # inline maths
    text <- gsub("(?<![\\w$])\\$((?=[^\\s$])(?:\\\\.|[^\\\\$\u{007F}\n]+)+(?<=\\S))\\$(?![\\w$])",
                 "`inlineMath\u{007F}\\1`",
                 perl=T,
                 text)
    return(text)
}

# here `text` is the insides of an inline code block.
# if `inlineMath\u{007F} expr
replace_maths <- function (text) {
    text <- sub('^inlineMath\u{007F}(.+)$', '\\\\eqn{\\1}', text)
    text <- sub('^displayMath\u{007F}(.+)$', '\\\\deqn{\\1}', text)
    return(text)
}
