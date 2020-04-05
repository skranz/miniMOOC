copy.into.missing.fields = function(dest, source) {
  restore.point("copy.into.empty.fields")

  new.fields = setdiff(names(source), names(dest))
  dest[new.fields] = source[new.fields]
  dest
}

colored.html = function(txt, color="blue") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

parse.block.args = function (header, arg.str = NULL, add.type = TRUE, type = "", allow.unquoted.title = FALSE)
{
    restore.point("parse.block.args")
    if (is.null(arg.str)) {
        str = header
        str = str.trim(str.right.of(str, "#< "))
        type = str.left.of(str, " ")
        arg.str = str.right.of(str, " ")
    }
    if (allow.unquoted.title) {
        arg.str = str.trim(arg.str)
        first = substring(arg.str, 1, 1)
        is.list = (grepl(",", arg.str, fixed = TRUE) & grepl("=",
            arg.str, fixed = TRUE))
        is.quoted = first == "'" | first == "\""
        if (is.list & !is.quoted) {
            stop(paste0("If your ", type, " title contains the character \",\" and \"=\" you must quote it, like \"my title\", to distinguish it from a parameter list."))
        }
        if (!is.list & !is.quoted) {
            return(list(name = arg.str, type = type))
        }
    }
    code = paste0("alist(", arg.str, ")")
    li = try(eval(base::parse(text = code, srcfile = NULL)),
        silent = TRUE)
    if (is(li, "try-error")) {
        if (!allow.unquoted.title | (grepl(",", code, fixed = TRUE) &
            grepl(",", code, fixed = TRUE))) {
            stop("I cannot parse your block arguments ", arg.str,
                " as a list in R. Perhaps you have to add quotes around some arguments, like the title.")
        }
        li = list(name = arg.str)
    }
    else {
        li = lapply(li, function(el) {
            res = try(eval(el, envir = baseenv()), silent = TRUE)
            if (is(res, "try-error"))
                return(as.character(el))
            res
        })
    }
    if (add.type) {
        if (length(li) == 0)
            return(list(name = NULL, type = type))
        if (is.null(names(li))) {
            return(list(type = type, name = li[[1]]))
        }
        else if (nchar(names(li)[1]) == 0) {
            return(c(list(type = type, name = li[[1]]), li[-1]))
        }
    }
    else {
        if (length(li) == 0)
            return(list(name = NULL))
        if (is.null(names(li))) {
            return(list(name = li[[1]]))
        }
        else if (nchar(names(li)[1]) == 0) {
            return(c(list(name = li[[1]]), li[-1]))
        }
    }
    li
}
