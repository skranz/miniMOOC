examples.parse_rmd = function() {
  setwd("D:/libraries/miniMOOC/example")
  file = "vq_ma_1a.Rmd"
  preview_mooc_rmd(file, lang="de", log.file="log.txt", window.title="Market Analysis 1a")
}

#' Create a miniMOOC shinyEvents app
#'
#' @param mm A compiled miniMOOC object created with \code{parse_mooc_rmd}. You can save the result of \code{parse_mooc_rmd} with \code{saveRDS} and load it with \code{readRDS}.
#' @param log.file an optional log file in which answers to multiple choice quizzes are anomyously logged in csv format.
#' @param window.title title of browser window
#' @param title Optional title shown above the sections tabSet.
miniMOOCApp = function(mm=readRDS("mm.Rds"), log.file=NULL,title=NULL, window.title=title) {
  app = eventsApp()
  js = read.as.utf8(system.file("js/miniMOOC.js", package="miniMOOC")) %>%
    merge.lines()

  quiz.handler = NULL
  if (!is.null(log.file)) {
    quiz.handler = function(qu, part.ind=part.ind, part.correct=correct, solved, answer, ..., app=getApp()) {
      if (part.ind == 0) return()
      restore.point("jkhfhdf")
      if (is.null(answer)) return()
      line = as.data.frame(list(time=Sys.time(),userid=app$random_id, quiz=qu$id, part=part.ind, correct=part.correct, answer=as.character(answer)))
      if (file.exists(log.file)) {
        try(write.table(line, log.file,sep = ";", append=TRUE,quote = TRUE,row.names = FALSE,col.names = FALSE))
      } else {
        try(write.table(line, log.file,sep = ";", quote = TRUE,row.names = FALSE,col.names = TRUE))
      }


    }
  }

  for (qu in mm$quiz.li) {
    add.quiz.handlers(qu,quiz.handler = quiz.handler)
  }

  app$ui = fluidPage(
    if (!is.null(window.title))
      tags$head(tags$title(window.title)),
    if (!is.null(title))
      h3(title),
    mm$ui,
    tags$script(HTML(js))
  )
  appInitHandler(function(..., app=getApp()) {
    app$random_id = random.string()
  }, app=getApp())

  app
}


#' Preview an Rmd file as miniMOOC page
#'
#' You can essentially pass all arguments you pass to miniMOOC app and parse_mooc_rmd.
preview_mooc_rmd = function(file,log.file=NULL,title=NULL,window.title=title, ...) {
  #app = eventsApp()
  restore.point("preview_mooc_rmd")

  mm = parse_mooc_rmd(file,...)

  app = miniMOOCApp(mm, log.file=log.file, title=title, window.title=window.title)
  viewApp(app)
}


#' Parse and compile an Rmd
#'
#' @param file Rmd file path.
#' @param chunks what shall be done with R code chunks. By default "knit", the other option "render" allows some alternative behavior not yet well documented.
#' @param youtube.width default width of included youtube iframes. By default 560 you can also set 720.
#' @param youtube.height default height of included youtube iframes. By default keeps Google's proposed aspect ratio to width.
#' @param lang default language for quiz messages. Currently only "en" english and "de" German supported.
#' @param allow.zero.correct Do you want to alllow quizzes without any correct answer? (Could be used as a questionaire replacement.) By default FALSE
parse_mooc_rmd = function(file,chunks=c("knit","render","ignore")[1], youtube.width = 560, youtube.height=round((315/560)*youtube.width), lang="en", left.margin=1, right.margin = left.margin,with.mathjax=TRUE, allow.zero.correct=FALSE) {
  restore.point("parse_mooc_rmd")

  rmd.txt = read.as.utf8(file)

  section.lines = which(startsWith(rmd.txt, "#. section"))
  if (length(section.lines)==0) {
    res = parse_mooc_section(rmd.txt, chunks=chunks, youtube.width = youtube.width, youtube.height = youtube.height, lang=lang, allow.zero.correct=allow.zero.correct)
    ui = res$ui
    ui = fluidRow(column(width = 12-left.margin-right.margin, offset=left.margin, inner.ui))
    #if (with.mathjax)
    #  ui = shiny::withMathJax(ui)
    quiz.li = res$quiz.li
  } else {
    txt.lines = c(section.lines+1, NROW(rmd.txt)+2)
    sec.li = lapply(seq_along(section.lines), function(i) {
      parse_mooc_section(rmd.txt[txt.lines[i]:(txt.lines[i+1]-2)],chunks=chunks, youtube.width = youtube.width, youtube.height = youtube.height, lang=lang, allow.zero.correct=allow.zero.correct)
    })
    #ui.li = lapply(sec.li, function(sec) sec$ui)
    quiz.li = do.call(c, lapply(sec.li, function(sec) sec$quiz.li))

    ui = do.call("tabsetPanel",  c(list(id="sectionTabset"),
      lapply(seq_along(sec.li), function(i) {
        inner.ui = sec.li[[i]]$ui
        if (i < length(sec.li)) {
          inner.ui = tagList(inner.ui,
            hr(),
            simpleButton(id=paste0("nextBtn-",i),label="Continue", class.add="nextBtn")
          )
        }
        inner.ui = fluidRow(column(width = 12-left.margin-right.margin, offset=left.margin, inner.ui))
        #if (with.mathjax)
        #  inner.ui = shiny::withMathJax(inner.ui)
        tabPanel(title=i,inner.ui)
     })
    ))

  }

  # check if quiz has duplicated id
  quiz.ids = unlist(lapply(quiz.li, function(qu) qu$id))

  if (length(quiz.ids)>0) {
    dupl = quiz.ids[duplicated(quiz.ids)]
    if (length(dupl)>0) {
      stop("You specified more than once a quiz with name: ", dupl)
    }
  }

  if (with.mathjax)
    ui = shiny::withMathJax(ui)
  list(ui = ui, quiz.li = quiz.li)
}

parse_mooc_section = function(txt, chunks=c("knit","render","ignore")[2], youtube.width = 560, youtube.height=round((315/560)*youtube.width), lang="en", allow.zero.correct=FALSE) {
  restore.point("parse_mooc_section")

  blocks = rmdtools::find.rmd.nested(txt) %>%
    filter(form != "chunk")

  # Replace Youtube links
  lines = blocks$start[blocks$type=="youtube"]
  if (length(lines)>0) {
    txt[lines] = unlist(lapply(txt[lines], youtube.hashdot.to.iframe,width=youtube.width, height=youtube.height))
  }
  txt = remove.ignore.blocks.from.txt(txt, blocks)

  cr = rmdtools::compile.rmd(text=txt, blocks="ph",chunks = chunks)

  # Set placeholder values
  ph = cr$ph
  ph$value = ""

  ph.inds = which(ph$type=="quiz")
  quiz.li = lapply(ph.inds, function(ph.ind) {
    info = ph$info[[ph.ind]]
    arg.str = trimws(str.right.of(info$header,"quiz"))
    id = rmdtools::parse.block.args(arg.str=arg.str,allow.unquoted.title = TRUE)$name
    if (is.true(nchar(id)==0) | is.na(id)) {

      stop(paste0("You forgot to add a name for your quiz:\n\n", ph$txt[[ph.ind]],"\n\nStart the block like\n\n #< quiz \"myquiz\""))
    }
    quiz = shinyQuiz(id=id, yaml=info$inner.txt,add.handler = FALSE, lang=lang, allow.zero.correct=allow.zero.correct)
  })
  ph$value[ph.inds] = lapply(quiz.li, function(quiz) quiz$ui)
  names(ph$value) = ph$id

  cr$ph = ph

  #undebug(render.compiled.rmd)
  ui = render.compiled.rmd(cr, out.type="shiny")

  return(list(ui=ui,cr=cr, quiz.li=quiz.li))

}



remove.ignore.blocks.from.txt = function(txt, blocks) {
  # Remove ignore blocks
  ignore = filter(blocks, type=="ignore")
  if (NROW(ignore)>0) {
    ignore.line = stringtools::pos.to.ignore(as.matrix(ignore[,1:2]), end=NROW(txt))
    txt = txt[!ignore.line]
  }
  txt
}

#' Converts the short tag for a youtube video
#'
#' #. youtube id = "youtubeid"
#'
#' To proper HTML code that embeds the iframe with given width and height in pixels.
youtube.hashdot.to.iframe = function(str, width=560, height=round(width*0.5625)) {
  restore.point("youtube.hashdot.to.iframe")
  arg.str = str.right.of(str, "youtube ")
  args = parse.block.args(arg.str=arg.str)

  id = args$id
  if (is.null(id)) {
    stop("No youtube video id provided!")
  }
  html = paste0('<iframe width="', width,'" height="',height,'" src="https://www.youtube.com/embed/', id,'" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
')
  html
}
