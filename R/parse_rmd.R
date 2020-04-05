examples.parse_rmd = function() {
  setwd("D:/libraries/miniMOOC/example")
  file = "vq_ma_1a.Rmd"
  preview_mooc_rmd(file, lang="de", log.file="log.txt")
}

miniMOOCApp = function(mm=readRDS("mm.Rds"), log.file=NULL) {
  app = eventsApp()
  js = read.as.utf8(system.file("js/miniMOOC.js", package="miniMOOC")) %>%
    merge.lines()

  quiz.handler = NULL
  if (!is.null(log.file)) {
    quiz.handler = function(qu, part.ind=part.ind, part.correct=correct, solved, answer, ..., app=getApp()) {
      if (part.ind == 0) return()
      restore.point("jkhfhdf")
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
    mm$ui,
    tags$script(HTML(js))
  )
  appInitHandler(function(..., app=getApp()) {
    app$random_id = random.string()
  }, app=getApp())

  app
}



preview_mooc_rmd = function(file,log.file=NULL, ...) {
  #app = eventsApp()
  restore.point("preview_mooc_rmd")

  mm = parse_mooc_rmd(file,...)
  app = miniMOOCApp(mm, log.file=log.file)
  viewApp(app)
}

parse_mooc_rmd = function(file,chunks=c("knit","render","ignore")[2], youtube.width = 560, youtube.height=round((315/560)*youtube.width), lang="en") {
  restore.point("parse_mooc_rmd")

  rmd.txt = read.as.utf8(file)

  section.lines = which(startsWith(rmd.txt, "#. section"))
  if (length(section.lines)==0) {
    res = parse_mooc_section(rmd.txt, chunks=chunks, youtube.width = youtube.width, youtube.height = youtube.height, lang=lang)
    ui = res$ui
    quiz.li = res$quiz.li
  } else {
    txt.lines = c(section.lines+1, NROW(rmd.txt)+2)
    sec.li = lapply(seq_along(section.lines), function(i) {
      parse_mooc_section(rmd.txt[txt.lines[i]:(txt.lines[i+1]-2)],chunks=chunks, youtube.width = youtube.width, youtube.height = youtube.height, lang=lang)
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
        tabPanel(title=i,inner.ui)
     })
    ))

  }
  list(ui = ui, quiz.li = quiz.li)
}

parse_mooc_section = function(txt, chunks=c("knit","render","ignore")[2], youtube.width = 560, youtube.height=round((315/560)*youtube.width), lang="en") {
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
    id = rmdtools::parse.block.args(info$header,allow.unquoted.title = TRUE)$name
    if (is.null(id))
      stop("All your quizzes must have names!")
    quiz = shinyQuiz(id=id, yaml=info$inner.txt,add.handler = FALSE, lang=lang)
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
