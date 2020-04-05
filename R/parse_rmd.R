examples.parse_rmd = function() {
  setwd("D:/libraries/simplestMOOC/example")
  file = "vq_ma_1a.Rmd"

  app = eventsApp()
  app$ui = fluidPage(
    h4("Hello"),
    parse_mooc_rmd(file, youtube.width=800)
  )
  viewApp(app)
}

preview_mooc_rmd = function(file, ...) {
  app = eventsApp()
  app$ui = fluidPage(
    parse_mooc_rmd(file, ...)
  )
  viewApp(app)
}

parse_mooc_rmd = function(file, chunks=c("knit","render","ignore")[2], youtube.width = 560, youtube.height=round((315/560)*youtube.width)) {
  txt = read.as.utf8(file)
  blocks = rmdtools::find.rmd.nested(txt) %>%
    filter(form != "chunk")

  # Replace Youtube links
  lines = blocks$start[blocks$type=="youtube"]
  txt[lines] = sapply(txt[lines], youtube.hashdot.to.iframe,width=youtube.width, height=youtube.height)

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
    quiz = shinyQuiz(id=id, yaml=info$inner.txt)
  })
  ph$value[ph.inds] = lapply(quiz.li, function(quiz) quiz$ui)
  names(ph$value) = ph$id

  cr$ph = ph

  #undebug(render.compiled.rmd)
  ui = render.compiled.rmd(cr, out.type="shiny")
  ui

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
