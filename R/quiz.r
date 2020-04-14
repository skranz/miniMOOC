
examples.quiz = function() {
    yaml = '
parts:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = shinyQuiz(id="myquiz", yaml=yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)

  runEventsApp(app, launch.browser=rstudioapi::viewer)

}

quizDefaults = function(lang="en") {
  if (lang=="de") {
    list(
      success = "Richtig!",
      failure= "Leider noch nicht richtig. Versuchen Sie es nochmal.",
      success_color = "black",
      failure_color = "red",
      points_txt = "Punkte",
      point_txt = "Punkt"
    )
  } else {
    list(
      success = "Great, you answered correctly!",
      failure= "Sorry, not yet correct. Try again.",
      success_color = "black",
      failure_color = "red",
      points_txt = "points",
      point_txt = "point"
    )
  }
}

# Create a shiny quiz widget
#
# @param id the id of the quiz
# @param qu a list that contains the quiz fields as would have
#        been parsed by read.yaml from package YamlObjects
# @param yaml alternatively to qu, is yaml a string that specifies the quiz
# @param quiz.handler a function that will be called if the quiz is checked.
#        The boolean argument solved is TRUE if the quiz was solved
#        and otherwise FALSE
shinyQuiz = function(id=paste0("quiz_",sample.int(10e10,1)),qu=NULL, yaml,  quiz.handler=NULL, add.handler=FALSE, single.check.btn=TRUE, defaults=quizDefaults(lang=lang), lang="en", allow.zero.correct=FALSE) {
  restore.point("shinyQuiz")

  if (is.null(qu)) {
    yaml = enc2utf8(yaml)
    qu = try(mark_utf8(read.yaml(text=yaml)), silent=TRUE)
    if (is(qu,"try-error")) {
      err = paste0("When importing quiz:\n",paste0(yaml, collapse="\n"),"\n\n",as.character(qu))
      stop(err,call. = FALSE)
    }
  }

  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }


  qu$single.check.btn = single.check.btn
  if (qu$single.check.btn) {
     qu$checkBtnId = paste0(qu$id,"__checkBtn")
  }

  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu, defaults=defaults, allow.zero.correct=allow.zero.correct))
  np = length(qu$parts)

  qu$max.points = sum(sapply(qu$parts, function(part) part[["points"]]))

  qu$state = as.environment(list(part.solved=rep(FALSE,np), solved=FALSE))

  qu$ui = quiz.ui(qu)

  if (add.handler)
    add.quiz.handlers(qu, quiz.handler)
  qu
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, has.check.btn=!qu$single.check.btn, defaults=quizDefaults(), allow.zero.correct=FALSE) {
  restore.point("init.quiz.part")

  part = copy.into.missing.fields(dest=part, source=defaults)

  if (!is.null(part[["sc"]])) {
    part$choices = part$sc
    part$multiple = FALSE
    #part$type = "sc"
  } else if (!is.null(part[["mc"]])) {
    part$choices = part$mc
    part$multiple = TRUE
    #part$type = "mc"
  }


  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (length(correct.choices)==0 & part$multiple==FALSE & !allow.zero.correct) {
      stop(paste0("You have not specified a correct answer for your quiz with question:\n\n", part$question,"\nJust put a * at the end of the correct answer. If you want to allow quizzes without correct answer set the argument allow.zero.correct=TRUE in your call to parse_mooc_rmd."))
    }

    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)
    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=1e-7
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  }

  if (is.null(part[["points"]])) {
    part$points = 0
  }

  txt = part$success

  if (part$points==1) {
    txt = paste0(txt," (", part$points, " ", defaults$point_txt,")")
  } else if (part$points > 0 ) {
    txt = paste0(txt," (", part$points, " ", defaults$points_txt,")")
  }
  txt = colored.html(txt, part$success_color)
  part$success =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  txt = colored.html(part$failure, part$failure_color)
  part$failure =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  part$id = paste0(qu$id,"__part", part.ind)
  part$answerId = paste0(part$id,"__answer")
  if (has.check.btn) {
    part$checkBtnId = paste0(part$id,"__checkBtn")
  } else {
    part$checkBtnId = NULL
  }
  part$resultId = paste0(part$id,"__resultUI")
  part$ui = quiz.part.ui(part)
  part$solved = FALSE

  if (is.null(part$points)) {
    part$points = 1
  }

  part
}

quiz.ui = function(qu, solution=FALSE) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }

    if (solution) {
      if (is.null(part$sol.ui)) {
        part$sol.ui = quiz.part.ui(part, solution=TRUE)
      }
      return(list(part$sol.ui,hr))
    } else {
      return(list(part$ui,hr))
    }
  })
  if (!is.null(qu$checkBtnId)) {
    pli = c(pli, list(actionButton(qu$checkBtnId,label = "check")))
  }

  pli
}

quiz.part.ui = function(part, solution=FALSE, add.button=!is.null(part$checkBtnId)) {
  question = rmdtools::md2html(part$question)
  head = list(
    HTML(paste0("<p>",question,"</p>"))
  )
  if (solution) {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",part$choices,selected = part$answer)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",part$choices, selected=part$answer)
    }
  } else {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",part$choices)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",part$choices, selected=NA)
    }
  }

  if (add.button) {
    button = actionButton(part$checkBtnId,label = "check")
  } else {
    button = NULL
  }
  list(head,answer,uiOutput(part$resultId),button)
}

quiz.md = function(qu, solution=FALSE) {
  restore.point("quiz.md")
  li = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    quiz.part.md(part, solution=solution)
  })
  paste0(li, collapse="\n")
}


quiz.part.md = function(part, solution=FALSE) {
  restore.point("quiz.part.md")

  head = paste0("\nQuiz: ",part$question,"\n")
  if (solution) {
    if (part$type=="numeric" | part$type == "text") {
      answer = paste0("Answer: ", part$answer)
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      mark = rep("[ ]", length(ans))
      mark[ans %in% part$answer] =  "[x]"
      answer = paste0("- ", ans, " ", mark,"\n", collapse="\n")
    }
  } else {
    if (part$type=="numeric" | part$type == "text") {
      answer = "Answer: "
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      answer = paste0("- ", ans, "[   ]\n", collapse="\n")
    }
  }
  paste0(head,"\n", answer)
}


add.quiz.handlers = function(qu, quiz.handler=NULL, id=qu$id){
  restore.point("add.quiz.handlers")
  app = getApp()
  if (is.null(app)) {
    cat("\nCannot add quiz handlers since no shinyEvents app object is set.")
    return()
  }

  if (!qu$single.check.btn) {
    for (part.ind in seq_along(qu$parts)) {
      part = qu$parts[[part.ind]]
      buttonHandler(part$checkBtnId,fun = click.check.quiz, part.ind=part.ind, qu=qu, quiz.handler=quiz.handler)
    }
  } else {
    buttonHandler(qu$checkBtnId,fun = click.check.quiz, part.ind=0, qu=qu, quiz.handler=quiz.handler)
  }
}

click.check.quiz = function(app=getApp(), part.ind, qu, quiz.handler=NULL, ...) {
  restore.point("click.check.quiz")

  # check all parts
  if (part.ind == 0) {
    for (part.ind in seq_along(qu$parts))
      click.check.quiz(app=app, part.ind=part.ind,qu=qu, quiz.handler=quiz.handler)

    if (!is.null(quiz.handler)) {
      quiz.handler(app=app, qu=qu, part.ind=0, part.correct=NA, solved=qu$state$solved, answer=NA)
    }
    return(qu$state$solved)
  }

  part = qu$parts[[part.ind]]
  answer = getInputValue(part$answerId)
  restore.point("click.check.quiz.inner")


  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else {
    correct = setequal(answer,part$answer)
  }
  if (correct) {
    cat("Correct!")
    setUI(part$resultId,HTML(part$success))
    dsetUI(part$resultId,withMathJax(HTML(part$success)))
  } else {
    cat("Wrong")
    setUI(part$resultId,HTML(part$failure))
    dsetUI(part$resultId,withMathJax(HTML(part$failure)))
  }
  qu$state$part.solved[part.ind] = correct
  qu$state$solved = all(qu$state$part.solved)

  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.ind=part.ind, part.correct=correct, solved=qu$state$solved, answer=answer)
  }

}
