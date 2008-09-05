#.First.lib<-function(lib, pkg) {
#      options(warn=1)
#      options(warning.expression={cat("WARN: Warning-Info see Console window")})
#      cat(".First.lib erledigt!\n")
#}
relax<-function(file.name,no.plots=FALSE,cmds="",but.Wizardry=TRUE){
 # Copyright (C) 2005--2008 Hans Peter Wolf
  options(warn=1)
 #      options(warning.expression={cat("WARNING: Warning-Info see Console window")})
  relax.path<-.path.package("relax"
)
  relax.pos<-grep("^package:relax$",search())[1]

  assign("running.function","relax"
, pos=relax.pos)
  myhead.menu<-function(item="Test",code=function()cat("Menu-Test"),
                        title="Menue",rm.menu=FALSE,menu.no=1){
    set.tclvalue<-function(name,value)tclvalue(name)<-as.character(value)
    out.msg<-NULL
    menu.widget.name<-paste("mrtrevive",   menu.no,sep="")
    menu.item.name  <-paste("mmyhead.menu",menu.no,sep="")
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    if( !( exists(menu.widget.name,envir=revive.sys)
 )){
      if( rm.menu==TRUE && menu.no!=0){
        tkmessageBox(title="Warnung", icon="warning",
                     message="Achtung:  kein Kopfmenue vorhanden")
        return("Error")
      }
      fhead<-get("fhead",envir=revive.sys)
      mrtrevive<-tkmenubutton(fhead,text=title,
                              font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
,
                              relief="flat", width=10
)
      assign(menu.widget.name,mrtrevive,envir=revive.sys)
      tkbind(mrtrevive,"<Enter>",function() set.tclvalue("tvmess","R cmd menue"))
      tkbind(mrtrevive,"<Leave>",function(){ set.tclvalue("tvmess","relax") })
      tkpack(mrtrevive,side="left")
      mmyhead.menu<-tkmenu(mrtrevive)
      assign(menu.item.name,mmyhead.menu,envir=revive.sys)
      tkconfigure(mrtrevive, menu=get(menu.item.name,env=revive.sys))
      out.msg<-c(out.msg, paste("Menue im Kopf eingerichtet!"))

    }
    if( rm.menu==TRUE ){
      mrtrevive<-get(menu.widget.name,envir=revive.sys)
      tkpack("forget",mrtrevive)
      tkdestroy(mrtrevive)
      remove(list=menu.widget.name,envir=revive.sys)
      out.msg<-c(out.msg, paste("Menue im Kopf entfernt!"))

    }else{
      item<-as.character(item[[1]])
      mmyhead.menu<-get(menu.item.name,envir=revive.sys)
      revive.env <- get("revive.env",envir=revive.sys)
      if(is.function(code))  code<-deparse(body(code))
      item.code<-function()eval(parse(text=code),envir=revive.env)
      tkadd(mmyhead.menu, "command", label=item, command=item.code,
              font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"

      )
      out.msg<-c(out.msg, paste("Item",item,"im Kopfmenue eingerichtet!"))

    }
    return(out.msg)
  }
  assign("myhead.menu",myhead.menu, pos=relax.pos)


  melde<-function(report.msg,typ=0,...){
    cat<-get("cat",pos="package:base")
    # if(exists("last.warning"))get("print",pos="package:base")(last.warning)
    if(typ==0) cat(report.msg,...,"\n")
    if(exists("DEBUG") && TRUE==DEBUG){
      if(typ==1 | typ==2) cat(c(a="Start:",z="Ende:")[typ], report.msg,"\n")
      if(typ==3){cat(report.msg); cat(...); cat("\n")}
    }
    if(typ=="cmd.msg"){
      if((version$os=="Win32" || version$os=="mingw32")
) flush.console()
      if(exists("report.msg.to.file.flag")) cat(report.msg, "\n", file="report.msg", append=TRUE)
    }
    invisible()
  }
  assign("melde",melde, pos=relax.pos)

  #############################################
  # configuration file of relax
  #############################################
  #
  ## general parameters of relax
  # language for messages:
  language<-"english"
  # language<-"german"
  # maximal characters of output to be shown
  maxol.sys<-4000
  # size of fonts # set of values: 1,2, ..., 7
  initial.font.size<-4
  # height of included postscript plot in LaTeX document:
  psheight.sys<-"10cm"
  # height parameter for postscript device
  psdesignheight.sys<-6
  # width parameter for postscript device
  psdesignwidth.sys<-6
  # rotation of PS graphics
  pshorizontal.sys<-FALSE
  # size parameter for jpeg device in inch
  jpgdesignsize.sys<-4
  # height of relax window ( e.g.: "500" or "1200" ) 
  relaxwindow.height.sys<-"700"
  # width of relax window ( e.g.: "500" or "1200" ) 
  relaxwindow.width.sys<-"700"
  # use of "tab"-key for name completion
  name.complete.sys<-TRUE
  # replace german umlaute
  replace.umlaute.sys<-TRUE
  #############################################
  ## special settings for windows
  # how to call LaTeX
  latex.command.windows<-"echo q | latex"
  # view command for viewing .dvi files
  view.command.windows<-"yap"
  # how to call dvipdf
  dvipdf.command.windows<-"dvipdfm"
  # text editor
  text.editor.windows<-"notepad"
  # browser
  browser.windows<-" "
  # browser.windows<-"c:/Programme/\"Mozilla Firefox\"/firefox.exe "
  pdfview.windows<-"acroread"
  # ghostscript-program e.g.: ghostscript<-"c:\\gs\\gs7.04\\bin\\gswin32"
  ghostscript<-" "
  # Path to Tcl/Tk-img-package:
  # imgpath.sys <- "C:/Tcl/lib"
  #############################################
  ## special settings for linux
  path.tcltk.package.img<-"/usr/local/lib"
  # how to call LaTeX
  latex.command.linux<-"echo q | latex"
  # view command for viewing .dvi files
  view.command.linux<-"xdvi"
  # how to call dvipdf
  dvipdf.command.linux<-"dvipdf"
  # text editor ?????????
  text.editor.linux<-"kwrite"
  # browser
  browser.linux<-"konqueror"
  # pdf viewer
  pdfview.linux<-"acroread"
  #############################################
  ## special settings for mac
  # how to call LaTeX
  latex.command.mac<-"echo q | pdflatex" 
  # view command for viewing .dvi files
  view.command.mac<-"open " 
  # how to call dvipdf
  dvipdf.command.mac<-"dvipdf" 
  # text editor
  text.editor.mac<-"nano"
  # browser
  browser.mac<-"safari"

  try({
    h<-scan(file=file.path(relax.path,"config/settings.relax"),what="",sep="\n")
    for(i in seq(h)) eval(parse(text=h[i]))
  })
  editor.sys<-text.editor.mac
  if((version$os=="Win32" || version$os=="mingw32")
) {
    editor.sys <- text.editor.windows
  }
  if(substring(version$os,1,5)=="linux"
) {
    editor.sys <- text.editor.linux
  }
  browser.sys<-""
  if((version$os=="Win32" || version$os=="mingw32")
) {
    browser.sys<-"start "
    if(exists("browser.windows")&&nchar(browser.windows)>0&&0<length(grep("[a-zA-Z]",browser.windows))) 
      browser.sys <- browser.windows
  }
  if(substring(version$os,1,5)=="linux"
) {
    if(exists("browser.linux")) browser.sys <- browser.linux
  }
  if(substring(version$os,1,7)=="darwin8"){
    browser.sys<-"open "
    latex.command.linux<-latex.command.mac
    view.command.linux<-view.command.mac
    dvipdf.command.linux<-dvipdf.command.mac
    text.editor.linux<-text.editor.mac
  }
  if(!exists("initial.font.size")) initial.font.size<-4
  initial.font.size<-initial.font.size[1]
  if(is.na(initial.font.size) || all(initial.font.size!=(1:7))) initial.font.size<-4
  if(!exists("relaxwindow.width.sys")) relaxwindow.width.sys<-"700"
  if(!exists("relaxwindow.height.sys")) relaxwindow.height.sys<-"700"
  if(!exists("name.complete.sys")) name.complete.sys<-TRUE
  if((version$os=="Win32" || version$os=="mingw32")
){
    settclenvvars<-function(){
      if(exists("imgpath.sys")) imgpath<-imgpath.sys else
        imgpath<-file.path(relax.path,"lib")  # ; print(imgpath)
      try(.Tcl(paste("lappend auto_path",imgpath)))
      return(0<length(grep("img",ignore.case=TRUE,list.files(imgpath))))
    }
    res<-settclenvvars()
    if(res){   
      try.res<-try(.Tcl("package require Img"))
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


    }
    if(!res || !ok){ 
            cat('Warning: Tcl/Tk-package Img not found!',
      'Without this package relax is not able to show jpeg pictures in the report window.',
      'However, the pictures will appear in the html report and the formated tex report.',
      'For installing the Img package see: > help(relax).',
                sep="\n" )
       no.plots<-TRUE
    }
  } 
  if(substring(version$os,1,5)=="linux"
){
    Img.ok<-FALSE
    if(file.exists(path.tcltk.package.img)){ # pwolf-lokal
            addTclPath(path.tcltk.package.img); Img.ok<-TRUE
    }
    if(Img.ok) try(tclRequire("Img"))

  }

  set.tclvalue<-function(name,value)  tclvalue(name)<-as.character(value)
  assign("set.tclvalue",set.tclvalue, pos=relax.pos)
    
  if(!exists("revive.env")){
    revive.env<<-revive.env<-new.env(parent=.GlobalEnv)
  } else {
    revive.sys<-get("revive.sys",envir=revive.env)
    rm(list=ls(env=revive.sys),  envir=revive.sys)
  }
  revive.sys<-environment()
  assign("revive.sys", revive.sys, envir=revive.env)

  .Tcl("set XYZ [encoding system]")
  is.UTF<- 0<length(grep("utf",tclvalue("XYZ")))
  readline<-function(prompt=""){
    if(! ("1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
) ){
      readline<-get("readline", pos="package:base")
      return(readline(prompt=prompt))
    }
    .newl<-tktoplevel()
    tkwm.title(.newl, "text input"); tkwm.geometry(.newl,"+0+15")
    if(exists("running.function") && running.function=="relax"
){
      tkpack(minfo<-tkmessage(.newl,width="1000",justify="left",relief="raised"))
      if(!exists("toutwin"))
        toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))

      news<-strsplit(tclvalue(tkget(toutwin,"0.0","end")),"\n")[[1]]
      if(length(news)>10) news<-rev(rev(news)[1:10])
      if(length(h<-grep("^output", news))>0) news<-news[-h]
      if(length(h<-grep("^@", news))>0)      news<-news[-h]
      if(length(h<-grep("verbatim",news))>0) news<-news[-h]
      if(length(news)>0){
        news<-rev(rev(news)[1:min(20,length(news))])
        news<-paste(news,collapse="\n")
        tkconfigure(minfo,text=news)
      }

    }else{
      tkpack(minfo<-tkmessage(.newl,width="1000",justify="left",relief="raised"))
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      worktext<-worktext[worktext!=""]
      line<-rev(grep("^<<(.*)>>=",worktext))[1]
      if(!is.na(line)&length(line)>0) worktext<-worktext[-(1:line)]
      line<-grep("^@",worktext)[1]
      if(!is.na(line)&length(line)>0) worktext<-worktext[-(1:line)]
      if(length(h<-grep("^output", worktext))>0) worktext<-worktext[-h]
      if(length(h<-grep("^@", worktext))>0)      worktext<-worktext[-h]
      if(length(h<-grep("verbatim",worktext))>0) worktext<-worktext[-h]
      if(length(worktext)>0){
        worktext<-rev(rev(worktext)[1:min(20,length(worktext))])
        worktext<-paste(worktext,collapse="\n")
        tkconfigure(minfo,text=worktext)
      }

    }


    eline<-tkentry(.newl,textvariable="tvreadline")
    set.tclvalue("tvreadline","")
    lline<-tklabel(.newl,text=prompt)
    tkpack(eline, lline, side="right");         tkfocus(eline)
    twin<-eline; f.sonderzeichen<-function(zeichen){
                   function(){
                     tkinsert(twin,"insert",zeichen)
                     if((version$os=="Win32" || version$os=="mingw32")
)tkdelete(twin,"insert-1chars")
                     if(substring(version$os,1,7)=="darwin8")tkdelete(twin,"insert-1chars")
                   }
                 }
                 f.umlaut<-function(zeichen){
                   function(){
                     return()
                     #char337<-eval(parse(text='"\\337"'))
                     #if(zeichen==char337 & tclvalue(tkget(twin,"insert-1chars","insert"))=="\\") return()
                     #tkinsert(twin,"insert",zeichen); tkdelete(twin,"insert-2chars")
                   }
                 }
                 tkbind(twin,"<<LKeckig>>", f.sonderzeichen("["))
                 tkbind(twin,"<<RKeckig>>", f.sonderzeichen("]"))
                 tkbind(twin,"<<Tilde>>",   f.sonderzeichen("~"))
                 tkbind(twin,"<<LKgeschw>>",f.sonderzeichen("{"))
                 tkbind(twin,"<<RKgeschw>>",f.sonderzeichen("}"))
                 tkbind(twin,"<<Klammera>>",f.sonderzeichen("@"))
                 tkbind(twin,"<<Pipe>>",    f.sonderzeichen("|"))
                 tkbind(twin,"<<Backsl>>",  f.sonderzeichen("\\"))
                 renewhighlighting<-function(){
                   tworkwin<-get("tworkwin",env=revive.sys)
                   melde("ak texthervor",1)
                   tcl("markclear",tworkwin)
                   tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                   tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                   tcl("marklinetypes",tworkwin)
                   melde("ak texthervor",2)

                 }
                 # tkbind(twin,"<<Klammeraffe>>",renewhighlighting)
                 tkbind(twin,"<Return>",renewhighlighting)

    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    tkbind(.newl,"<Return>",function(){tkfocus(tworkwin);tkdestroy(.newl)})
    tkwait.window(.newl)
    news<-paste("\nreadline Input:\n", (out<-tclvalue("tvreadline")))
    if(exists("running.function") && running.function=="relax"
){
           if(!exists("toutwin"))
             toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
           pos.to.insert<-"end"
           news<-paste(gsub("\n+","\n",news),collapse="\n")
           try(tkinsert(toutwin,pos.to.insert,news))
           tksee(toutwin,"end - 0 lines")
           melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    }else{
           if(!exists("tworkwin"))
             tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

           pos.to.insert<-"end"
           if(0<length(grep("output-start",news))){
             tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
             ltail<-length(tail)
             if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                news<-sub(".*output-start\n","",news)
                news<-sub("output-end","",news)
                h<-seq(along=h)[h][1]
                pos.to.insert<-paste("end -",h,"lines")
             }
           }
           try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
           tksee(tworkwin,"end - 0 lines")
           melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    }

    return(out)
  }
  assign("readline",readline,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("readline saved",3)

  menu<-function(choices, graphics=FALSE, title=""){
   if(! ("1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
) ){
      menu<-get("menu", pos="package:base")
      return(menu(choices=choices,graphics=graphics,title=title))
   }else{
    TopN<-tktoplevel()
    if(title=="") title<-"select item, press Return (or exit by Esc)"
    tkwm.geometry(TopN,"+0+15"); tkwm.title(TopN, title)
    if(missing(choices)||length(choices)==0) choices<-"relax"
    choices<-paste(seq(choices),": ",choices, sep="")
    nc<-length(choices<- c(choices, "EXIT"))
    scr <- tkscrollbar(TopN, command=function(...)tkyview(tl,...))
    tl<-tklistbox(TopN,height=min(30,length(choices)),width=60,
                  selectmode="single",yscrollcommand=
                    function(...)tkset(scr,...),background="white")
    for(ch in choices) tkinsert(tl,"end",ch)
    tkpack(tl,side="left",expand="yes",fill="y")
    tkpack(scr,side="left",expand="yes",fill="y")
    set.tclvalue("choice","0"); tkbind(TopN,"<Escape>",function(){tkdestroy(TopN)})
    tkbind(TopN,"<Return>",function(){
       choice<-as.numeric(tkcurselection(tl))+1
       set.tclvalue("choice",choice); tkdestroy(TopN)
    })
    tkwait.window(TopN)
    choice<-tclvalue("choice")
    choice<- if(choice==length(choices)) 0 else as.numeric(choice)
    news<-paste("",paste(choices,collapse="\n"),"\nSelection: ",choice, "",sep="")
    if(exists("running.function") && running.function=="relax"
){
           if(!exists("toutwin"))
             toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
           pos.to.insert<-"end"
           news<-paste(gsub("\n+","\n",news),collapse="\n")
           try(tkinsert(toutwin,pos.to.insert,news))
           tksee(toutwin,"end - 0 lines")
           melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    }else{
           if(!exists("tworkwin"))
             tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

           pos.to.insert<-"end"
           if(0<length(grep("output-start",news))){
             tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
             ltail<-length(tail)
             if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                news<-sub(".*output-start\n","",news)
                news<-sub("output-end","",news)
                h<-seq(along=h)[h][1]
                pos.to.insert<-paste("end -",h,"lines")
             }
           }
           try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
           tksee(tworkwin,"end - 0 lines")
           melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    }

    tkfocus(get("tworkwin",env=get("revive.sys",env=revive.env))) # wichtig!!
    return(choice)
   }
  }
  assign("menu",menu,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("menu saved",3)

  scan<-function(file="",what=double(0),nmax=-1,n=-1,sep="",
                 quote="",dec=".",skip=0,nlines=0,na.strings="NA",flush=FALSE,
                 fill=FALSE,strip.white=FALSE,quiet=FALSE,blank.lines.skip=TRUE){
      scan<-get("scan",pos="package:base")

      if(file!="" || ! ("1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
) ){
          if(!missing(n))
            worktext<-scan(file=file,what=what,n=n,sep=sep,quote=quote,dec=dec,
                         skip=skip,nlines=nlines,na.strings=na.strings,
                         flush=flush,fill=fill,strip.white=strip.white,quiet=quiet,
                         blank.lines.skip=blank.lines.skip)
          else
            worktext<-scan(file=file,what=what,nmax=nmax,sep=sep,quote=quote,dec=dec,
                         skip=skip,nlines=nlines,na.strings=na.strings,
                         flush=flush,fill=fill,strip.white=strip.white,quiet=quiet,
                         blank.lines.skip=blank.lines.skip)

      } else {
        typ<-if(is.numeric(what)) "Zahlen-Eingabe" else "Text-Eingabe"
        .newl<-tktoplevel(); tkwm.title(.newl, typ); tkwm.geometry(.newl,"+0+15")
        revive.sys<-get("revive.sys",envir=revive.env)
        assign(".newl",.newl,envir=revive.sys)
        if(exists("running.function") && running.function=="relax"
){
          tkpack(minfo<-tkmessage(.newl,width="1000",justify="left",relief="raised"))
          if(!exists("toutwin"))
            toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))

          news<-strsplit(tclvalue(tkget(toutwin,"0.0","end")),"\n")[[1]]
          if(length(news)>10) news<-rev(rev(news)[1:10])
          if(length(h<-grep("^output", news))>0) news<-news[-h]
          if(length(h<-grep("^@", news))>0)      news<-news[-h]
          if(length(h<-grep("verbatim",news))>0) news<-news[-h]
          if(length(news)>0){
            news<-rev(rev(news)[1:min(20,length(news))])
            news<-paste(news,collapse="\n")
            tkconfigure(minfo,text=news)
          }

        }else{
          tkpack(minfo<-tkmessage(.newl,width="1000",justify="left",relief="raised"))
          if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
          tworkwin<-get("tworkwin",envir=revive.sys)
          worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
          if(nchar(worktext)<10000){
            worktext<-strsplit(worktext,"\n")[[1]]
          }else{
            base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
            worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
          }

          worktext<-worktext[worktext!=""]
          line<-rev(grep("^<<(.*)>>=",worktext))[1]
          if(!is.na(line)&length(line)>0) worktext<-worktext[-(1:line)]
          line<-grep("^@",worktext)[1]
          if(!is.na(line)&length(line)>0) worktext<-worktext[-(1:line)]
          if(length(h<-grep("^output", worktext))>0) worktext<-worktext[-h]
          if(length(h<-grep("^@", worktext))>0)      worktext<-worktext[-h]
          if(length(h<-grep("verbatim",worktext))>0) worktext<-worktext[-h]
          if(length(worktext)>0){
            worktext<-rev(rev(worktext)[1:min(20,length(worktext))])
            worktext<-paste(worktext,collapse="\n")
            tkconfigure(minfo,text=worktext)
          }

        }


        if(!missing(n)&&n==1){
          sp<-"10"; zei<-"1"
          tkbind(.newl,"<Return>",function() set.tclvalue("tvscandone",2))
        }else{sp<-"50"; zei<-"3"}
        tkbind(.newl,"<Return><Return>",function()set.tclvalue("tvscandone",2))
        tscan<-tktext(.newl,width=sp,height=zei)
        twin<-tscan; f.sonderzeichen<-function(zeichen){
                       function(){
                         tkinsert(twin,"insert",zeichen)
                         if((version$os=="Win32" || version$os=="mingw32")
)tkdelete(twin,"insert-1chars")
                         if(substring(version$os,1,7)=="darwin8")tkdelete(twin,"insert-1chars")
                       }
                     }
                     f.umlaut<-function(zeichen){
                       function(){
                         return()
                         #char337<-eval(parse(text='"\\337"'))
                         #if(zeichen==char337 & tclvalue(tkget(twin,"insert-1chars","insert"))=="\\") return()
                         #tkinsert(twin,"insert",zeichen); tkdelete(twin,"insert-2chars")
                       }
                     }
                     tkbind(twin,"<<LKeckig>>", f.sonderzeichen("["))
                     tkbind(twin,"<<RKeckig>>", f.sonderzeichen("]"))
                     tkbind(twin,"<<Tilde>>",   f.sonderzeichen("~"))
                     tkbind(twin,"<<LKgeschw>>",f.sonderzeichen("{"))
                     tkbind(twin,"<<RKgeschw>>",f.sonderzeichen("}"))
                     tkbind(twin,"<<Klammera>>",f.sonderzeichen("@"))
                     tkbind(twin,"<<Pipe>>",    f.sonderzeichen("|"))
                     tkbind(twin,"<<Backsl>>",  f.sonderzeichen("\\"))
                     renewhighlighting<-function(){
                       tworkwin<-get("tworkwin",env=revive.sys)
                       melde("ak texthervor",1)
                       tcl("markclear",tworkwin)
                       tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                       tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                       tcl("marklinetypes",tworkwin)
                       melde("ak texthervor",2)

                     }
                     # tkbind(twin,"<<Klammeraffe>>",renewhighlighting)
                     tkbind(twin,"<Return>",renewhighlighting)

        bexit<-tkbutton(.newl, text="Exit: end of input")
        tkpack(tscan, bexit); tkfocus(tscan)
        tkconfigure(bexit,command=function()set.tclvalue("tvscandone",2))
        set.tclvalue("tvscandone",0); tkwait.variable("tvscandone")
        worktext<-tclvalue(tkget(tscan,"0.0","end"))
        if(!exists("tworkwin"))
          tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

        tkfocus(tworkwin); tkdestroy(.newl)
        news<-paste("\nscan-Eingabe:\n",worktext,sep="")
        if(exists("running.function") && running.function=="relax"
){
               if(!exists("toutwin"))
                 toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
               pos.to.insert<-"end"
               news<-paste(gsub("\n+","\n",news),collapse="\n")
               try(tkinsert(toutwin,pos.to.insert,news))
               tksee(toutwin,"end - 0 lines")
               melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


        }else{
               if(!exists("tworkwin"))
                 tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

               pos.to.insert<-"end"
               if(0<length(grep("output-start",news))){
                 tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
                 ltail<-length(tail)
                 if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                    any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                    news<-sub(".*output-start\n","",news)
                    news<-sub("output-end","",news)
                    h<-seq(along=h)[h][1]
                    pos.to.insert<-paste("end -",h,"lines")
                 }
               }
               try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
               tksee(tworkwin,"end - 0 lines")
               melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

        }

             worktext<-strsplit(worktext,"\n")[[1]]
        worktext<-strsplit(paste(worktext,collapse=" ")," ")[[1]]
        worktext<-worktext[worktext!=""]
        if(typ=="Zahlen-Eingabe"){
          try.res<-try(as.numeric(worktext))
          if(is.function(try.res)){
            ok <- "OK"
          } else {
            if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
            ok<-try.res[1]
            if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
            if(!is.character(ok)) { ok <- "OK" }
          }
          if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
            ok<-FALSE
            cat(error.msg<-unclass(try.res),"\n")
            if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
               cat("A warning message stopped the evaluation!",
                     "If you want to\nevaluate the code anyway",
                     "evaluate code by:\n>WarnEval<")
            cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
          } else { ok<-TRUE }


          if(ok) worktext<-try.res else NULL
        }


      }
      worktext
  }
  assign("scan",scan,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("scan saved",3)

  # myscan<-get("scan",pos="package:base"); formals(myscan)$comment.char<-""
  myscan<-function(file,what,sep="\n", blank.lines.skip=FALSE){ readLines(file) } #2.1.0

  print<-function(x, ...){ # 050614
    if( "1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
 ){
      sink(get("tmp.file.name",env=revive.sys)
); base::print(x, ...); sink()
       news<-paste("", # date(),
                   paste(scan(file=get("tmp.file.name",env=revive.sys)
,what="",sep="\n"),collapse="\n"),
                   "",sep="\n" )
      if(exists("running.function") && running.function=="relax"
){
             if(!exists("toutwin"))
               toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
             pos.to.insert<-"end"
             news<-paste(gsub("\n+","\n",news),collapse="\n")
             try(tkinsert(toutwin,pos.to.insert,news))
             tksee(toutwin,"end - 0 lines")
             melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


      }else{
             if(!exists("tworkwin"))
               tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

             pos.to.insert<-"end"
             if(0<length(grep("output-start",news))){
               tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
               ltail<-length(tail)
               if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                  any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                  news<-sub(".*output-start\n","",news)
                  news<-sub("output-end","",news)
                  h<-seq(along=h)[h][1]
                  pos.to.insert<-paste("end -",h,"lines")
               }
             }
             try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
             tksee(tworkwin,"end - 0 lines")
             melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      }

    } else base::print(x, ...)
  }
  assign("print",print,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("print saved",3)

  cat<-function(...,file="",sep=" ",fill=FALSE,labels=NULL,append=FALSE){
    cat<-get("cat",pos="package:base")
    cat(...,file=file,sep=sep,fill=fill,labels=labels,append=append)
    if(file==""&& "1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
 ){
       cat(...,file=get("tmp.file.name",env=revive.sys)
,sep=sep,fill=fill,labels=labels,append=append)
       news<-paste("\n", # date(),
                   paste(scan(file=get("tmp.file.name",env=revive.sys)
,what="",sep="\n"),collapse="\n"),
                   "",sep="\n")
       if(exists("running.function") && running.function=="relax"
){
              if(!exists("toutwin"))
                toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
              pos.to.insert<-"end"
              news<-paste(gsub("\n+","\n",news),collapse="\n")
              try(tkinsert(toutwin,pos.to.insert,news))
              tksee(toutwin,"end - 0 lines")
              melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


       }else{
              if(!exists("tworkwin"))
                tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

              pos.to.insert<-"end"
              if(0<length(grep("output-start",news))){
                tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
                ltail<-length(tail)
                if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                   any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                   news<-sub(".*output-start\n","",news)
                   news<-sub("output-end","",news)
                   h<-seq(along=h)[h][1]
                   pos.to.insert<-paste("end -",h,"lines")
                }
              }
              try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
              tksee(tworkwin,"end - 0 lines")
              melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

       }

    }
  }
  assign("cat",cat,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("cat saved",3)

  str<-function(object,...){
    if("1"==tclvalue(tkwinfo("exists",get("toutwin",get("revive.sys",revive.env))))
 ){
       base::cat(file=get("tmp.file.name",env=revive.sys)
,"")
       if(is.data.frame(object)) 
          base::cat(file=get("tmp.file.name",env=revive.sys)
,
             paste("'data.frame':  ", 
             paste(dim(object)[1],"obs. of",dim(object)[2],"variables:\n")))
       a <- deparse(getS3method("str", "default"))
       a <- gsub("cat[(]", paste("base::cat(file=\"", get("tmp.file.name",env=revive.sys)
, 
                 "\",append=TRUE,", sep = ""), a) # ))
       a <- sub(" str[(]", " mystr(", a) # ))
       mystr <- eval(parse(text = a))
       mystr(object)
       news <- scan(file = get("tmp.file.name",env=revive.sys)
,
                    what = "", sep = "\n")
       news <- sub("chr .data.frame.", "", news)
       if(0<length(ind<-grep("^ -",news))) news<-news[-ind]
       if(0<length(ind<-grep("^List of",news))) news<-news[-ind]
       news<-paste("\n", # date(),
                   paste(news,collapse="\n"),"",sep="\n")
       if(exists("running.function") && running.function=="relax"
){
              if(!exists("toutwin"))
                toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
              pos.to.insert<-"end"
              news<-paste(gsub("\n+","\n",news),collapse="\n")
              try(tkinsert(toutwin,pos.to.insert,news))
              tksee(toutwin,"end - 0 lines")
              melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


       }else{
              if(!exists("tworkwin"))
                tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

              pos.to.insert<-"end"
              if(0<length(grep("output-start",news))){
                tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
                ltail<-length(tail)
                if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                   any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                   news<-sub(".*output-start\n","",news)
                   news<-sub("output-end","",news)
                   h<-seq(along=h)[h][1]
                   pos.to.insert<-paste("end -",h,"lines")
                }
              }
              try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
              tksee(tworkwin,"end - 0 lines")
              melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

       }

    } else { utils::str(object,...) }
    invisible(NULL)
  }
  assign("str",str,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("str saved",3)

  step.dep<-deparse(stats::step)
  step.dep<-gsub("cat[(]", "relax::cat(",step.dep) # ))  
  step.dep<-gsub("print[(]", "relax::print(",step.dep) # ))  
  mystep <- eval(parse(text = step.dep))
  assign("step",mystep,pos=which(paste("package","relax"
,sep=":")==search())
)
  melde("step saved",3)

  WinToTcl.read<-function(x){
    try(if(is.UTF&&replace.umlaute.sys && any(is.na(iconv(x,"","LATIN1")))){  
      # Latin1 document loaded->translate to utf-8
      res<-tkmessageBox(message=if(language=="german") 
          "Soll das Encoding des Dokuments (?Latin1?) auf das lokale umgesetzt werden?"
        else 
          "Do you want to change the encoding of the document (?latin1?) to the local one?",
              title="Encoding?",icon="warning",type="yesnocancel",default="no")
      if("externalptr"==mode(res))  res<-tclvalue(res)
      if(res=="yes"){
        x<-iconv(x,"LATIN1","")
        cat("Latin1 Encoding of document has been changed to local coding\n")
      }
    })
    try(if(!is.UTF&&replace.umlaute.sys && any(is.na(iconv(x,"","LATIN1")))){  
      # utf-8 document loaded->translate to Latin1
      res<-tkmessageBox(message=if(language=="german") 
          "Soll das Encoding des Dokuments (?UTF-8?) auf das lokale umgesetzt werden?"
        else 
          "Do you want to change the encoding (?utf-8?) of the document to the local one?",
              title="Encoding?",icon="warning",type="yesnocancel",default="no")
      if("externalptr"==mode(res))  res<-tclvalue(res)
      if(res=="yes"){
        x<-iconv(x,"utf-8","")
        cat("utf-8 Coding of document has been changed to local coding\n")
      }
    })
    return(x)
  }

  if(is.UTF)  TcltoWin.write<-function(x){ return(x) } else {
    # Latin1-Umwandlung von Umlauten
    pc<-eval(parse(text='"\\283"'))  # UTF-8-pre-char
    uml.utf.8 <-eval(parse(text='"\\244\\266\\274\\204\\226\\234\\237"'))
    uml.latin1<-eval(parse(text='"\\344\\366\\374\\304\\326\\334\\337"'))
    TcltoWin.write<-function(x){
      if(replace.umlaute.sys){
        x<-chartr(uml.utf.8,uml.latin1,gsub(pc,"",x))
      }
      return(x)
    }
  }
  tmp.file.name <- tempfile("rt-tmp")
  assign(tmp.file.name,"tmp.file.name",env=revive.sys)

  tmp.sink.name <- tempfile("rt-sink")
  assign(tmp.sink.name,"tmp.sink.name",env=revive.sys)
  ##definiere [[SaveAsHtml]]##           
  ##definiere Testknopf\-funktion##
  fEvalRCode<-function(){
    melde("fEvalRCode",1)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    code.start<-grep("^<<(.*)>>=",worktext)
    if(0==length(code.start)){cat("Warning: no code found!!!\n");return()}
    code.start<-code.start[code.start<=line]
    if(0<length(code.start)){
      if((code.start<-max(code.start)+1)>length(worktext)) return()
      code.end  <-c(grep("^@",worktext),1+length(worktext))
      code.end  <-min(code.end[code.end>code.start])-1
      code<-worktext[code.start:code.end]
      code<-code[code!=""]
      if(length(weg.ab<-grep("^<<(.*)>>=",code))>0) code<-code[-(weg.ab:length(code))]
      if(length(code)==0 || code[1]=="@") code<-" "
      melde("code:",3,code,"\n")

    }
    if(0<length(code)){
      melde("vor tangle - Code:\n",3,code)
      if(length(grep("<<(.*)>>",code))>0 || length(grep(">#",code))>0){
        code.a    <- grep("^<<(.*)>>=",worktext)
        code.z    <- grep("^@",worktext)
        code.z    <- unlist(sapply(code.a ,function(x,y) y[y>x][1], code.z))
        if(any(h<-is.na(code.z))) code.z<-code.z[!h]
        ################
        # code.n    <- length(worktext)
        # change    <- rep(0,code.n); change[c(code.a ,code.z)]<-1
        # code.ch   <- worktext[1==(cumsum(change)%%2)]
        ## 080311
        ind<-rep(0,length(worktext)); copy<-0
        for(i in seq(ind)){
          if(i %in% code.z) copy<-0
          if(i %in% code.a) copy<-1
          ind[i]<-copy
        }
        code.ch   <- worktext[ind==1]
        ## cat("input-text, Anfaenge, Code.chunks"); print(worktext); print(code.a); print(code.ch)

        code.n    <- length(code.ch)

        code.ch<-gsub("@>>","DoSpCloseKl-esc",gsub("@<<","DoSpOpenKl-esc",code.ch))

        code.ch<-gsub("(.*)<<(.*)>>=(.*)","cOdEdEf\\2",code.ch)
        repeat{
          if(0==length(cand<-grep("<<(.*)>>",code.ch))) break
          code.ch<-unlist(strsplit(gsub("(.*)<<(.*)>>(.*)",
                     "\\1bReAkuSeChUnK\\2bReAk\\3",code.ch),"bReAk"))
        }
        code.ch<-code.ch[code.ch!=""]
        code.n<-length(code.ch)
        melde("code.ch:",3,code.ch,"\n")

        line.typ  <-rep("C",code.n)
        code.a    <-grep("cOdEdEf",code.ch)
        code.ch[code.a]<-substring(code.ch[code.a],8)
        line.typ[code.a]<-"D"
        code.use    <-grep("uSeChUnK",code.ch)
        code.ch[code.use]<-substring(code.ch[code.use],9)
        line.typ[code.use]<-"U"
        code.ext  <-grep("#<file",code.ch)
        line.typ[code.ext]<-"E"
        melde("code.ch:",3,code.ch,"\n")

        code.out<-"##act:##"

        def.names<-code.ch[code.a]
        use.names<- if(length(code.use)>0) code.ch[code.use] else NULL
        code.z<-c(if(length(code.a)>1) code.a[-1]-1, code.n)
        code.ch<-paste(line.typ,code.ch,sep="")
        melde("code.ch:",3,code.ch,"\n")

        melde("vor expand - Code:\n",3,code)
        melde("bearbeite aktuellen Chunk\n",3)
        line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        ch.no<-length(grep("^<<(.*)>>=",worktext[1:line]))

        rows      <-c((code.a[ch.no]+1),code.z[ch.no])
        if(all(!is.na(rows))&&rows[1]<=rows[2]){
          rows<-rows[1]:rows[2]
          code.stack<-code.ch[rows]
          max.depth.refinements<-500; i<-1
          repeat{
             if((i<-i+1)>max.depth.refinements){ 
                 cat("ERROR: maximal number of expandations (",max.depth.refinements,
                     ") exceeded\n --- perhaps a unintended recursion ???")
                 return()
             }
             if(0==length(code.stack))break
             typ<-substring(code.stack[1],1,1)
             if("C"==typ||"E"==typ){
               n.lines<-sum(cumprod("C"==substring(code.stack,1,1)))
               code.out<-c(code.out, substring(code.stack[1:n.lines],2))
               code.stack<-code.stack[-(1:n.lines)]
             }
             if(length(code.stack)>0 && "U"==substring(code.stack[1],1,1)){
               if(any(found<-def.names==substring(code.stack[1],2))){
                 found<-seq(along=def.names)[found]; rows<-NULL
                 for(no in found){
                   if((code.a[no]+1)<=code.z[no]) rows<-c(rows,(code.a[no]+1):code.z[no])
                 }
                 code.stack<-c(code.ch[rows],code.stack[-1])
                 melde("found",0,found)
               }else{code.stack<-code.stack[-1]}
             }

          }
        }
        if(length(code.ext)>0){
          code.out<-code.out[code.out!=""]
          code.ext<-rev(grep(">#",code.out))
          found<-TRUE
          repeat{
            if(length(code.ext)==0) break

            if(!found){
              code.out[code.ext[1]]<-paste("# ??",code.out[code.ext[1]])
              cat("ERROR: External Chunk",code.out[code.ext[1]],"not found!!!\n")
              code.ext<-code.ext[-1]
            }

            found<-TRUE
            ext.name <- rev(unlist(strsplit(code.out[code.ext[1]],"#<file:")))[1]
            ext.name <- unlist(strsplit(unlist(strsplit(ext.name,">#"))[1],":"))
            ext.chunk<-ext.name[2]; ext.name <-ext.name[1]
            ext.name.n<-nchar(ext.name)
            if(ext.name.n >4 && ".rev"==substring(ext.name,ext.name.n-3,ext.name.n)){
              ext.name<-substring(ext.name,1,ext.name.n-4)
            }

            if(is.na(as.numeric(ext.chunk))){
              # tld untersuchen
              filename<-paste(ext.name[1],".rev",sep="")
              if(!file.exists(filename)){
                cat("ERROR: file",filename,"for expansion of code chunk not found!!!\n")
                ext.file<-"Error"
              }else{
                ext.file<-try(myscan(file=filename,what="",sep="\n"))
              }
              if("Error"==substring(unlist(ext.file)[1],1,5)){
                found <-FALSE; next
              }
              ext.file <-ext.file[grep("^<<(.*)>>=",ext.file)]
              if(!is.null(ext.file)){ found<-FALSE; next }
              ext.chunk<-grep(ext.chunk,ext.file)
            }

            filename<-paste(ext.name[1],".R",sep="")
            if(!file.exists(filename)){
              cat("Warning: file",filename,"not found!!!\n")
              cat("         file",filename,"is now generated!!!\n")
              try(tangleR(ext.name[1]))
            }
            if(!file.exists(filename)){
              ext.file<-"Error"
            }else{
              ext.file<-try(myscan(file=filename,what="",sep="\n"))
            }
            if("Error"==substring(unlist(ext.file)[1],1,5)){
              found <-FALSE; next
            }

            ext.chunk<-as.numeric(ext.chunk)
            a        <-grep(paste("#", ext.chunk,":",sep=""),ext.file)[1]
            z        <-grep(paste("#:",ext.chunk,    sep=""),ext.file)[1]
            if(is.na(a)){
              found <- FALSE; next
            }
            if(a<=z) ext.file <-ext.file[a:z]

            code.out <-c(code.out[1:(code.ext[1]-1)], ext.file,
                         if(length(code.out)>(code.ext[1]+1))
                           code.out[(code.ext[1]+1):length(code.out)]
                       )

            code.ext<-code.ext[-1]

          }

        }
        code.out<-c(code.out,"##:act##")

        code.out<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",code.out))

        melde("Ende Rtangle-last\n",3)
        code<-code.out[code.out!=""]
        melde("nach expand\n",3,code)
      }

      code<-c("options(warn=2)",code)
      try.res <- try(eval(parse(text=code),envir=revive.env))
      options(warn=1)

      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


      if(ok){
        if(!is.null(try.res)&&0<length(try.res)){
          if(!exists("tworkwin"))
            tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

          worktext<-TcltoWin.write(tclvalue(tkget(tworkwin,"0.0","end")))
          get("cat","package:base")(worktext,file="report-UnDo-bak.rev")

          sink(get("tmp.file.name",env=revive.sys)
);get("print",pos="package:base")(try.res);sink()
          news<-paste(myscan(get("tmp.file.name",env=revive.sys)
,"",sep="\n"),collapse="\n")
          if(nchar(news)>maxol.sys){
              news<-paste(substring(news,1,maxol.sys),"...",sep="\n")
          }
          news<-paste("", date(), news,"",sep="\n")
          if(!exists("toutwin"))
            toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
          pos.to.insert<-"end"
          news<-paste(gsub("\n+","\n",news),collapse="\n")
          try(tkinsert(toutwin,pos.to.insert,news))
          tksee(toutwin,"end - 0 lines")
          melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


      ##zeige Ergebnisse in neuem Fenster an>>
        }
      } else { cat("sorry, evaluation not successful!!!\n") }
    } else { cat("no code found!!!\n") }
    melde("event wird generiert",3)
    tkevent.generate(TopW,"<<Acticmds>>")

    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    melde("fEvalRCode",2)
  }

  fFindText<-function(){
    melde("fFindText",1)
  frage<-"text string to be searched?"; set.tclvalue("tvinfo",string.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        set.tclvalue("tvmess","relax")
        such<-string.sys<-tclvalue("tvinfo")
        assign("string.sys",string.sys,env=revive.sys)
        if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
        tworkwin<-get("tworkwin",envir=revive.sys)
        worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
        if(nchar(worktext)<10000){
          worktext<-strsplit(worktext,"\n")[[1]]
        }else{
          base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
          worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
        }

        if(nchar(such)==0) {
          tcl("findclear",tworkwin); tkfocus(tworkwin); return()
        }
        if(length(found<-grep(such,worktext))>0){
          line <- floor(as.numeric(tkindex(tworkwin,"insert")))
          line <- if(any(found>line)) found[found>line][1] else found[1]
          tksee(tworkwin,paste(line,".1",sep=""))
          h<-worktext[line]
          h<--1+charmatch(such,substring(h,1:nchar(h))); if(is.na(h)) h<-0 # 060605
          h<-paste(line,".",h,sep="")
          tkmark.set(tworkwin, "insert", h); tkfocus(tworkwin)
          tworkwin<-get("tworkwin",env=revive.sys)
          tktag.configure(tworkwin,"found",background="#000999fff",relief="raised")
          tcl("findmatches",tworkwin,such)
        } else set.tclvalue("tvmess",paste("Warning: search string >",tclvalue("tvinfo"),"< not found!!!"))
      } # end of function
    )
    melde("fFindText",2)
  }

  fHelp.R<-function(){
    melde("fHelp.R",1)
    frage<-"name of R function?"; set.tclvalue("tvinfo",string.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        # help.start(browser=browser.sys); Sys.sleep(0.1);
        fns<-tclvalue("tvinfo")
        mess<-paste("documentation of",fns,"appears in the R window;",
                              "click \"ok\" and change to the R window")
        res<-tkmessageBox(message=mess,title="Help",icon="info",type="ok")
        try.res<-try(eval(parse(text=
          paste("get(\"print\",\"package:base\")(help(\"",fns,"\",htmlhelp=FALSE))",sep=""))))
        if(length(try.res)==0){
          mess<- paste("Warning: no documentation for",fns,"found!")
          res<-tkmessageBox(message=mess,title="Help",icon="info",type="ok")
        }
        mess<-paste("relax"); set.tclvalue("tvmess",mess)
      } # end of function
    )
    melde("fHelp.R",2)
  }

  ##definiere Funktion für Knopf: [[InsertPlot]]##
  InsertTeX<-function(){
    melde("InsertTeX",1)
    h<-gsub(" ","",gsub(":","",date()))
    h<-substring(h,4:nchar(h),4:nchar(h))
    bildname<-paste(c("p",h[6:11],h[4:5],h[1:2],h[14:15]),collapse="")
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    start<-grep("begin-tex-code",worktext)
    start<-rev(start[start<line])[1]
    ende<-grep("end-tex-code",worktext)
    ende<-ende[ende>start][1]
    h<-worktext[start+1:ende-1]
    h<-c("\\documentclass{article}\\begin{document}",h,"\\end{document}")
    cat(h,file="tmptmp.tex",sep="\n")
    if(version$os=="linux-gnu") system("echo q|latex tmptmp.tex;dvips tmptmp.dvi")
    ##lege Bild unter [[bildname]] ab##
    ##zeige Bilder im Textfenster an##
    ##show.single.plot(tworkwin,line,jpgname) 
    melde("InsertTeX",2)
  }

  fPlanRCode<-function(){
     melde("fPlanRCode",1)
     if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
     tworkwin<-get("tworkwin",envir=revive.sys)
     worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
     if(nchar(worktext)<10000){
       worktext<-strsplit(worktext,"\n")[[1]]
     }else{
       base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
       worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
     }

     news <- "\n@\n\n<<*>>=\n\n"
     ##hole ggf. [[tworkwin]]>>
     line <-floor(as.numeric(tkindex(tworkwin,"insert")))

     ##lese Arbeitsfenster auf [[worktext]] ein>>
     textstart<-grep("^@",worktext)-1; textstart<-textstart[textstart>=line][1]
     codestart<-grep("^<<(.*)>>=",worktext)-1; codestart<-codestart[codestart>=line][1]
     if(is.na(codestart))codestart<-Inf; if(is.na(textstart))textstart<-Inf
     insertline<-if(codestart==textstart) NA else min(codestart,textstart)
     anzrows<-length(unlist(strsplit(news,"\n")))
     if(is.na(insertline)){
         insertline<-"end"
         try(tkinsert(tworkwin,"end","\n"))
         try(tkinsert(tworkwin,"end",paste(news,collapse="\n")))
         tkmark.set(tworkwin, "insert","end - 2 lines")
         tksee(tworkwin,"end")  # paste(insertline+anzrows,"0",sep="."))
         insertline<-length(worktext)
     }else{
       # in einem Text-Chunks muss ein Kl-Affe eingebaut werden.
         if(length(grep("<<\\*>>=",news[1]))>0 && codestart < textstart) news<-c(news,"@\n")
         try(tkinsert(tworkwin,paste(insertline+1,"0",sep="."),paste(news,collapse="\n")))
         tkmark.set(tworkwin, "insert", paste(insertline+anzrows,"0",sep="."))
         tksee(tworkwin,paste(insertline+anzrows,"0",sep="."))
     }
     ## melde(insertline)
     melde("ak texthervor",1)
     tcl("markclear",tworkwin)
     tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
     tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
     tcl("marklinetypes",tworkwin)
     melde("ak texthervor",2)

     ##zeige Bilder im Textfenster an##
     tkfocus(tworkwin)
     melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

     if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
     tworkwin<-get("tworkwin",envir=revive.sys)
     worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
     if(nchar(worktext)<10000){
       worktext<-strsplit(worktext,"\n")[[1]]
     }else{
       base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
       worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
     }

     line <-floor(as.numeric(tkindex(tworkwin,"insert")))

     code.start<-grep("^<<(.*)>>=",worktext)
     try(if(0<length(code.start)){ 
            worktext[code.start]<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext[code.start])
            worktext[code.start]<-paste(worktext[code.start]," (",1:length(code.start),")",sep="")
     })
     if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
     tkdelete(tworkwin,"0.0","end")
     try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
     tksee(tworkwin,"end")
     melde("ak texthervor",1)
     tcl("markclear",tworkwin)
     tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
     tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
     tcl("marklinetypes",tworkwin)
     melde("ak texthervor",2)

     if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


     tkmark.set(tworkwin, "insert", paste(line,"0",sep="."))
     tksee(tworkwin,paste(line,"0",sep="."))
     tkfocus(tworkwin)

     ##zeige Bilder im Textfenster an##
     melde("fPlanRCode",2)
  }

  fRemoveOut<-function(){
    melde("fRemoveOut",1)
    worktext<-""
    if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
    tkdelete(toutwin,"0.0","end")
    try(tkinsert(toutwin,"0.0",paste(worktext,collapse="\n")))
    melde("fRemoveOut",2)
  }

  fInsert<-function(){
    melde("fInsert",1)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    news<-tclvalue(tkget(toutwin,"0.0","end"))
    if(1<nchar(news)){
      if(length(grep("egin[{]table[}]",news))>0 &&
         length(grep("generated.*xtable.*package",news))>0){
          news<-sub("(\n%.*begin[{]table[}])","\noutput-end\n\\1",news)       
          news<-sub("(.end[{]table[}])","\\1\noutput-start",news)       
          news<-paste("\n@",paste("output-start",news,sep=""),"output-end\n", sep="\n")
          news<-sub("output-start\n+output-end","",news)
      }else{    
        news<-paste("\n@","output-start",news,"output-end\n",sep="\n")
      }
      news<-gsub("\n+","\n",news)
      ##hole ggf. [[tworkwin]]>>
      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      ##lese Arbeitsfenster auf [[worktext]] ein>>
      textstart<-grep("^@",worktext)-1; textstart<-textstart[textstart>=line][1]
      codestart<-grep("^<<(.*)>>=",worktext)-1; codestart<-codestart[codestart>=line][1]
      if(is.na(codestart))codestart<-Inf; if(is.na(textstart))textstart<-Inf
      insertline<-if(codestart==textstart) NA else min(codestart,textstart)
      anzrows<-length(unlist(strsplit(news,"\n")))
      if(is.na(insertline)){
          insertline<-"end"
          try(tkinsert(tworkwin,"end","\n"))
          try(tkinsert(tworkwin,"end",paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert","end - 2 lines")
          tksee(tworkwin,"end")  # paste(insertline+anzrows,"0",sep="."))
          insertline<-length(worktext)
      }else{
        # in einem Text-Chunks muss ein Kl-Affe eingebaut werden.
          if(length(grep("<<\\*>>=",news[1]))>0 && codestart < textstart) news<-c(news,"@\n")
          try(tkinsert(tworkwin,paste(insertline+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(insertline+anzrows,"0",sep="."))
          tksee(tworkwin,paste(insertline+anzrows,"0",sep="."))
      }
      ## melde(insertline)
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      ##zeige Bilder im Textfenster an##
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      worktext<-""
      if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
      tkdelete(toutwin,"0.0","end")
      try(tkinsert(toutwin,"0.0",paste(worktext,collapse="\n")))
    }
    melde("fInsert",2)
  }

  fSavePlot<-function(){
    melde("fSavePlot",1)
    h<-strsplit(gsub(":","",date())," ")[[1]]
    bildname<-paste("p",h[5],"-",h[2],h[3],"-",h[4],".ps",sep="")
    if(!is.null(bildname)&&nchar(bildname)>0){
      n<-nchar(bildname<-gsub(" ","",bildname))
      bildname<-sub(".ps$","",bildname)
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

    # Postscript:
      psname <-paste(bildname,".ps", sep="")
      news<-paste("@\n \\begin{center}","\\includegraphics[",
                           "height=",psheight.sys,"]{",bildname,"}\\end{center}\n",sep="") #081121
      try.res<-try({dev.copy(postscript,psname,horizontal=pshorizontal.sys,
                           width=psdesignwidth.sys,height=psdesignheight.sys);dev.off()})
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


      if(!ok) cat("Error: *ps file not generated by dev.copy!!!\n")
    # jpeg:
      jpgname<-paste(bildname,".jpg",sep="")
      news<-paste(news,'\n% <p><img src="',jpgname,'">\n@\n', sep="" )
      if((version$os=="Win32" || version$os=="mingw32")
){ # width=width in pixel, 72 dpi
        try.res<-try({dev.copy(jpeg,jpgname,width=jpgdesignsize.sys*72,
              height=jpgdesignsize.sys*72,quality=100,pointsize=7);dev.off()})
      }else{
        try.res<-try({dev.copy(bitmap,type="jpeg",jpgname,
             width=jpgdesignsize.sys,height=jpgdesignsize.sys);dev.off()})
      }
    # gif:
      if(substring(version$os,1,7)=="darwin8" ){
        gifname<-sub("jpg$","gif",jpgname) 
        try.res<-try({system(paste("convert",jpgname,gifname))})
      }
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


      if(!ok) cat("Error: jpg of gif file not generated by dev.copy!!!\n")
      ##hole ggf. [[tworkwin]]>>
      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      ##lese Arbeitsfenster auf [[worktext]] ein>>
      textstart<-grep("^@",worktext)-1; textstart<-textstart[textstart>=line][1]
      codestart<-grep("^<<(.*)>>=",worktext)-1; codestart<-codestart[codestart>=line][1]
      if(is.na(codestart))codestart<-Inf; if(is.na(textstart))textstart<-Inf
      insertline<-if(codestart==textstart) NA else min(codestart,textstart)
      anzrows<-length(unlist(strsplit(news,"\n")))
      if(is.na(insertline)){
          insertline<-"end"
          try(tkinsert(tworkwin,"end","\n"))
          try(tkinsert(tworkwin,"end",paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert","end - 2 lines")
          tksee(tworkwin,"end")  # paste(insertline+anzrows,"0",sep="."))
          insertline<-length(worktext)
      }else{
        # in einem Text-Chunks muss ein Kl-Affe eingebaut werden.
          if(length(grep("<<\\*>>=",news[1]))>0 && codestart < textstart) news<-c(news,"@\n")
          try(tkinsert(tworkwin,paste(insertline+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(insertline+anzrows,"0",sep="."))
          tksee(tworkwin,paste(insertline+anzrows,"0",sep="."))
      }
      ## melde(insertline)
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      ##zeige Bilder im Textfenster an##
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      insertline<-insertline+3
      melde(paste("p", psname), "cmd.msg")
    }
    tworkwin<-get("tworkwin",envir=revive.sys)
    no.plots<-get("no.plots",envir=revive.sys) #081125
    if(!no.plots) createandshow.single.plot(tworkwin,insertline,jpgname)
    melde("fSavePlot",2)
  }

  # ------------------------------------------------------------------------
  # a new plot should be shown
  try(.Tcl( paste(
    "proc createandshowimage {w im place} {",
    "global imageno",
     if(substring(version$os,1,7)=="darwin8") 
        "set imageno [image create photo -format gif -file $im]" else
        "set imageno [image create photo -format jpeg -file $im]",
     "$w image create $place -image $imageno",
  "}", sep="\n") ) )
  assign("pic.list.sys",NULL,env=revive.sys)
  # ------------------------------------------------------------------------
  createandshow.single.plot<-function(textwidget,row.of.plot,jpgname){
    melde("createandshow.single.plot",1)
    # fix pic name / file 
    picname<-if(substring(version$os,1,7)=="darwin8") sub(".jpg$",".gif",jpgname) else jpgname
    if(!substring(version$os,1,7)=="darwin8"){
        if(file.exists(sub(".jpg$",".ps",picname))&&(!file.exists(picname))){
          pstojpg(sub(".jpg$",".ps",picname),picname)
        }
    }
    if(!file.exists(picname)) return()
    # create and show image
    place<-paste(row.of.plot,".0",sep="")
    try.res<-try(tcl("createandshowimage",textwidget,picname,place))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    # update pic list
    if(try.res=="ok"){
      imageno<-tclvalue(.Tcl("set imageno"))
      pic.list<-rbind(get("pic.list.sys",env=revive.sys),c(picname,imageno))
      assign("pic.list.sys",pic.list,env=revive.sys)
    }
    melde("createandshow.single.plot",2)
  }
  # --------------------------------------------------------------------
  # all plots should be shown again
  try(.Tcl( paste(
    "proc showsingleimage {w imageno place} {",
       "$w image create $place -image $imageno",
    "}", sep="\n") ) )
  # --------------------------------------------------------------------
  show.plots.again<-function(tworkwin){
      melde("show.plots.again",1)
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      rows   <-grep("<img src=",worktext); if(0==length(rows))return()
      pic.names.report  <-unlist(lapply(strsplit(worktext[rows],"<img src=\""), 
                               function(x){ x<-x[2]; strsplit(x,"\"")[[1]][1] }))
      pic.list<-get("pic.list.sys",env=revive.sys)
      ind<-match(pic.names.report,pic.list[,1]); image.ok<-!is.na(ind); ind<-ind[image.ok]
      if(0==length(ind))return()
      place<-paste(rows[image.ok]-1,".0",sep=""); imageno<-pic.list[ind,2]
      for(i in seq(imageno)){ 
           try(tcl("showsingleimage",tworkwin,imageno[i],place[i]))
      }
      melde("show.plots.again",2)
  }
  # ------------------------------------------------------------         
  # create and show all plots
  createandshow.all.plots<-function(tworkwin){
     melde("createandshow.all.plots",1)
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      rows   <-grep("<img src=",worktext); if(0==length(rows))return()
      pic.names.report<-unlist(lapply(strsplit(worktext[rows],"<img src=\""), 
                               function(x){ x<-x[2]; strsplit(x,"\"")[[1]][1] }))
      place<-paste(rows-1,".0",sep="")
      # fix pic name / file 
      picname<-if(substring(version$os,1,7)=="darwin8") 
             sub(".jpg$",".gif",pic.names.report) else pic.names.report
      if(0==length(picname)) return()
      # create missing jpg-files
      if(!substring(version$os,1,7)=="darwin8"){
        for(i in seq(picname)){
           if(file.exists(sub(".jpg$",".ps",picname[i]))&&(!file.exists(picname[i]))){
             pstojpg(sub(".jpg$",".ps",picname[i]),picname[i])
           }
        }
      }
     #
     image.nos<-rep("xx",length(picname))
     for(i in seq(picname)){
         if(!file.exists(picname[i])) next
         try.res<-try(tcl("createandshowimage",tworkwin,picname[i],place[i]))
         if(is.function(try.res)){
           ok <- "OK"
         } else {
           if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
           ok<-try.res[1]
           if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
           if(!is.character(ok)) { ok <- "OK" }
         }
         if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
           ok<-FALSE
           cat(error.msg<-unclass(try.res),"\n")
           if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
              cat("A warning message stopped the evaluation!",
                    "If you want to\nevaluate the code anyway",
                    "evaluate code by:\n>WarnEval<")
           cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
         } else { ok<-TRUE }


         if(try.res=="ok") image.nos[i]<-tclvalue(.Tcl("set imageno"))
     }
     pic.list<-cbind(picname,image.nos)
     pic.list<-pic.list[pic.list[,2]!="xx",,drop=FALSE]
     assign("pic.list.sys",pic.list,env=revive.sys)
     melde("createandshow.all.plots",2)
  } 
  # --------------------------------------------------------
  exclude.plots<-function(tworkwin){
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    rows<-grep("<img src=",worktext); if(0==length(rows))return()
    names  <-unlist(lapply(strsplit(worktext[rows],"<img src=\""), 
                    function(x){ x<-x[2]; strsplit(x,"\"")[[1]][1] }))
    rows <- rows - 1   # + 1
    for(i in seq(along=rows)){
      anf<-paste(rows[i],".0",sep=""); end<-paste(rows[i],".end",sep="")
      a<-tclvalue(tkget(tworkwin,anf,end))
      tkdelete(tworkwin,anf,end)
      tkinsert(tworkwin,anf,paste(a,collapse="\n")) # 040922
  #    tkdelete(tworkwin,paste(rows[i],".0",sep=""),paste(rows[i],".1",sep=""))
    }
  }

  pstojpg<-function(psname,jpgname){
    return() # im Moment nicht im Dienst
    if(substring(version$os,1,5)=="linux"
){
      gsexe <- Sys.getenv("R_GSCMD")
      if(is.null(gsexe) || nchar(gsexe) == 0) {
        gsexe <- "gs"; rc <- system(paste(gsexe, "-help > /dev/null"))
        if (rc != 0) return()
      }
    }
    if((version$os=="Win32" || version$os=="mingw32")
){
      if(exists("ghostscript")&&0<nchar(ghostscript)&&0<length(grep("[A-Za-z]",ghostscript))) 
        gsexe<-ghostscript else return()
    }
    type<-"jpeg"; width<-height<-13; res<-30
    cmd <- paste(gsexe, " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                        " -r", res,
                        " -g",ceiling(res*width),"x",ceiling(res*height),
                        " -sOutputFile=", jpgname, "  ",psname, sep = "")
    try(system(cmd)); invisible()
  }

  ##definiere Funktion für Knopf: [[CopyToEnd]]##
  fTrashROutput<-function(){
    melde("fTrashROutput",1)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    out.end  <-grep("^output-end",worktext)
    out.end  <-out.end[out.end>=(line-1)][1]
    if(is.na(out.end)) return()
    out.start <-grep("^output-start",worktext)
    out.start<-rev(out.start[out.start<out.end])[1]
    if(is.na(out.end)) return()
    code.start   <-grep("^<<(.*)>>=",worktext)
    code.start <- rev(code.start[code.start<out.end])[1]
    if(is.na(code.start)) return()
    if(code.start>out.start) return()
    if("@"==worktext[out.start-1]) out.start<-out.start-1
    if(""==worktext[out.start-1]) out.start<-out.start-1
    if(""==worktext[out.start-1]) out.start<-out.start-1
    trash<-worktext[out.start:out.end]
    worktext<-worktext[-(out.start:out.end)]
    if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
    tkdelete(tworkwin,"0.0","end")
    try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
    tksee(tworkwin,"end")
    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


    tkfocus(tworkwin)
    tkmark.set(tworkwin,"insert",paste(out.start-1,".0",sep=""))
    tksee(tworkwin,"insert")
    melde("fTrashROutput",2)
  }

  fWarnEval<-function(){ # vorher EvalCursorChunk
    melde("fWarnEval",1)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    code.start<-grep("^<<(.*)>>=",worktext)
    if(0==length(code.start)){cat("Warning: no code found!!!\n");return()}
    code.start<-code.start[code.start<=line]
    if(0<length(code.start)){
      if((code.start<-max(code.start)+1)>length(worktext)) return()
      code.end  <-c(grep("^@",worktext),1+length(worktext))
      code.end  <-min(code.end[code.end>code.start])-1
      code<-worktext[code.start:code.end]
      code<-code[code!=""]
      if(length(weg.ab<-grep("^<<(.*)>>=",code))>0) code<-code[-(weg.ab:length(code))]
      if(length(code)==0 || code[1]=="@") code<-" "
      melde("code:",3,code,"\n")

    }
    if(0<length(code)){
      melde("vor tangle - Code:\n",3,code)
      if(length(grep("<<(.*)>>",code))>0 || length(grep(">#",code))>0){
        code.a    <- grep("^<<(.*)>>=",worktext)
        code.z    <- grep("^@",worktext)
        code.z    <- unlist(sapply(code.a ,function(x,y) y[y>x][1], code.z))
        if(any(h<-is.na(code.z))) code.z<-code.z[!h]
        ################
        # code.n    <- length(worktext)
        # change    <- rep(0,code.n); change[c(code.a ,code.z)]<-1
        # code.ch   <- worktext[1==(cumsum(change)%%2)]
        ## 080311
        ind<-rep(0,length(worktext)); copy<-0
        for(i in seq(ind)){
          if(i %in% code.z) copy<-0
          if(i %in% code.a) copy<-1
          ind[i]<-copy
        }
        code.ch   <- worktext[ind==1]
        ## cat("input-text, Anfaenge, Code.chunks"); print(worktext); print(code.a); print(code.ch)

        code.n    <- length(code.ch)

        code.ch<-gsub("@>>","DoSpCloseKl-esc",gsub("@<<","DoSpOpenKl-esc",code.ch))

        code.ch<-gsub("(.*)<<(.*)>>=(.*)","cOdEdEf\\2",code.ch)
        repeat{
          if(0==length(cand<-grep("<<(.*)>>",code.ch))) break
          code.ch<-unlist(strsplit(gsub("(.*)<<(.*)>>(.*)",
                     "\\1bReAkuSeChUnK\\2bReAk\\3",code.ch),"bReAk"))
        }
        code.ch<-code.ch[code.ch!=""]
        code.n<-length(code.ch)
        melde("code.ch:",3,code.ch,"\n")

        line.typ  <-rep("C",code.n)
        code.a    <-grep("cOdEdEf",code.ch)
        code.ch[code.a]<-substring(code.ch[code.a],8)
        line.typ[code.a]<-"D"
        code.use    <-grep("uSeChUnK",code.ch)
        code.ch[code.use]<-substring(code.ch[code.use],9)
        line.typ[code.use]<-"U"
        code.ext  <-grep("#<file",code.ch)
        line.typ[code.ext]<-"E"
        melde("code.ch:",3,code.ch,"\n")

        code.out<-"##act:##"

        def.names<-code.ch[code.a]
        use.names<- if(length(code.use)>0) code.ch[code.use] else NULL
        code.z<-c(if(length(code.a)>1) code.a[-1]-1, code.n)
        code.ch<-paste(line.typ,code.ch,sep="")
        melde("code.ch:",3,code.ch,"\n")

        melde("vor expand - Code:\n",3,code)
        melde("bearbeite aktuellen Chunk\n",3)
        line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        ch.no<-length(grep("^<<(.*)>>=",worktext[1:line]))

        rows      <-c((code.a[ch.no]+1),code.z[ch.no])
        if(all(!is.na(rows))&&rows[1]<=rows[2]){
          rows<-rows[1]:rows[2]
          code.stack<-code.ch[rows]
          max.depth.refinements<-500; i<-1
          repeat{
             if((i<-i+1)>max.depth.refinements){ 
                 cat("ERROR: maximal number of expandations (",max.depth.refinements,
                     ") exceeded\n --- perhaps a unintended recursion ???")
                 return()
             }
             if(0==length(code.stack))break
             typ<-substring(code.stack[1],1,1)
             if("C"==typ||"E"==typ){
               n.lines<-sum(cumprod("C"==substring(code.stack,1,1)))
               code.out<-c(code.out, substring(code.stack[1:n.lines],2))
               code.stack<-code.stack[-(1:n.lines)]
             }
             if(length(code.stack)>0 && "U"==substring(code.stack[1],1,1)){
               if(any(found<-def.names==substring(code.stack[1],2))){
                 found<-seq(along=def.names)[found]; rows<-NULL
                 for(no in found){
                   if((code.a[no]+1)<=code.z[no]) rows<-c(rows,(code.a[no]+1):code.z[no])
                 }
                 code.stack<-c(code.ch[rows],code.stack[-1])
                 melde("found",0,found)
               }else{code.stack<-code.stack[-1]}
             }

          }
        }
        if(length(code.ext)>0){
          code.out<-code.out[code.out!=""]
          code.ext<-rev(grep(">#",code.out))
          found<-TRUE
          repeat{
            if(length(code.ext)==0) break

            if(!found){
              code.out[code.ext[1]]<-paste("# ??",code.out[code.ext[1]])
              cat("ERROR: External Chunk",code.out[code.ext[1]],"not found!!!\n")
              code.ext<-code.ext[-1]
            }

            found<-TRUE
            ext.name <- rev(unlist(strsplit(code.out[code.ext[1]],"#<file:")))[1]
            ext.name <- unlist(strsplit(unlist(strsplit(ext.name,">#"))[1],":"))
            ext.chunk<-ext.name[2]; ext.name <-ext.name[1]
            ext.name.n<-nchar(ext.name)
            if(ext.name.n >4 && ".rev"==substring(ext.name,ext.name.n-3,ext.name.n)){
              ext.name<-substring(ext.name,1,ext.name.n-4)
            }

            if(is.na(as.numeric(ext.chunk))){
              # tld untersuchen
              filename<-paste(ext.name[1],".rev",sep="")
              if(!file.exists(filename)){
                cat("ERROR: file",filename,"for expansion of code chunk not found!!!\n")
                ext.file<-"Error"
              }else{
                ext.file<-try(myscan(file=filename,what="",sep="\n"))
              }
              if("Error"==substring(unlist(ext.file)[1],1,5)){
                found <-FALSE; next
              }
              ext.file <-ext.file[grep("^<<(.*)>>=",ext.file)]
              if(!is.null(ext.file)){ found<-FALSE; next }
              ext.chunk<-grep(ext.chunk,ext.file)
            }

            filename<-paste(ext.name[1],".R",sep="")
            if(!file.exists(filename)){
              cat("Warning: file",filename,"not found!!!\n")
              cat("         file",filename,"is now generated!!!\n")
              try(tangleR(ext.name[1]))
            }
            if(!file.exists(filename)){
              ext.file<-"Error"
            }else{
              ext.file<-try(myscan(file=filename,what="",sep="\n"))
            }
            if("Error"==substring(unlist(ext.file)[1],1,5)){
              found <-FALSE; next
            }

            ext.chunk<-as.numeric(ext.chunk)
            a        <-grep(paste("#", ext.chunk,":",sep=""),ext.file)[1]
            z        <-grep(paste("#:",ext.chunk,    sep=""),ext.file)[1]
            if(is.na(a)){
              found <- FALSE; next
            }
            if(a<=z) ext.file <-ext.file[a:z]

            code.out <-c(code.out[1:(code.ext[1]-1)], ext.file,
                         if(length(code.out)>(code.ext[1]+1))
                           code.out[(code.ext[1]+1):length(code.out)]
                       )

            code.ext<-code.ext[-1]

          }

        }
        code.out<-c(code.out,"##:act##")

        code.out<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",code.out))

        melde("Ende Rtangle-last\n",3)
        code<-code.out[code.out!=""]
        melde("nach expand\n",3,code)
      }

      try.res <- try(eval(parse(text=code),envir=revive.env))
      #options(warn=0)
      #options(warn.expression=NULL)

      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


      if(ok){
        if(!is.null(try.res)&&0<length(try.res)){
          if(!exists("tworkwin"))
            tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

          worktext<-TcltoWin.write(tclvalue(tkget(tworkwin,"0.0","end")))
          get("cat","package:base")(worktext,file="report-UnDo-bak.rev")

          sink(get("tmp.file.name",env=revive.sys)
);get("print",pos="package:base")(try.res);sink()
          news<-paste(myscan(get("tmp.file.name",env=revive.sys)
,"",sep="\n"),collapse="\n")
          if(nchar(news)>maxol.sys){
              news<-paste(substring(news,1,maxol.sys),"...",sep="\n")
          }
          news<-paste("", date(), news,"",sep="\n")
          if(!exists("toutwin"))
            toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
          pos.to.insert<-"end"
          news<-paste(gsub("\n+","\n",news),collapse="\n")
          try(tkinsert(toutwin,pos.to.insert,news))
          tksee(toutwin,"end - 0 lines")
          melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


      ##zeige Ergebnisse in neuem Fenster an>>
        }
      } else { cat("sorry, evaluation not successful!!!\n") }
    } else { cat("no code found!!!\n") }
    melde("event wird generiert",3)
    tkevent.generate(TopW,"<<Acticmds>>")

    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    ##lese Arbeitsfenster auf [[worktext]] ein##
    ##aktualisiere Code-Chunk-Zähler und schreibe [[worktext]]##
    ##generiere ein Ereignis zum Anzeigen von Warnungen##
    ##zeige ggf. Warnungen an#
    melde("fWarnEval",2)
  }

  fUp<-function(){
    melde("fUp",1)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    code.start<-grep("^<<(.*)>>=",worktext)
    code.start<-rev(code.start[code.start<line])[1]
    if(!is.na(code.start)) tkmark.set(tworkwin, "insert", paste(code.start,".0",sep=""))
    tksee(tworkwin,"insert")
    melde("fUp",2)
  }

  fDown<-function(){
    melde("fDown",1)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    code.start<-grep("^<<(.*)>>=",worktext)
    code.start<-code.start[code.start>line][1]
    if(!is.na(code.start)) tkmark.set(tworkwin, "insert", paste(code.start,".0",sep=""))
    tksee(tworkwin,"insert")
    melde("fDown",2)
  }

  Exit<-function(){
    melde("Exit",1)
    res<-tkmessageBox(message=
              if(language=="german") "Report-Manager ohne erneute Speicherung beenden?"
              else "Quit RELAX without saving again?",
                      title="Exit",icon="warning",type="yesnocancel",default="no")
    if("externalptr"==mode(res))  res<-tclvalue(res)
    if(res=="cancel") return()
    if(res=="no") SaveReport()
    set.tclvalue("tvexit","fertig"); tkdestroy(TopW)
    remove("print",pos=which(.path.package("relax")==searchpaths()))
    melde("==================================================\n")
    melde("RELAX --- EXIT                    \n")
    # melde("copy of report saved as: \n")
    # melde(" report-UnDo-bak.rev                           \n")
    melde("restart RELAX by: \n relax()           \n")
    melde("==================================================\n")
    melde("q","cmd.msg")
    melde("Exit",2)
  }

  SetPSDesignWidth<-function(){
    melde("SetPSDesignWidth",1)
    frage<-"ps design width?"; set.tclvalue("tvinfo",psdesignwidth.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>",
      function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        h<-as.numeric(tclvalue("tvinfo"))
        assign("psdesignwidth.sys",h,env=revive.sys)
        melde(paste('> assign("psdesignwidth.sys","',psdesignwidth.sys,'",env=revive.sys)',sep="")
                  , "cmd.msg")
      }
    )
    melde("SetPSDesignWidth",2)
  }
  SetPlotHeight<-function(){
    melde("SetPlotHeight",1)
    frage<-"ps height?"; set.tclvalue("tvinfo",psheight.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>",
      function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        psheight.sys<-tclvalue("tvinfo")
        assign("psheight.sys",psheight.sys,env=revive.sys)
        melde(paste('> assign("psheight.sys","',psheight.sys,'",env=revive.sys)',sep="")
              ,"cmd.msg")
      }
    )
    melde("SetPlotHeight",2)
  }
  SetPSDesignHeight<-function(){
    melde("SetPSDesignHeight",1)
    frage<-"ps design width?"; set.tclvalue("tvinfo",psdesignheight.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>",
      function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        h<-as.numeric(tclvalue("tvinfo"))
        assign("psdesignheight.sys",h,env=revive.sys)
        melde(paste('> assign("psdesignheight.sys","',psdesignheight.sys,'",env=revive.sys)',sep="")
                  , "cmd.msg")
      }
    )
    melde("SetPSDesignHeight",2)
  }

  SetPSRotation<-function(){
    melde("SetPSRotation",1)
    choices<-c("vertical / standard","horizontal") 
    activate<-1+pshorizontal.sys
    menuwidget <- tkmenu(TopW); set.tclvalue("tvchoice","0")
    for(i in choices) tkadd(menuwidget,"radiobutton",label=i,variable="tvchoice")
    tkpost(menuwidget,"0","0"); tkactivate(menuwidget,activate)
    tkbind(menuwidget,"<Escape>",function(){
           tkdestroy(menuwidget);set.tclvalue("tvchoice","0")})
    if( (! (version$os=="Win32" || version$os=="mingw32")
) ) tkwait.variable("tvchoice")

    choice <- tclvalue("tvchoice")
    choice <- if(choice!="0") choice<-which(choice==choices) else 0

    if(choice!=0){
        pshorizontal.sys<-2==choice
        assign("pshorizontal.sys",pshorizontal.sys,env=revive.sys)
        melde(paste(
             '> assign("pshorizontal.sys","',pshorizontal.sys,'",env=revive.sys)'
             ,sep=""),"cmd.msg")
    }
    melde("SetPSRotation",2)
  }

  SetJPGSize<-function(){
    melde("SetJPGSize",1)
    frage<-"jpg design size (3..8)?"; set.tclvalue("tvinfo",jpgdesignsize.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>",
      function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        h<-as.numeric(tclvalue("tvinfo"))
        assign("jpgdesignsize.sys",h,env=revive.sys)
        melde(paste(
    '> assign("jpgdesignsize.sys","',jpgdesignsize.sys,'",env=revive.sys)',
           sep=""), "cmd.msg")
      }
    )
    melde("SetJPGSize",2)
  }

  SetFontType<-function(){
    melde("SetFontType",1)
    choices<-c("helvetica","courier","new century schoolbook","times")
    activate<-which(strsplit(tfont.sys,"-")[[1]][3]==choices)
    menuwidget <- tkmenu(TopW); set.tclvalue("tvchoice","0")
    for(i in choices) tkadd(menuwidget,"radiobutton",label=i,variable="tvchoice")
    tkpost(menuwidget,"0","0"); tkactivate(menuwidget,activate)
    tkbind(menuwidget,"<Escape>",function(){
           tkdestroy(menuwidget);set.tclvalue("tvchoice","0")})
    if( (! (version$os=="Win32" || version$os=="mingw32")
) ) tkwait.variable("tvchoice")

    choice <- tclvalue("tvchoice")
    choice <- if(choice!="0") choice<-which(choice==choices) else 0

    if(choice!=0){
      font<-choices[choice]
      tfont.sys<-sub("be-.+-Me",paste("be-",font,"-Me",sep=""),tfont.sys)
      tkconfigure(tworkwin,font=tfont.sys)
      assign("tfont.sys",tfont.sys,env=revive.sys)
      ## if(exists("trevwin")) tkconfigure(trevwin, font=tfont.sys)
    }
    melde(paste("> font type changed"))
    melde("SetFontType",2)
  }
  SetFontSize<-function(){
    melde("SetFontSize",1)
    sizes<-c("8-80","10-100","12-120","14-140","18-180","24-240","*-2000")
    choices<-c("1 tiny","2 very small","3 small","4 normal","5 large",
               "6 very large","7 huge")
    activate<-which(substring(sub("(.*)Normal..","",tfont.sys),1,3)==substring(sizes,1,3))
    menuwidget <- tkmenu(TopW); set.tclvalue("tvchoice","0")
    for(i in choices) tkadd(menuwidget,"radiobutton",label=i,variable="tvchoice")
    tkpost(menuwidget,"0","0"); tkactivate(menuwidget,activate)
    tkbind(menuwidget,"<Escape>",function(){
           tkdestroy(menuwidget);set.tclvalue("tvchoice","0")})
    if( (! (version$os=="Win32" || version$os=="mingw32")
) ) tkwait.variable("tvchoice")

    choice <- tclvalue("tvchoice")
    choice <- if(choice!="0") choice<-which(choice==choices) else 0

    if(choice!=0){
      size<-sizes[choice]
      tfont.sys<-sub("al--.+-.+-\\*",paste("al--",size,"-*",sep=""),tfont.sys)
      tkconfigure(tworkwin,font=tfont.sys)
      assign("tfont.sys",tfont.sys,env=revive.sys)
      outfont.sys<-sub("al--.+-.+-\\*",paste("al--",size,"-*",sep=""),outfont.sys)
      assign("outfont.sys",outfont.sys,env=revive.sys)
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      tkconfigure(toutwin,font=outfont.sys)
      melde(paste("> font size changed"))
    }
    melde("SetFontSize",2)
  }

  SetOutputLength<-function(){
    melde("SetOutputLength",1)
    frage<-"maximum number of output character?"; set.tclvalue("tvinfo",maxol.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>",
      function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        h<-as.numeric(tclvalue("tvinfo"))
        if(!is.na(h)) assign("maxol.sys",h,env=revive.sys)
        melde(paste("> maxol.sys <-",maxol.sys),"cmd.msg")
      }
    )
    melde("SetOutputLength",2)
  }

  ConfigRelax<-function(){
    melde("ConfigRelax",1)
        .newl<-tktoplevel();tkwm.geometry(.newl,"+0+15");tkpack(tt<-tktext(.newl))
        tkpack(fcmd<-tkframe(.newl))
        Save<-tkbutton(fcmd,width=15,text="save settings",command=function(){
                   newsettings<-paste(tclvalue(tkget(tt,"0.0","end")),"\n")
                   filename<-file.path(.path.package("relax"),"config/settings.relax")
                   try(cat(file=filename,newsettings))
                   tkmessageBox(title="", icon="warning",
                            message="to get new settings work, relax has to be restarted")
                   print("new settings saved")
        })
        Exit<-tkbutton(fcmd,width=15,text="Exit",command=function(){
                   tkdestroy(.newl); set.tclvalue("tvscandone",2)
        })
        ReloadOld<-tkbutton(fcmd,width=15,text="reload old",command=function(){
                   settings<-get("settings",env=revive.sys)
                   tkdelete(tt,"0.0","end")
                   try(tkinsert(tt,"0.0",paste(settings,collapse="\n")))
        })
        LoadOrig<-tkbutton(fcmd,width=15,text="load original",command=function(){
          filename<-file.path(.path.package("relax"),"config/settings.init")
          settings<-scan(file=filename,what="",sep="\n")
          try(tkinsert(tt,"0.0",paste(settings,collapse="\n")))
        })
        tkpack(Save,ReloadOld,LoadOrig,Exit,side="left")
        tkwm.title(.newl,"view and change settings of relax: config/settings.relax")
        filename<-file.path(.path.package("relax"),"config/settings.relax")
        settings<-scan(file=filename,what="",sep="\n")
        assign("settings",settings,env=revive.sys)
        try(tkinsert(tt,"0.0",paste(settings,collapse="\n")))
        tkbind(.newl,"<Escape>", function(){
                   tkdestroy(.newl); set.tclvalue("tvscandone",2)
                 })
        tkfocus(.newl)
        set.tclvalue("tvmess","relax")
    melde("ConfigRelax",2)
  }
  FindReportText<-function(){
    melde("FindReportText",1)
    frage<-"text string to be searched?"; set.tclvalue("tvinfo",string.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        set.tclvalue("tvmess","relax")
        such<-string.sys<-tclvalue("tvinfo")
        assign("string.sys",string.sys,env=revive.sys)
        if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
        tworkwin<-get("tworkwin",envir=revive.sys)
        worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
        if(nchar(worktext)<10000){
          worktext<-strsplit(worktext,"\n")[[1]]
        }else{
          base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
          worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
        }

        if(nchar(such)==0) {
          tcl("findclear",tworkwin); tkfocus(tworkwin); return()
        }
        repl.pat<-gsub("(.)","\\\\\\1","^!$%&/()=?{}}+*#,.-;:\\_[") ## 070830
        repl.pat<-paste("([",repl.pat,"])",collapse="")
        such<-gsub(repl.pat,"\\\\\\1",such)
        if(length(found<-grep(such,worktext))>0){
          line <- floor(as.numeric(tkindex(tworkwin,"insert")))
          line <- if(any(found>line)) found[found>line][1] else found[1]
          tksee(tworkwin,paste(line,".1",sep=""))
          h<-worktext[line]
          h<--1+charmatch(such,substring(h,1:nchar(h))); if(is.na(h)) h<-0 # 060605
          h<-paste(line,".",h,sep="")
          tkmark.set(tworkwin, "insert", h); tkfocus(tworkwin)
          # tworkwin<-get("tworkwin",env=revive.sys)
          tktag.configure(tworkwin,"found",background="#000999fff",relief="raised")
          tcl("findmatches",tworkwin,such)
        } else set.tclvalue("tvmess",paste("Warning: search string >",tclvalue("tvinfo"),"< not found!!!"))
      } # end of function
    )
    melde("FindReportText",2)
  }
  proc<-c(  # 050628
     "proc findmatches {w pattern} {",
     "$w tag remove found    1.0 end",
     "  scan [$w index end] %d numLines",
     "  for {set i 1} {$i < $numLines} {incr i} {",
     "    $w mark set last $i.0",
     "    while {[regexp -indices $pattern \\",
     "       [$w get last \"last lineend\"] indices]} {",
     "     $w mark set first \"last + [lindex $indices 0] chars\"",
     "     $w mark set last \"last  + 1 chars\\",
     "                              + [lindex $indices 1] chars\"",
     "     uplevel [$w tag add found first last]",
     "     }",
     "  }",
     "}",
      "proc findclear w {",
        "$w tag remove found    1.0 end",
      "}"
    )
  .Tcl(paste(proc,collapse="\n"))


  GoToLine<-function(){
    melde("GoToLine",1)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    frage<-"GOTO line?"; set.tclvalue("tvinfo",line)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        line<-string.sys<-tclvalue("tvinfo")[1]
        assign("string.sys",string.sys,env=revive.sys)
        line<-as.numeric(line)
        if(!is.na(line)){
          tksee(tworkwin,h<-paste(line,".1",sep=""))
          tkmark.set(tworkwin, "insert", h); tkfocus(tworkwin)
        } else set.tclvalue("tvmess",paste("Warning: line",tclvalue("tvinfo"),"< not found!!!"))
      } # end of function
    )
    melde("GoToLine",2)
  }
  if(but.Wizardry){
   WebReport<-function(){
    melde("WebReport",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(weaveR(workname.sys,
                       replace.umlaute=replace.umlaute.sys))
    try(tangleR(workname.sys))
    melde("WebReport",2)
   }
   WeaveReport<-function(){
    melde("WeaveReport",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(weaveR(workname.sys,
                       replace.umlaute=replace.umlaute.sys))
    melde("WeaveReport",2)
   }
   WeaveReportNoCode<-function(){
    melde("WeaveReportNoCode",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(weaveR(workname.sys,show.code=FALSE,
                       replace.umlaute=replace.umlaute.sys))
    melde("WeaveReportNoCode",2)
   }
   WeaveReportNoText<-function(){
    melde("WeaveReportNoText",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(weaveR(workname.sys,show.text=FALSE,
                       replace.umlaute=replace.umlaute.sys))
    melde("WeaveReportNoText",2)
   }
   TangleReport<-function(){
    melde("TangleReport",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(tangleR(workname.sys))
    cat(sub(".rev$",".R",workname.sys),"generated\n")
    melde("TangleReport",2)
   }
   TangleReportNoComments<-function(){
    melde("TangleReportNoComments",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(tangleR(workname.sys,insert.comments=FALSE))
    cat(sub(".rev$",".R",workname.sys),"generated\n")
    melde("TangleReportNoComments",2)
   }
   TangleReportChunk<-function(){
    melde("TangleReportChunk",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    revfile<-readLines(workname.sys)
    codechunks<-grep(paste("^<","<.*",">",">","=",sep=""),revfile) 
    if(length(codechunks)==0) return()
    codechunks<-revfile[codechunks]; codechunks<-unique(codechunks)
    if(0==(ind<-menu(codechunks))) return()
    TLD<-codechunks[ind]; TLD<-sub(paste("^<","<(.*)",">",">","=",sep=""),"\\1",TLD)
    try(tangleR(workname.sys, expand.roots=TLD,expand.root.start=FALSE))
    cat("Remark: processing without saving of",workname.sys,"\n")
    cat(sub(".rev$",".R",workname.sys),"generated\n")
    melde("TangleReportChunk",2)
   }
   LaTeX.head<-function(){
    melde("LaTeX.head",1)
    # news<-scan(file=paste(.path.package("relax"),"lib/LaTeX-head.tex",sep="/"),what="",sep="\n")
    news<-readLines(paste(.path.package("relax"),"lib/LaTeX-head.tex",sep="/")) # 2.1.0
    news<-sub("NOWEBSTYLEFILE",file.path(relax.path,"lib","noweb"),news)
    news<-sub("JOBPATH",getwd(),news); news<-c(news,"")
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line<-"2.0"
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    try(tkinsert(tworkwin,line,paste(news,collapse="\n")))
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    melde("LaTeX.head",2)
   }
   LatexReport<-function(){
    melde("LatexReport",1)
    n<-nchar(filename<-workname.sys)
    filename<-sub("rev$","tex",filename)
    if(is.null(n)||5>n||substring(filename,n-3,n)!=".tex"){
      cat("ERROR: file",filename,"not compatible!!!\n");return()
    }
    if(!file.exists(filename)){
      cat("ERROR: file",filename,"not found!!!\n");return()
    }
    melde("latex process starts, messages: see output window\n") 
    # 060116: show logfile
    if((version$os=="Win32" || version$os=="mingw32")
){
        latex<-paste(latex.command.windows," ",filename,sep="")
        ok<-try(shell(latex,wait=TRUE))
  #     if(ok!=0){
  #        cat("!!!ERROR in LaTeX process !!!\n-- for details look at log file\n") 
  #        cmd <-paste(editor.sys, sub("tex$","log",filename))
  #        try(shell(cmd,wait=FALSE))
  #     }
    }else{ 
        latex<-paste(latex.command.linux," ",filename,sep="")
        ok<-try(system(latex))
  #     if(ok!=0){ 
  #        cat("!!!ERROR in LaTeX process !!!\n-- for details look at log file\n") 
  #        cmd <-paste(editor.sys, sub("tex$","log",filename))
  #        try(system(paste(cmd," &")))
  #     }
    }
    if(ok!=0){ 
          fname<-sub("tex$","log",filename); txt<-readLines(fname)
          ind<-c(grep("(^[!])",txt)[1],grep("^[?].q",txt)[1])
          ind<-ind[!is.na(ind)]
          if(0==length(ind)) ind<-length(txt)-10
          ind<-(-8+min(ind)):(-1+max(ind)); txt<-txt[ind]
          line<-grep("(^l[.][0-9])",txt,value=TRUE)
          line<-line[!is.na(line)]
          if(0<length(line)){
            line<-paste("......\n-> find error line by pressing <Ctrl/Strg> <G>  + line number:",
                        sub("..([0-9]*).*","\\1",line))
          }else{
            line<-"-> Wizardry -> ShowLogFile "
          }
          txt<-c(
                  "!!!ERROR in LaTeX process !!!\n-- for details look at the log file\n",
                  " \nExtract of LaTeX log file:\n......",txt,line
               )
          cat(txt,sep="\n")
    }
    cat("latex process finished")
    melde("LatexReport",2)
   }
   ShowLogFile<-function(){
    melde("ShowLogFile",1)
    n<-nchar(filename<-workname.sys)
    filename<-sub("rev$","log",filename)
    if(is.null(n)||5>n||substring(filename,n-3,n)!=".log"){
      cat("ERROR: file",filename,"not compatible!!!\n");return()
    }
    if(!file.exists(filename)){ cat("ERROR: file",filename,"not found!!!\n");return() }
    cmd <-paste(editor.sys, filename)
    if((version$os=="Win32" || version$os=="mingw32")
){
        try(shell(cmd,wait=FALSE))
    }else{
      if( substring(version$os,1,7)=="darwin8" ){
        cmd<-paste(view.command.mac,filename)
      } 
      try(system(paste(cmd," &")))
    }
    melde("ShowLogFile",2)
   }
   ViewReport<-function(){
    melde("ViewReport",1)
    n<-nchar(filename<-workname.sys)
    filename<-sub("rev$","dvi",filename)
    if(is.null(n)||5>n||substring(filename,n-3,n)!=".dvi"){
      cat("ERROR: file",filename,"not compatible!!!\n");return()
    }
    if(substring(version$os,1,7)=="darwin8" ){
          filename<-sub("dvi$","pdf",filename)   
    }    
    if(!file.exists(filename)){
      cat("ERROR: file",filename,"not found!!!\n");return()
    }
    if((version$os=="Win32" || version$os=="mingw32")
){
        view<-paste(view.command.windows," ",filename,sep="")
        try(shell(view,wait=FALSE))
    }else{
        if(substring(version$os,1,5)=="linux"
) 
          view<-paste(view.command.linux,"  ",filename," &",sep="")
        if(substring(version$os,1,7)=="darwin8" ){
          view<-paste(view.command.mac,"  ",filename," &",sep="")
        }
        try(system(view))
    }
    melde("ViewReport",2)
   }
   DvipdfReport<-function(){  #050607
    melde("DvipdfReport",1)
    n<-nchar(filename<-workname.sys)
    filename<-sub("rev$","dvi",filename)
    if(is.null(n)||5>n||substring(filename,n-3,n)!=".dvi"){
      cat("ERROR: file",filename,"not compatible!!!\n");return()
    }
    if(!file.exists(filename)){
        cat("ERROR: file",filename,"not found!!!\n");return()
    }
    if((version$os=="Win32" || version$os=="mingw32")
){
        dvipdf<-paste(dvipdf.command.windows," ",filename,sep="")
        try(shell(dvipdf,wait=FALSE))
    }else{
        dvipdf<-paste(dvipdf.command.linux,"  ",filename," &",sep="")
        try(system(dvipdf))
    }
    cat("\"",dvipdf,"\" has been started!\n")
    melde("DvipdfReport",2)
   }
   ProcessReport<-function(){
      if(file.exists(filename<-file.path(getwd(),workname.sys))){
        res<-tkmessageBox(message=
                   if(language=="german") 
                      paste("Datei",filename,"existiert. Soll sie ersetzt werden?")
                   else paste(filename,"exists. Do you want to replace it?"),
                 title="Save File",icon="warning",type="yesnocancel",default="yes")
        if("externalptr"==mode(res))  res<-tclvalue(res)
        if(res=="cancel")return()
        if(res=="no"){msg<-SaveReport(); if(msg=="cancel") return()}  #050607
      }
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      if(0==length(grep("documentclass", worktext)))
        cat("Warning: there is no \\documentclass-command in the file:",
             "LaTeX needs a preamble for processing tex-files!",sep="\n")
      worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
      worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
      worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
      if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
         worktext<-c(worktext,"@\n\\end{document}")
      worktext<-TcltoWin.write(worktext)
      try.res <- try(cat(worktext,file=filename,sep="\n"))
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }



      if(ok){
        melde(paste("report file",filename,"saved\n"),0)
        melde(paste("w",workname.sys),"cmd.msg")
      } else {
        cat("ERROR: write operation failed!!!\n"); return()
      }
      Sys.sleep(0.25)
      try(weaveR(workname.sys,
                       replace.umlaute=replace.umlaute.sys))
      Sys.sleep(0.5)
      LatexReport()
   }
   SWEAVE<-function(){ ###060112
      workname.Rnw<-sub("rev$","rnw",workname.sys)
      if(file.exists(filename<-file.path(getwd(),workname.sys))){
        res<-tkmessageBox(message=
                   if(language=="german") 
                      paste("Datei",workname.sys,"oder",workname.Rnw,
                                "existiert. Soll(en) sie ersetzt werden?")
                   else paste(filename,"or",workname.Rnw,
                                   "exists. Do you want to replace it/them?"),
                   title="Save File",icon="warning",type="yesnocancel",default="yes")
        if("externalptr"==mode(res))  res<-tclvalue(res)
        if(res=="cancel") return(); if(res=="no") return() 
      }
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
      worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
      worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
      if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
         worktext<-c(worktext,"@\n\\end{document}")
      worktext<-TcltoWin.write(worktext)
      try.res <- try(cat(worktext,file=filename,sep="\n"))
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }



      Sys.sleep(0.25)
      file.copy(workname.sys,workname.Rnw,overwrite = TRUE)
      frage<-"add additional arguments for Sweave then return:"; set.tclvalue("tvinfo",sweave.args.sys)
      tkconfigure(linfo.tmp,text=frage)
      tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
      tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
      tkfocus(einfo.tmp)
      tkselection.range(einfo.tmp,"0","end") ## 051219
      tkbind(TopW,"<Escape>",function(){
          tkbind(TopW,"<Return>","")
          tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
          tkpack(linfo.name,linfo,side="left",fill="x")


        }
      )


      tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        set.tclvalue("tvmess","relax")
        sweave.args.sys<-tclvalue("tvinfo")
        assign("sweave.args.sys",sweave.args.sys,env=revive.sys)
        if(nchar(sweave.args.sys)!=0) {
          sweave.call<-paste("Sweave(\"",workname.Rnw,"\",",sweave.args.sys,")",sep="")
        } else sweave.call<-paste("Sweave(\"",workname.Rnw,"\")",sep="")
        try(eval(parse(text=sweave.call),envir=revive.env))
        Sys.sleep(0.5); LatexReport()
        cat("check R-window for messages!\n")
      } # end of function
    )
   } # end of SWEAVE
  SWEAVEB<-function(){ ###060112
      workname.Rnw<-sub("rev$","rnw",workname.sys)
      if(file.exists(filename<-file.path(getwd(),workname.sys))){
        res<-tkmessageBox(message=
                   if(language=="german") 
                      paste("Datei",workname.sys,"oder",workname.Rnw,
                                "existiert. Soll(en) sie ersetzt werden?")
                   else paste(filename,"or",workname.Rnw,
                                   "exists. Do you want to replace it/them?"),
                   title="Save File",icon="warning",type="yesnocancel",default="yes")
        if("externalptr"==mode(res))  res<-tclvalue(res)
        if(res=="cancel") return(); if(res=="no") return() 
      }
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
      worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
      worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
      if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
         worktext<-c(worktext,"@\n\\end{document}")
      worktext<-TcltoWin.write(worktext)
      try.res <- try(cat(worktext,file=filename,sep="\n"))
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }



      Sys.sleep(0.25)
      file.copy(workname.sys,workname.Rnw,overwrite = TRUE)
      frage<-"add additional arguments for Sweave then return:"; set.tclvalue("tvinfo",sweave.args.sys)
      tkconfigure(linfo.tmp,text=frage)
      tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
      tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
      tkfocus(einfo.tmp)
      tkselection.range(einfo.tmp,"0","end") ## 051219
      tkbind(TopW,"<Escape>",function(){
          tkbind(TopW,"<Return>","")
          tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
          tkpack(linfo.name,linfo,side="left",fill="x")


        }
      )


      tkbind(TopW,"<Return>", function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        set.tclvalue("tvmess","relax")
        sweave.args.sys<-tclvalue("tvinfo")
        assign("sweave.args.sys",sweave.args.sys,env=revive.sys)
        if(nchar(sweave.args.sys)!=0) {
          sweave.call<-paste("Sweave(\"",workname.Rnw,"\",",sweave.args.sys,")",sep="")
        } else sweave.call<-paste("Sweave(\"",workname.Rnw,"\")",sep="")
        infile<-tempfile("sweavein"); outfile<-tempfile("sweaveout")
        cat(file=infile,sweave.call,"\n")
     cmd<-paste(R.home(),"/bin/Rcmd BATCH ",infile," ",
                  outfile,sep="")
  if((version$os=="Win32" || version$os=="mingw32")
){
     shell(cmd)
  }else{
      system(cmd)
  }
        news<-readLines(outfile)
        if(!exists("toutwin"))
          toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
        pos.to.insert<-"end"
        news<-paste(gsub("\n+","\n",news),collapse="\n")
        try(tkinsert(toutwin,pos.to.insert,news))
        tksee(toutwin,"end - 0 lines")
        melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


        Sys.sleep(0.5); LatexReport()
        cat("check R-window for messages!\n")
      } # end of function
    )
   } # end of SWEAVEB
  } # end of wizard-if

  ConstructDemoFunction<-function(){
    melde("ConstructDemoFunction",1)
    if(!file.exists(workname.sys)){
      cat(paste("Error: File",workname.sys,"not found!!!"));return()
    }
    cat("Remark: processing without saving of",workname.sys,"\n")
    try(tangleR(workname.sys))
    workname<-sub(".rev$",".R",workname.sys)
    cat(workname,"generated\n")
    txt<-scan(workname,"",sep="\n")
    fh<-function(file,no=1,start=TRUE){
      # initial code of demo function
      require("tcltk"); where<-environment()
    }

    fh<-deparse(fh); fh<-fh[-length(fh)]
    ft<-function(){
      # end of code of demo function
      ## activate start chunk
      if(start==TRUE){
        no.0<-"0"
        no.start.0<-grep(paste("^#",no.0,":$",sep=""),chunks)
        no.end.0<-grep(paste("^#:",no.0,"$",sep=""),chunks)
        code.0<-chunks[no.start.0:no.end.0]
        eval(parse(text=code.0),envir=where)
      }
      ## activate chunk no
      # eval(parse(text=code),envir=where)
      secno<-tclVar("1") # 0
      show.next.number<-function(...){
       no<-as.character(as.numeric(tclvalue(secno))+1)
       no.start<-grep(paste("^#",no,":$",sep=""),chunks)
       no.end<-grep(paste("^#:",no,"$",sep=""),chunks)
       if(length(no.end)==0||is.na(no.end) ||is.na(no.start)||
            is.nan(no.end)||is.nan(no.start)){
            cat("# versuchte Chunk-Nummer falsch\n"); return()
       }
       ### cat("# aktueller chunk:",no,"\n")
       code<-paste(chunks[no.start:no.end],collapse="\n")
       if(0<length(code)) {
        tkdelete(ttext,"0.0","end")  
        tkinsert(ttext,"0.0",code)
        tclvalue(secno)<-as.character(no)
        }
      }
      show.back.number<-function(...){
       no<-as.character(as.numeric(tclvalue(secno))-1)
       no.start<-grep(paste("^#",no,":$",sep=""),chunks)
       no.end<-grep(paste("^#:",no,"$",sep=""),chunks)
       if(length(no.end)==0||is.na(no.end) ||is.na(no.start)||
            is.nan(no.end)||is.nan(no.start)){
            cat("# versuchte Chunk-Nummer falsch\n"); return()
       }
       ### cat("# aktueller chunk:",no,"\n")
       code<-paste(chunks[no.start:no.end],collapse="\n")
       if(0<length(code)) {
        tkdelete(ttext,"0.0","end")  
        tkinsert(ttext,"0.0",code)
        tclvalue(secno)<-as.character(no)
       }
      }
      show.number<-function(...){
       no<-as.character(as.numeric(tclvalue(secno)))
       no.start<-grep(paste("^#",no,":$",sep=""),chunks)
       no.end<-grep(paste("^#:",no,"$",sep=""),chunks)
       if(length(no.end)==0||is.na(no.end) ||is.na(no.start)||
            is.nan(no.end)||is.nan(no.start)){
            cat("# versuchte Chunk-Nummer falsch\n"); return()
       }
       ### cat("# aktueller chunk:",no,"\n")
       code<-paste(chunks[no.start:no.end],collapse="\n")
       if(0<length(code)) {
        tkdelete(ttext,"0.0","end")  
        tkinsert(ttext,"0.0",code)
        tclvalue(secno)<-as.character(no)
       }
      }
      eval.code<-function(...){
        code<-tclvalue(tkget(ttext,"0.0","end"))
        code.orig<-code<-unlist(strsplit(code,"\n"))
        code<-code[!substring(code,1,1)=="#"]
        code<-unlist(strsplit(code,";"))
        if(length(code)==0){ cat("ok\n"); return() }
        result<-try(eval(parse(text=code),envir=where))
        code.orig<-sub("#([0-9]+):","##wnt-Code-Chunk:\\1-begin#",code.orig)
        code.orig<-sub("#:([0-9]+)","##wnt-Code-Chunk:\\1-end#",code.orig)
        h<-get("allcodechunks",envir=where)
        h<-c(h,paste("<","<*>",">=",sep=""),code.orig,"\n@\n")
        assign("allcodechunks",h,envir=where)
        code<-sub("^ *","",code)
        code<-code[nchar(code)>0]
        lexpr<-rev(code)[1]; lexpr<-substring(lexpr,1,4)
        if(length(code)==0||is.null(lexpr)||is.na(lexpr)) return()
        plot.res<-c("plot","boxp","par(","abli","pie(","hist","axis","show",
               "lsfi","pair","ylab","help",
               "qqli","qqno","qqpl","rug(","lege","segm","text","xlab", 
               "poin","line","titl","eda(","imag","vgl.","curv")
        if(any(plot.res==lexpr)){
           cat("Plot erstellt\n"); return()
        }
        if(is.null(result)||is.na(result)||lexpr=="prin"||lexpr=="cat("){
          cat("ok\n"); return() }
        if(is.list(result)&& length(names(result))> 0 && 
                                   names(result)[1]=="ID") return()
        ## if(is.list(result)&& TRUE) return()
        no<-as.character(as.numeric(tclvalue(secno)))
        cat("Result of code chunk",no,":\n") 
       if(class(result)=="try-error"){
         class(result)<-"character"
         cat(result,"\n")
       }else{
        print(result)
       }
       cat("ok\n")
       }
      exit.function<-function(){
         tkdestroy(top)
         filename<-tkgetSaveFile(filetypes="{{Paper Files} {.rev}}",
                                               title="Do you want to save the activated R statements?")
         if(!is.character(filename)) filename<-tclvalue(filename)
         if(filename==""){
           cat("Demo function stopped without saving\n")
           return()
         }
         if(0==length(grep("rev$",filename))) filename<-paste(filename,".rev",sep="")
         h<-get("allcodechunks",envir=where)
         try(cat(h,sep="\n",file=filename))
         cat(paste("Remark: activated statements saved in\n   ",filename,"\n"))
         return()
      }
      allcodechunks<-paste(
              "@\nReport of activated R-chunks from: ",date(),
              "\n(demo function constructed by relax (c) Peter Wolf 2007)\n\n  ", sep="")
      no<-0
      no.start<-grep(paste("^#",no,":$",sep=""),chunks)
      no.end<-grep(paste("^#:",no,"$",sep=""),chunks)
      if(length(no.end)==0||is.na(no.end) ||is.na(no.start)||
           is.nan(no.end)||is.nan(no.start)){
           cat("# versuchte Chunk-Nummer falsch\n"); return()
      }
      ### cat("# aktueller chunk:",no,"\n")
      code<-paste(chunks[no.start:no.end],collapse="\n")
      h<-paste(rep("#",60),collapse="")
      code<-sub("#0:",h,code); code<-sub("#:0",h,code)
      allcodechunks<-c(allcodechunks,"\n@\n<<start>>=",code,"\n@")
      assign("allcodechunks",allcodechunks,envir=where)
      top<-tktoplevel()
      ttext<-tktext(top,height=19,background="#f7fffF",
                    font="-Adobe-courier-Medium-R-Normal--18-180-*")
      tf<-tkframe(top) 
      tkwm.title(top, "demo of file WoRkNaMe, constructed by relax (c) Peter Wolf 2007")
      tkpack(tf,ttext,side="bottom")
      tkevent.add("<<Paste>>",   "<Control_L><v>")
      tkbind(ttext,"<<Paste>> { catch {%W insert insert [selection get -selection CLIPBOARD] } }")

      bexit<-tkbutton(tf,text="EXIT",width=9)
      beval<-tkbutton(tf,text="ACTIVATE",width=9)
      bnext<-tkbutton(tf,text="NEXT",width=9)
      bback<-tkbutton(tf,text="BACK",width=9)
      lno  <-tkentry(tf,textvariable=secno,width=9)
      linfo<-tklabel(tf,text="chunk number:")
      tkpack(linfo,lno,beval,bnext,bback,bexit,side="left")
      tkconfigure(bexit,command=exit.function)
      tkconfigure(bnext,command=show.next.number)
      tkconfigure(bback,command=show.back.number)
      tkconfigure(beval,command=eval.code)
      # tkbind(lno,"<Return>",show.number)
      tkbind(lno,"<KeyRelease>",show.number)
      tclvalue(secno)<-as.character(no)
      show.number()
      ### tkwait.window(top)
    }

    ft<-deparse(ft)[-(1:2)]
    ft<-sub("WoRkNaMe",workname,ft)
    fname<-gsub("[^A-Za-z]","",sub(".R$",".demo",workname))
    txt<-gsub("\\\\","\\\\\\\\",txt)
    txt<-gsub("\"","\\\\\"",txt)
    txt<-paste(',"',txt,'"',sep=""); txt[1]<-substring(txt[1],2)
    txt<-c(paste(fname,"<-"),fh,"chunks<-c(",txt,")",ft
           ,paste("cat(\"Demo will be started by > ",fname,"()\\n\")",sep="")
           ,paste(fname,"()\n",sep="")
        )
    workname<-sub(".R$",".demo.R",workname)
    cat(txt,file=workname,sep="\n")
    cat("Demo in file ",workname," saved, load demo by source(\"",workname,"\") \n",sep="")
    melde("ConstructDemoFunction",2)
   }

  FindReportChunk<-function(){
    melde("FindReportChunk",1)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    no <- grep("^<<(.*)>>=",worktext); if(0==length(no)) return()

    choices<-paste(worktext[no],worktext[no+1],sep=": ")
    choices<-substring(choices,1,pmin(nchar(choices),60))
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    actual.chunk<-min(max(1,c(which(line<no),length(no)+1)[1]-1),length(no))

    newtop<-tktoplevel();tkwm.title(newtop,"code chunk? -- Esc=Exit, Return=Selection")
    scr <- tkscrollbar(newtop, command=function(...)tkyview(tl,...))
    tl<-tklistbox(newtop,height=min(20,length(no)),width=60,selectmode="single",
                yscrollcommand=function(...)tkset(scr,...),background="white")
    for(ch in choices) tkinsert(tl,"end",ch)
    tkselection.set(tl,actual.chunk-1)  # Default
    tksee(tl,max(actual.chunk-10,0))
    tkpack(tl,side="left",expand="yes",fill="y"); tkpack(scr,side="left",expand="yes",fill="y")
    tkbind(newtop,"<Escape>",function()tkdestroy(newtop))
    tkbind(newtop,"<Return>",function(){
       line<-no[as.numeric(tkcurselection(tl))+1]; tkdestroy(newtop)
       if(!is.na(line)){
         tksee(tworkwin,paste(line,".1",sep=""))
         tkmark.set(tworkwin, "insert", paste(line,".1",sep=""))
         tkfocus(tworkwin)
       }
    })
    melde("FindReportChunk",2)
  }

  InsertLaTeXEnv<-function(){
    melde("InsertLaTeXEnv",1)
    newtop<-tktoplevel();tkwm.title(newtop,"LaTeX-environment? -- Esc=Exit, Return=Selection")
    tkwm.geometry(newtop,"+0+15")
    scr <- tkscrollbar(newtop, command=function(...)tkyview(tl,...))
    tl<-tklistbox(newtop,height=7,width=70,selectmode="single",
                yscrollcommand=function(...)tkset(scr,...),background="white")
    choices<-c("center","quote","itemize","enumerate","eqnarray","verbatim")
    for(ch in choices) tkinsert(tl,"end",ch)
    tkpack(tl,side="left",expand="yes",fill="y"); tkpack(scr,side="left",expand="yes",fill="y")
    tkbind(newtop,"<Escape>",function()tkdestroy(newtop))
    tkbind(newtop,"<Return>",function(){
       choice<-as.numeric(tkcurselection(tl))+1; tkdestroy(newtop)
       if(!is.na(choice) && any(choice==1:6)){
         news<-c("\\begin{center}\n\n\\end{center}\n",
                        "\\begin{quote}\n\n\\end{quote}\n",
                        "\\begin{itemize}\n\\item\n\\item\n\n\\end{itemize}\n",
                        "\\begin{enumerate}\n\\item\n\\item\n\n\\end{enumerate}\n",
                        "\\begin{eqnarray*}\n\n\\end{eqnarray*}\n",
                        "\\begin{verbatim}\n\n\\end{verbatim}\n")[choice]
         line <-floor(as.numeric(tkindex(tworkwin,"insert")))

         line <- paste(line+1,"0",sep=".")
         if(!exists("tworkwin"))
           tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

         if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
         tworkwin<-get("tworkwin",envir=revive.sys)
         worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
         if(nchar(worktext)<10000){
           worktext<-strsplit(worktext,"\n")[[1]]
         }else{
           base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
           worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
         }

         try(tkinsert(tworkwin,line,paste(news,collapse="\n")))
         melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))


       }
       if(!is.na(line)){
         tksee(tworkwin,line)
         tkmark.set(tworkwin, "insert", line)
         tkfocus(tworkwin)
       }
    })
    melde("InsertLaTeXEnv",2)
  }
  FindLaTeXSection<-function(){
    melde("FindLaTeXSection",1)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    no <- grep("\\\\s(.*)ection",worktext); if(0==length(no)) return()

    choices<-paste(worktext[no],worktext[no+1],sep=": ")
    choices<-substring(choices,1,pmin(nchar(choices),60))
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    actual.chunk<-min(max(1,c(which(line<no),length(no)+1)[1]-1),length(no))

    newtop<-tktoplevel();tkwm.title(newtop,"code chunk? -- Esc=Exit, Return=Selection")
    scr <- tkscrollbar(newtop, command=function(...)tkyview(tl,...))
    tl<-tklistbox(newtop,height=min(20,length(no)),width=60,selectmode="single",
                yscrollcommand=function(...)tkset(scr,...),background="white")
    for(ch in choices) tkinsert(tl,"end",ch)
    tkselection.set(tl,actual.chunk-1)  # Default
    tksee(tl,max(actual.chunk-10,0))
    tkpack(tl,side="left",expand="yes",fill="y"); tkpack(scr,side="left",expand="yes",fill="y")
    tkbind(newtop,"<Escape>",function()tkdestroy(newtop))
    tkbind(newtop,"<Return>",function(){
       line<-no[as.numeric(tkcurselection(tl))+1]; tkdestroy(newtop)
       if(!is.na(line)){
         tksee(tworkwin,paste(line,".1",sep=""))
         tkmark.set(tworkwin, "insert", paste(line,".1",sep=""))
         tkfocus(tworkwin)
       }
    })
    melde("FindLaTeXSection",2)
  }

  ShowAboutRelax<-function(){
    melde("ShowAboutRelax",1)
    doc<-c("relax() is designed to support the process of data analysis, report writing,",
                 "presentation, and programming. On start it creates a new window for writing",
                 "code and text at the same time. You are allowed to evaluate R code chunks",
                 "as well as to write down interpretations of the results.",
                 "This style of working will result in correct reports.",
                 "The reports can be reloaded again in cause of presentation, modification",
                 "and checking of the results. Have a look at the help page of relax and",
  #              "http://www2.wiwi.uni-bielefeld.de/~wolf/software/relax/relax.html","",
                 "http://www.wiwi.uni-bielefeld.de/statistik/mitarbeiter/wolf/software/relax.html","",
                 "Copyright (C) 2005 Hans Peter Wolf.","",
                 "This software is licensed under the GPL (version 2 or newer) terms.",
                 "It is free software and comes with ABSOLUTELY NO WARRANTY.","",
                 "The windows version may use some ingredients",
                # "of the noweb system",
                # " (Norman Ramsey -- http://www.eecs.harvard.edu/~nr/noweb/intro.html),",
                 "of the Img package of Jan Nijtmans (see:",
                 "http://wiki.tcl.tk/1404,",
                 "http://home.kpnplanet.nl/~J.Nijtmans@kpnplanet.nl/img.html",
                 "and http://members.chello.nl/~j.nijtmans/img.html",
                 "the package is found in http://sourceforge.net/projects/tkimg).",
                # "and gawk (http://www.gnu.org/software/gawk/gawk.html).",
                 "If used the license terms concerning the img package are found in the source", 
                 "file of the package, see: relax/src/tkimg1.3.tar.gz.","",
                 "---------------------------------------------------------","",""
                 )
    try(doc<-c(doc,scan(file=paste(.path.package("relax"),"lib/gpl.txt",sep="/"),what="",sep="\n")))
    .newl<-tktoplevel();tkwm.geometry(.newl,"+0+15")
    tkpack(tt<-tktext(.newl,height=length(doc)))
    tkwm.title(.newl,paste("What's relax? ","Exit by Return or Escape!"))
    try(tkinsert(tt,"0.0",paste(doc,collapse="\n")))
    abbruch<-function(){tkdestroy(.newl); set.tclvalue("tvscandone",2)}
    tkbind(.newl,"<Escape>", function(){
                 tkdestroy(.newl)
    })
    tkbind(.newl,"<Return>", function(){
                 tkdestroy(.newl)
    })
    tkfocus(.newl)
    melde("ShowAboutRelax",2)
  }

  ShowShortCuts<-function(){
    melde("ShowShortCuts",1)
    keys<-c("Shortcuts of relax:","",
                  "Alt-D:  move cursor one code chunk DOWN",
                  "Alt-U:  move cursor one code chunk UP",
                  "Alt-P:  plan new code chunk",
                  "Alt-E:  eval code chunk",
                  "Alt-T:  delete output of text field",
                  "Alt-S:  copy plot and generate postscript / JPEG file",
                  "Alt-R:  clear output field",
                  "Alt-H:  R help",
                  "Crtl-F:  Find text in report field",
                  "<F3>:  Find text in report field again",
                  "Crtl-S:  Save Report",
                  "Crtl-P:  Process Report: save, weave, and latex Report",
          #       "Crtl-Page-down:  copy code chunk to end", -- abgeschaltet
                  "","Shortcuts of tcltk, may depend on tcl/tk version:","",
                  "Crtl-Z:  UNDO",
                  "Crtl-C:  copy marked text",
                  "Crtl-V:  paste marked text",
                  "Crtl-Y:  paste marked text (=Crtl-V)",
                  "Crtl-X:  delete marked text",
                  "Crtl-W:  copy and delete marked text",
                  "Crtl-Shift-7:  mark all",
                 # "Crtl-F:  unmark text",
                 # "Crtl-A:  unmark text",
                 # "Crtl-P:  unmark text",
                  "Crtl-O:  break line",
                  "Crtl-E:  move cursor to the end of line",
                  "Crtl-A:  move cursor to the beginning of line",
                  "Crtl-B:  move cursor one character to the left",
                  "Crtl-F:  move cursor one character to the right",
                  "Crtl-N:  move cursor to the next line",
                  "Crtl-Pos1:  move cursor to the beginning of text",
                  "Crtl-End:  move cursor to the end of text",
                 # "Crtl-P:  move cursor one line back",
                  "Crtl-I:  insert tab",
                  "Crtl-T:  exchange characters",
                  "Crtl-D:  delete character right of cursor",
                  "Crtl-H:  delete character left of cursor",
                  "Crtl-K:  delete characters from cursor to end of line"     )
    .newl<-tktoplevel();tkwm.geometry(.newl,"+0+15")
    tkpack(tt<-tktext(.newl,height=length(keys)))
    tkwm.title(.newl,paste("shortcuts of relax and text field,","Exit by Return or Escape!"))
    try(tkinsert(tt,"0.0",paste(keys,collapse="\n")))
    abbruch<-function(){tkdestroy(.newl); set.tclvalue("tvscandone",2)}
    tkbind(.newl,"<Escape>", function(){
                 tkdestroy(.newl)
    })
    tkbind(.newl,"<Return>", function(){
                 tkdestroy(.newl)
    })
    tkfocus(.newl)
    melde("ShowShortCuts",2)
  }



  FindRFns<-function(){
    melde("FindRFns",1)
    frage<-"keyword for function search?"; set.tclvalue("tvinfo",string.sys)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        such<-tclvalue("tvinfo")
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        set.tclvalue("tvmess",paste(tclvalue("tvinfo"),": bitte etwas Geduld!"))
        found<-help.search(such)[[4]][,1:2]
        found<-paste(paste(found[,1],
                           substring("         ",1,pmax(1,10-nchar(found[,1]))),
                           found[,2]),collapse="\n")
        .newl<-tktoplevel();tkwm.geometry(.newl,"+0+15");tkpack(tt<-tktext(.newl))
        tkwm.title(.newl,paste("zu >",such,"< gefundene Funktionen, ",
                               "Beendigung durch Escape oder Return!"))
        try(tkinsert(tt,"0.0",paste(found,collapse="\n")))
        abbruch<-function(){tkdestroy(.newl);set.tclvalue("tvndone",2)}
        tkbind(.newl,"<Return>",abbruch);tkbind(.newl,"<Escape>",abbruch)
        tkfocus(.newl);tkwait.variable("tvndone")
        set.tclvalue("tvmess","relax")
      } # end of function
    )
    melde("FindRFns",2)
  }

  ReloadPlots<-function(){
    melde("ReloadPlots",1)
    if(!no.plots) { exclude.plots(tworkwin); createandshow.all.plots(tworkwin) }

    melde("ReloadPlots",2)
  }

  ReloadReportWidget<-function(){
    melde("ReloadReportWidget",1)
    revive.sys<-get("revive.sys",env=revive.env)
    tworkwin<-get("tworkwin",env=revive.sys); fworkwin<-get("fworkwin",env=revive.sys)
    del<-0.02; tkpack.forget(tworkwin); Sys.sleep(del)
    tkpack(tworkwin,expand="yes",fill="both"); Sys.sleep(del)
    .Tcl(paste("place ",fworkwin$ID," -relheight 0.35")); Sys.sleep(del)
    .Tcl(paste("place ",fworkwin$ID," -relheight 0.50"))
    melde("ReloadReportWidget",2)
  }

  FormatTeXLines<-function(){
    melde("FormatTeXLines",1)
    # ermittle neue Zeilen:
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    text.start<-h<-grep("^@",c(worktext,"@"))
    texbegin<-rev(text.start[text.start<line])[1]; if(is.na(texbegin)) return()
    texend<-text.start[text.start>line][1]
    code.start<-grep("^<<(.*)>>=",worktext)
    c.start<-rev(code.start[code.start<line])[1]
    if(!is.na(c.start) &&  c.start>texbegin ) return()
    c.end<-code.start[code.start>line][1];  if(!is.na(c.end))texend<-min(texend,c.end)
    ttext<-worktext[(texbegin+1):(texend-1)]
    # formatiere Zeilen
    ttext<-c("\\documentclass{article}\\pagestyle{empty}\\parindent0mm",
             "\\begin{document}\\LARGE",ttext,"\\end{document}")
    cat(ttext,file="tmptmp.tex",sep="\n")
    # erstelle jpg-bild
    h<-gsub(" ","",gsub(":","",date())); h<-substring(h,4:nchar(h),4:nchar(h))
    jpgname<-paste(c("t",h[6:11],h[4:5],h[1:2],h[14:15],".jpg"),collapse="")
    if(substring(version$os,1,5)=="linux"
){
      system("echo q | latex tmptmp.tex; dvips -E tmptmp.dvi")
      system(paste("convert tmptmp.ps ",jpgname))
      system("rm tmptmp.tex tmptmp.aux tmptmp.log tmptmp.dvi tmptmp.ps")
    }
    if((version$os=="Win32" || version$os=="mingw32")
){
      if(!(exists("ghostscript")&&0<nchar(ghostscript)&&0<length(grep("[A-Za-z]",ghostscript)))) 
         return()
         if(!file.exists(paste(ghostscript,".exe",sep=""))){
            cat("ERROR: sorry, ghostscript not found!!!\n"); return()
         }
         ft.path<-file.path(relax.path,"lib")
         ft.path<-ft.path[file.exists(ft.path)][1]
         try(shell("echo q | latex tmptmp.tex"))
         try(shell("dvips -E tmptmp.dvi"))
         try(shell(paste(ghostscript,
    " -dBATCH -sDEVICE=ppm -quit -sOutputFile=tmptmp.ppm -dNOPAUSE tmptmp.ps", sep="")))
         try(shell(file.path(ft.path,"\\pnmcrop   tmptmp.ppm   > tmptmp.crp")))
         try(shell(paste(ft.path,"\\ppmtojpeg tmptmp.crp   > ",jpgname,  sep="")))
         try(shell("del tmptmp.tex tmptmp.aux tmptmp.log tmptmp.ppm tmptmp.crp tmptmp.dvi tmptmp.ps"))
    }
    # passe workttext an, integriere Bild
    insertline<-texend+3
    news<-paste("\n%<!--latex-end-->\n\n% <p><img src=\"",jpgname,"\">\n",sep="")
    line<-paste(texend,".0",sep="")
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    try(tkinsert(tworkwin,line,paste(news,collapse="\n")))
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    line<-paste(texbegin+1,".0",sep=""); news<-"%<!--latex-begin--\n"
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    try(tkinsert(tworkwin,line,paste(news,collapse="\n")))
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    ##zeige Bild im Textfenster an##
    no.plots<-get("no.plots",envir=revive.sys) #081125
    if(!no.plots) createandshow.single.plot(tworkwin,insertline,jpgname)
    ## old: show.single.plot(tworkwin,line,jpgname) 
    melde("FormatTeXLines",2)
  }

  DumpCodeChunk<-function(){
    melde("DumpCodeChunk",1)
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    code.start<-grep("^<<(.*)>>=",worktext)
    if(0==length(code.start)) return()
    code.start<-code.start[code.start<=line]
    if(0<length(code.start)){
      if((code.start<-max(code.start)+1)>length(worktext)) return()
      code.end  <-c(grep("^@",worktext),1+length(worktext))
      code.end  <-min(code.end[code.end>code.start])-1
      code<-worktext[code.start:code.end]
      code<-code[code!=""]
      if(length(weg.ab<-grep("^<<(.*)>>=",code))>0) code<-code[-(weg.ab:length(code))]
      if(length(code)==0 || code[1]=="@") code<-" "
      melde("code:",3,code,"\n")

    }
    if(0<length(code)){
      melde("vor tangle - Code:\n",3,code)
      if(length(grep("<<(.*)>>",code))>0 || length(grep(">#",code))>0){
        code.a    <- grep("^<<(.*)>>=",worktext)
        code.z    <- grep("^@",worktext)
        code.z    <- unlist(sapply(code.a ,function(x,y) y[y>x][1], code.z))
        if(any(h<-is.na(code.z))) code.z<-code.z[!h]
        ################
        # code.n    <- length(worktext)
        # change    <- rep(0,code.n); change[c(code.a ,code.z)]<-1
        # code.ch   <- worktext[1==(cumsum(change)%%2)]
        ## 080311
        ind<-rep(0,length(worktext)); copy<-0
        for(i in seq(ind)){
          if(i %in% code.z) copy<-0
          if(i %in% code.a) copy<-1
          ind[i]<-copy
        }
        code.ch   <- worktext[ind==1]
        ## cat("input-text, Anfaenge, Code.chunks"); print(worktext); print(code.a); print(code.ch)

        code.n    <- length(code.ch)

        code.ch<-gsub("@>>","DoSpCloseKl-esc",gsub("@<<","DoSpOpenKl-esc",code.ch))

        code.ch<-gsub("(.*)<<(.*)>>=(.*)","cOdEdEf\\2",code.ch)
        repeat{
          if(0==length(cand<-grep("<<(.*)>>",code.ch))) break
          code.ch<-unlist(strsplit(gsub("(.*)<<(.*)>>(.*)",
                     "\\1bReAkuSeChUnK\\2bReAk\\3",code.ch),"bReAk"))
        }
        code.ch<-code.ch[code.ch!=""]
        code.n<-length(code.ch)
        melde("code.ch:",3,code.ch,"\n")

        line.typ  <-rep("C",code.n)
        code.a    <-grep("cOdEdEf",code.ch)
        code.ch[code.a]<-substring(code.ch[code.a],8)
        line.typ[code.a]<-"D"
        code.use    <-grep("uSeChUnK",code.ch)
        code.ch[code.use]<-substring(code.ch[code.use],9)
        line.typ[code.use]<-"U"
        code.ext  <-grep("#<file",code.ch)
        line.typ[code.ext]<-"E"
        melde("code.ch:",3,code.ch,"\n")

        code.out<-"##act:##"

        def.names<-code.ch[code.a]
        use.names<- if(length(code.use)>0) code.ch[code.use] else NULL
        code.z<-c(if(length(code.a)>1) code.a[-1]-1, code.n)
        code.ch<-paste(line.typ,code.ch,sep="")
        melde("code.ch:",3,code.ch,"\n")

        melde("vor expand - Code:\n",3,code)
        melde("bearbeite aktuellen Chunk\n",3)
        line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        ch.no<-length(grep("^<<(.*)>>=",worktext[1:line]))

        rows      <-c((code.a[ch.no]+1),code.z[ch.no])
        if(all(!is.na(rows))&&rows[1]<=rows[2]){
          rows<-rows[1]:rows[2]
          code.stack<-code.ch[rows]
          max.depth.refinements<-500; i<-1
          repeat{
             if((i<-i+1)>max.depth.refinements){ 
                 cat("ERROR: maximal number of expandations (",max.depth.refinements,
                     ") exceeded\n --- perhaps a unintended recursion ???")
                 return()
             }
             if(0==length(code.stack))break
             typ<-substring(code.stack[1],1,1)
             if("C"==typ||"E"==typ){
               n.lines<-sum(cumprod("C"==substring(code.stack,1,1)))
               code.out<-c(code.out, substring(code.stack[1:n.lines],2))
               code.stack<-code.stack[-(1:n.lines)]
             }
             if(length(code.stack)>0 && "U"==substring(code.stack[1],1,1)){
               if(any(found<-def.names==substring(code.stack[1],2))){
                 found<-seq(along=def.names)[found]; rows<-NULL
                 for(no in found){
                   if((code.a[no]+1)<=code.z[no]) rows<-c(rows,(code.a[no]+1):code.z[no])
                 }
                 code.stack<-c(code.ch[rows],code.stack[-1])
                 melde("found",0,found)
               }else{code.stack<-code.stack[-1]}
             }

          }
        }
        if(length(code.ext)>0){
          code.out<-code.out[code.out!=""]
          code.ext<-rev(grep(">#",code.out))
          found<-TRUE
          repeat{
            if(length(code.ext)==0) break

            if(!found){
              code.out[code.ext[1]]<-paste("# ??",code.out[code.ext[1]])
              cat("ERROR: External Chunk",code.out[code.ext[1]],"not found!!!\n")
              code.ext<-code.ext[-1]
            }

            found<-TRUE
            ext.name <- rev(unlist(strsplit(code.out[code.ext[1]],"#<file:")))[1]
            ext.name <- unlist(strsplit(unlist(strsplit(ext.name,">#"))[1],":"))
            ext.chunk<-ext.name[2]; ext.name <-ext.name[1]
            ext.name.n<-nchar(ext.name)
            if(ext.name.n >4 && ".rev"==substring(ext.name,ext.name.n-3,ext.name.n)){
              ext.name<-substring(ext.name,1,ext.name.n-4)
            }

            if(is.na(as.numeric(ext.chunk))){
              # tld untersuchen
              filename<-paste(ext.name[1],".rev",sep="")
              if(!file.exists(filename)){
                cat("ERROR: file",filename,"for expansion of code chunk not found!!!\n")
                ext.file<-"Error"
              }else{
                ext.file<-try(myscan(file=filename,what="",sep="\n"))
              }
              if("Error"==substring(unlist(ext.file)[1],1,5)){
                found <-FALSE; next
              }
              ext.file <-ext.file[grep("^<<(.*)>>=",ext.file)]
              if(!is.null(ext.file)){ found<-FALSE; next }
              ext.chunk<-grep(ext.chunk,ext.file)
            }

            filename<-paste(ext.name[1],".R",sep="")
            if(!file.exists(filename)){
              cat("Warning: file",filename,"not found!!!\n")
              cat("         file",filename,"is now generated!!!\n")
              try(tangleR(ext.name[1]))
            }
            if(!file.exists(filename)){
              ext.file<-"Error"
            }else{
              ext.file<-try(myscan(file=filename,what="",sep="\n"))
            }
            if("Error"==substring(unlist(ext.file)[1],1,5)){
              found <-FALSE; next
            }

            ext.chunk<-as.numeric(ext.chunk)
            a        <-grep(paste("#", ext.chunk,":",sep=""),ext.file)[1]
            z        <-grep(paste("#:",ext.chunk,    sep=""),ext.file)[1]
            if(is.na(a)){
              found <- FALSE; next
            }
            if(a<=z) ext.file <-ext.file[a:z]

            code.out <-c(code.out[1:(code.ext[1]-1)], ext.file,
                         if(length(code.out)>(code.ext[1]+1))
                           code.out[(code.ext[1]+1):length(code.out)]
                       )

            code.ext<-code.ext[-1]

          }

        }
        code.out<-c(code.out,"##:act##")

        code.out<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",code.out))

        melde("Ende Rtangle-last\n",3)
        code<-code.out[code.out!=""]
        melde("nach expand\n",3,code)
      }

    } else { cat("ERROR: no code found!!!\n"); return() }
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.R}}",
                            title="name of file to save code chunk?", initialdir=getwd(),
                            defaultextension=".R", initialfile="RCode-act.R")
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    try.res <- try(cat(code,file=filename,sep="\n"))
  }

  CopyToEnd<-function(){
    melde("CopyToEnd",1)
    news<-tclvalue(tkget(toutwin,"0.0","end"))
    if(1<nchar(news)){
      news<-paste("\n@","output-start",news,"output-end\n",sep="\n")
      news<-gsub("\n+","\n",news)
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      pos.to.insert<-"end"
      if(0<length(grep("output-start",news))){
        tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
        ltail<-length(tail)
        if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
           any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
           news<-sub(".*output-start\n","",news)
           news<-sub("output-end","",news)
           h<-seq(along=h)[h][1]
           pos.to.insert<-paste("end -",h,"lines")
        }
      }
      try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
      tksee(tworkwin,"end - 0 lines")
      melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      worktext<-""
      if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
      tkdelete(toutwin,"0.0","end")
      try(tkinsert(toutwin,"0.0",paste(worktext,collapse="\n")))
    }
    melde("CopyToEnd",2)
  }

  SetWorkPath<-function(){
    melde("SetWorkPath",1)
    path<-tkchooseDirectory(title="Reportverzeichniswahl",initialdir=getwd())
    if(!is.character(path)) path<-tclvalue(path)
    if(path=="") return() else try.res <- try(setwd(path))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      cat("new directory:",getwd(),"\n")
    }else{
      cat("ERROR: change of directory failed!!!\n")
    }
    melde("SetWorkPath",2)
  }

  DumpEnvironment<-function(){
    melde("DumpEnvironment",1)
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.relax-env}}",
                            title="name of ENVIRONMENT DUMP file?", initialdir=getwd(),
                            defaultextension=".relax-env", 
                            initialfile=sub("rev$","relax-env",workname.sys))
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    noenv<-TRUE # ignore environments 
    objlist<-ls(env=revive.env) 
    if(noenv) objlist<-objlist[sapply(objlist,function(x)!is.environment(get(x,env=revive.env)))]
    objlist<-objlist[substring(objlist,1,6)!="revive"]
    if(length(objlist)==0){
      cat("WARNING: no objects found for saving!!!\n")
      return()
    }
    cat("list of objects:\n"); print(objlist); Sys.sleep(.5)
      

    assign("list.of.env.obj",objlist,env=revive.env)
    try.res<-try(eval(parse(text=
                    paste(sep="","dump(list.of.env.obj,file=\"",filename,"\")")),env=revive.env))
    try(eval(parse(text="remove(\"list.of.env.obj\")"),env=revive.env))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      cat(paste("Objects of environment saved in file:",filename,"!\n"))
    } else {
      cat("ERROR: saving of environment objects failed!!!\n")
    }
    melde("DumpEnvironment",2)
  }
  SaveEnvironment<-function(){
    melde("SaveEnvironment",1)
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.bin-relax-env}}",
                            title="name of BINARY SAVE file?", initialdir=getwd(),
                            defaultextension=".bin-relax-env", 
                            initialfile=sub("rev$","bin-relax-env",workname.sys))
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    noenv<-FALSE # do not ignore environments 
    objlist<-ls(env=revive.env) 
    if(noenv) objlist<-objlist[sapply(objlist,function(x)!is.environment(get(x,env=revive.env)))]
    objlist<-objlist[substring(objlist,1,6)!="revive"]
    if(length(objlist)==0){
      cat("WARNING: no objects found for saving!!!\n")
      return()
    }
    cat("list of objects:\n"); print(objlist); Sys.sleep(.5)
      

    assign("list.of.env.obj",objlist,env=revive.env)
    try.res<-try(eval(parse(text=
                    paste(sep="","save(list=list.of.env.obj,file=\"",filename,"\")")),env=revive.env))
    try(eval(parse(text="remove(\"list.of.env.obj\")"),env=revive.env))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      cat(paste("Objects of environment saved in file:",filename,"!\n"))
    } else {
      cat("ERROR: saving of environment objects failed!!!\n")
    }
    melde("SaveEnvironment",2)
  }
  LoadEnvironment<-function(){
    melde("LoadEnvironment",1)
    initialfile<-sub("rev$","bin-relax-env",workname.sys)
    if(!file.exists(initialfile)) initialfile<-sub("rev$","relax-env",workname.sys)
    filename<-tkgetOpenFile(filetypes="{{Paper Files} {.relax-env .bin-relax-env}}",
                            title="Select *relax-env file to be loaded!",
                            defaultextension=".relax-env",
                            initialfile=initialfile,
                            initialdir=getwd())
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    if(0<length(grep("bin-relax-env$",filename))){
      try.res<-try(load(filename,env=revive.env))
    } else {
      obj<-scan(filename,""); ind<-grep("<-",obj)-1   
      obj[ind]<-paste(";",obj[ind]); obj<-sub("^;","",paste(obj,collapse=""))
      try.res<-try(eval(parse(text=obj),env=revive.env))
    }
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      cat(paste("Objects of file:",filename,"\nloaded into environment!\n"))
      noenv<-FALSE # do not ignore environments 
      objlist<-ls(env=revive.env) 
      if(noenv) objlist<-objlist[sapply(objlist,function(x)!is.environment(get(x,env=revive.env)))]
      objlist<-objlist[substring(objlist,1,6)!="revive"]
      if(length(objlist)==0){
        cat("WARNING: no objects found for saving!!!\n")
        return()
      }
      cat("list of objects:\n"); print(objlist); Sys.sleep(.5)
        

    } else {
      cat("ERROR: saving of environment objects failed!!!\n")
    }
    melde("LoadEnvironment",2)
  }
  CleanEnvironment<-function(){
    melde("CleanEnvironment",1)
    noenv<-FALSE # do not ignore environments 
    objlist<-ls(env=revive.env) 
    if(noenv) objlist<-objlist[sapply(objlist,function(x)!is.environment(get(x,env=revive.env)))]
    objlist<-objlist[substring(objlist,1,6)!="revive"]
    if(length(objlist)==0){
      cat("WARNING: no objects found for saving!!!\n")
      return()
    }
    cat("list of objects:\n"); print(objlist); Sys.sleep(.5)
      

    res<-tkmessageBox(message=
        if(language=="german") 
          "Sollen die Objekte der Umgebung wirklich entfernt werden"
        else 
          "Do you want to delete all the objects of the environment?",
        title="Clean Environment?",icon="warning",type="yesnocancel",default="no")
    res<-tclvalue(res)
    if(res=="yes"){    
      objlist<-c(objlist,"objlist"); assign("objlist",objlist,env=revive.env)
      try.res<-try(eval(parse(text="remove(list=objlist)"),env=revive.env))
      cat("Objects of environment have been deleted!\n")
      # print(try.res)
    }
    melde("CleanEnvironment",2)
  }

  SaveReport<-function(){
    melde("SaveReport",1)
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.rev}}",
                            title="name of report file?", initialdir=getwd(),
                            defaultextension=".rev", initialfile=workname.sys)
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
    worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
    worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
    if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
       worktext<-c(worktext,"@\n\\end{document}")
    worktext<-TcltoWin.write(worktext)
    try.res <- try(cat(worktext,file=filename,sep="\n"))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }



    if(ok){
      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      olddir<-getwd(); newdir<-sub("(.*)/(.*)","\\1",filename)
      if(olddir!=newdir){
        rev<-grep("includegraphics",worktext,value=TRUE)
        if(0<length(rev)){
          # pics<-sub("(.*)\\{(p.*\\.ps)\\}(.*)","\\2",rev) #081121
          pics<-sub("(.*)\\{(p.*)\\}(.*)","\\2.ps",rev); pics<-sub("\\.ps\\.ps$",".ps",pics)
          pics<-c(pics,sub("ps$","jpg",pics))
          mess<-NULL
          for(pic in pics) {
            if(file.exists(pic)){
              a<-file.copy(pic,paste(newdir,pic,sep="/"))
              mess<-c(mess,if(a) paste("picture",pic,"copied to",newdir) else
                                              paste("warning: picture", pic,"not copied"))
            }
          }
          cat(mess,sep="\n")
          "process of copying file(s) finished"
        }
      }


      h <- strsplit(filename,.Platform$file.sep)[[1]]
      workname.sys<-rev(h)[1]
      workpath.sys<-paste(h[-length(h)],collapse=.Platform$file.sep)
      setwd(workpath.sys)

      melde(paste("report file",filename,"saved\n"),0)
      melde(paste("w",workname.sys),"cmd.msg")
    } else {
      cat("ERROR: write operation failed!!!\n")
    }
    melde("SaveReport",2)
    return("ok")
  }
  SaveHtml<-function(){
    melde("SaveHtml",1)
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.rev}}",
                            title="name of report file?", initialdir=getwd(),
                            defaultextension=".rev", initialfile=workname.sys)
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
    worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
    worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
    if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
       worktext<-c(worktext,"@\n\\end{document}")
    worktext<-TcltoWin.write(worktext)
    try.res <- try(cat(worktext,file=filename,sep="\n"))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }



    if(ok){
      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      olddir<-getwd(); newdir<-sub("(.*)/(.*)","\\1",filename)
      if(olddir!=newdir){
        rev<-grep("includegraphics",worktext,value=TRUE)
        if(0<length(rev)){
          # pics<-sub("(.*)\\{(p.*\\.ps)\\}(.*)","\\2",rev) #081121
          pics<-sub("(.*)\\{(p.*)\\}(.*)","\\2.ps",rev); pics<-sub("\\.ps\\.ps$",".ps",pics)
          pics<-c(pics,sub("ps$","jpg",pics))
          mess<-NULL
          for(pic in pics) {
            if(file.exists(pic)){
              a<-file.copy(pic,paste(newdir,pic,sep="/"))
              mess<-c(mess,if(a) paste("picture",pic,"copied to",newdir) else
                                              paste("warning: picture", pic,"not copied"))
            }
          }
          cat(mess,sep="\n")
          "process of copying file(s) finished"
        }
      }


      h <- strsplit(filename,.Platform$file.sep)[[1]]
      workname.sys<-rev(h)[1]
      workpath.sys<-paste(h[-length(h)],collapse=.Platform$file.sep)
      setwd(workpath.sys)

      melde(paste("report file",filename,"saved\n"),0)
      melde(paste("w",workname.sys),"cmd.msg")
    } else {
      cat("ERROR: write operation failed!!!\n")
    }
    {
      melde("weaveRhtml",1)
      weaveRhtml(workname.sys,
                         replace.umlaute=replace.umlaute.sys)
      melde("weaveRhtml",2)
    }
    melde("SaveHtml",2)
    return("ok")
  }
  SaveAsTextFile<-function(){
    melde("SaveAsTextFile",1)
    filename<-tkgetSaveFile(filetypes="{{TEXT-FILE!} {*.*}}",
                            title="name of file?", initialdir=getwd(),
                            defaultextension="", initialfile=sub(".rev$",".txt",workname.sys))
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    worktext<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext)
    worktext<-sub("^output-start","\\\\begin{verbatim}",worktext)
    worktext<-sub("^output-end","\\\\end{verbatim}",worktext)
    if(0==length(grep("\\\\end\\{document\\}",worktext))) #2.1.0
    worktext<-c(worktext,"@\n\\end{document}")
    ## remove noweb items
    code.ch<-c("@",worktext,"@")
    code.chunk.start<-paste("^<","<.*>",">=",sep="")
    code.a<- grep(code.chunk.start,code.ch)
    if(0<length(code.a)){
      code.z<-grep("^@",code.ch)
      code.z<-unlist(sapply(code.a ,function(x,y)min(y[y>x]),code.z))
      code.n<-length(code.ch)
      change<-rep(0,code.n); change[c(code.a ,code.z)]<-1
      ind<-(1==(cumsum(change)%%2))[-length(change)]
      def.lines<-diff(c(FALSE,ind))>0
      code.ch<-code.ch[!def.lines]; ind<-ind[!def.lines]
      code.ch[ind]<-paste("+",code.ch[ind])
      code.ch[ind]<-sub("^[+][ \t]*$"," ",code.ch[ind])
      ind<-diff(c(FALSE,ind))>0
      code.ch[ind]<-paste(">",substring(code.ch[ind],3))
    #  code.ch[ind]<-paste("+",code.ch[ind])
    #  ind<-diff(c(FALSE,ind))>0
    #  code.ch[ind]<-sub("^[+] ","> ",code.ch[ind])
    } else { "no code" }
    ind<-grep("^@",code.ch)
    worktext<-code.ch[-ind]
    worktext<-TcltoWin.write(worktext)
    try.res <- try(cat(worktext,file=filename,sep="\n"))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    # und ein script.R-file
    a<-worktext; ind<-grep("^[+>]",a)
    if(0!=length(ind)){
      a<-a[ind]; ind<-grep("^[>]",a); no<-seq(ind)
      a[ind]<-paste("#",no,": >\n ",substring(a[ind],2),sep="")
      ind<-grep("^[+]",a)
      if(0!=length(ind)) a[ind]<-paste("", substring(a[ind],2))
      filename<-sub(".txt$",".R",filename)
      try.res <- try(cat(a,file=filename,sep="\n"))
    }

    if(ok){
      filename<-paste(sub(".txt$","",filename),".rev",sep="")
      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      olddir<-getwd(); newdir<-sub("(.*)/(.*)","\\1",filename)
      if(olddir!=newdir){
        rev<-grep("includegraphics",worktext,value=TRUE)
        if(0<length(rev)){
          # pics<-sub("(.*)\\{(p.*\\.ps)\\}(.*)","\\2",rev) #081121
          pics<-sub("(.*)\\{(p.*)\\}(.*)","\\2.ps",rev); pics<-sub("\\.ps\\.ps$",".ps",pics)
          pics<-c(pics,sub("ps$","jpg",pics))
          mess<-NULL
          for(pic in pics) {
            if(file.exists(pic)){
              a<-file.copy(pic,paste(newdir,pic,sep="/"))
              mess<-c(mess,if(a) paste("picture",pic,"copied to",newdir) else
                                              paste("warning: picture", pic,"not copied"))
            }
          }
          cat(mess,sep="\n")
          "process of copying file(s) finished"
        }
      }


      h <- strsplit(filename,.Platform$file.sep)[[1]]
      workname.sys<-rev(h)[1]
      workpath.sys<-paste(h[-length(h)],collapse=.Platform$file.sep)
      setwd(workpath.sys)

      melde(paste("file",filename,"saved\n"),0)
    } else {
      cat("ERROR: write operation failed!!!\n")
    }
    melde("SaveAsTextFile",2)
    return("ok")
  }

  # ermittle neue Zeilen:
  SaveDiffReport<-function(){
    melde("SaveDiffReport",1)
    filename<-tkgetSaveFile(filetypes="{{Paper Files} {.html}}",
                            title="Diff-File-Name?", initialdir=getwd(),
                            defaultextension=".html", initialfile=
                paste("diff",workname.sys,sep="-"))
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    try.res <-try(cat(worktext,file=get("tmp.file.name",env=revive.sys)
,sep="\n"))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(!ok) return()
    # stelle Differenzen fest
    system(paste("diff ",workname.sys," ",get("tmp.file.name",env=revive.sys)
," | grep \"^[0-9]\" > ",get("tmp.file.name",env=revive.sys)
))
    difflines<-scan(get("tmp.file.name",env=revive.sys)
,"",blank.lines.skip=FALSE,sep="\n")
    difflines<-sub("^([0-9]*)[a-z]","",difflines)
    difflines<-sub(",",":",difflines)
    difflines<-paste("c(",paste(difflines,collapse=","),")")
    try(difflines<-eval(parse(text=difflines)))
    if(length(difflines)>0 || "ERROR"!=substring(unlist(difflines)[1],1,5)){
      # suche Chunks:
      worktext<-c("@",worktext,"@")
      difflines<-difflines+1
      chunkbegins<-grep("(^@)|(^<<(.*)>=)", worktext)
      chunk.log<-rep(FALSE,length(chunkbegins))
      ch.new<- 0<hist(difflines,plot=FALSE,
                      breaks=c(chunkbegins,length(worktext)+1)-0.5)$counts
      ch.new<- ch.new | c(ch.new[-1],FALSE) | c(FALSE,ch.new[-length(ch.new)])
      ch.new<-seq(ch.new)[ch.new]
      # extrahiere Chunks:
      lines<-paste(chunkbegins[ch.new],":",
               c(chunkbegins[-1],chunkbegins[length(chunkbegins)]+1)[ch.new]-1)
      lines<-paste(lines,collapse=",")
      lines<-try(eval(parse(text=paste("c(",lines,")"))))
      if(ch.new[1]) lines<-lines[-1]
      if(ch.new[length(ch.new)]) lines<-lines[-length(lines)]
      difftext<-worktext[unique(lines)]
      # speichere difftext
      ### SaveAsHtml(difftext,filename)
      try.res <-try(cat(difftext,file=filename,sep="\n"))
    }
    melde("SaveDiffReport",2)
  }

  OpenReport<-function(){
    melde("OpenReport",1)
    filename<-tkgetOpenFile(filetypes="{{Paper Files} {.rev}}",
                            title="Select .rev file to be loaded!",
                            defaultextension=".rev", initialfile=workname.sys,
                            initialdir=getwd())
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    try.res<-try(myscan(filename,"",sep="\n",blank.lines.skip=FALSE))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      h <- strsplit(filename,.Platform$file.sep)[[1]]
      workname.sys<-rev(h)[1]
      workpath.sys<-paste(h[-length(h)],collapse=.Platform$file.sep)
      setwd(workpath.sys)

      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      try.res<-WinToTcl.read(try.res)
          ## Eintrag mit Entfernung des bisherigen Inhalts:
          ## worktext<-paste(try.res, collapse="\n")
          ## <<schreibe [[worktext]] ins Arbeitsfenster>>
      news<-c("",try.res)
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      pos.to.insert<-"end"
      if(0<length(grep("output-start",news))){
        tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
        ltail<-length(tail)
        if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
           any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
           news<-sub(".*output-start\n","",news)
           news<-sub("output-end","",news)
           h<-seq(along=h)[h][1]
           pos.to.insert<-paste("end -",h,"lines")
        }
      }
      try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
      tksee(tworkwin,"end - 0 lines")
      melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      code.start<-grep("^<<(.*)>>=",worktext)
      try(if(0<length(code.start)){ 
             worktext[code.start]<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext[code.start])
             worktext[code.start]<-paste(worktext[code.start]," (",1:length(code.start),")",sep="")
      })
      if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
      tkdelete(tworkwin,"0.0","end")
      try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
      tksee(tworkwin,"end")
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


      tkmark.set(tworkwin, "insert", paste(line,"0",sep="."))
      tksee(tworkwin,paste(line,"0",sep="."))
      tkfocus(tworkwin)

      RunStart()
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); createandshow.all.plots(tworkwin) }

      melde(paste("r", workname.sys), "cmd.msg")
    } else { cat("ERROR: File",filename,"not found!!!\n") }
    melde("OpenReport",2)
  }
  OpenTextFile<-function(){
    melde("OpenTextFile",1)
    filename<-tkgetOpenFile(filetypes="{{TEXT-FILE!} {*.*}}",
                            title="Select file to be loaded!",
                            initialfile=sub(".rev$",".txt",workname.sys),
                            initialdir=getwd())
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    try.res<-try(myscan(filename,"",sep="\n",blank.lines.skip=FALSE))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      filename<-paste(sub(".txt$","",filename),".rev",sep="")
      h <- strsplit(filename,.Platform$file.sep)[[1]]
      workname.sys<-rev(h)[1]
      workpath.sys<-paste(h[-length(h)],collapse=.Platform$file.sep)
      setwd(workpath.sys)

      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      try.res<-WinToTcl.read(try.res)
          try.res<-sub("^[ \t]*$","\n@",try.res)
          try.res<-sub("^\\+ ","> ",try.res)
          code.line<-"> "==substring(try.res,1,2)
          try.res[code.line]<-substring(try.res[code.line],3)
          first.code.line<-diff(c(FALSE,code.line))>0
          code.chunk.start<-paste("<","<*>",">=",sep="")
          try.res[first.code.line]<-paste(code.chunk.start,try.res[first.code.line],sep="\n")
          first.text.line<-rev(diff(c(FALSE,rev(code.line)))>0)
          try.res[first.text.line]<-paste(try.res[first.text.line],"@",sep="\n")
          
      ## Eintrag mit Entfernung des bisherigen Inhalts:
      ## worktext<-paste(try.res, collapse="\n"); <<schreibe [[worktext]] ins Arbeitsfenster>>
      news<-c("",try.res)
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      pos.to.insert<-"end"
      if(0<length(grep("output-start",news))){
        tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
        ltail<-length(tail)
        if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
           any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
           news<-sub(".*output-start\n","",news)
           news<-sub("output-end","",news)
           h<-seq(along=h)[h][1]
           pos.to.insert<-paste("end -",h,"lines")
        }
      }
      try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
      tksee(tworkwin,"end - 0 lines")
      melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      code.start<-grep("^<<(.*)>>=",worktext)
      try(if(0<length(code.start)){ 
             worktext[code.start]<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext[code.start])
             worktext[code.start]<-paste(worktext[code.start]," (",1:length(code.start),")",sep="")
      })
      if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
      tkdelete(tworkwin,"0.0","end")
      try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
      tksee(tworkwin,"end")
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


      tkmark.set(tworkwin, "insert", paste(line,"0",sep="."))
      tksee(tworkwin,paste(line,"0",sep="."))
      tkfocus(tworkwin)

      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); createandshow.all.plots(tworkwin) }

      melde(paste("r", workname.sys), "cmd.msg")
    } else { cat("ERROR: File",filename,"not found!!!\n") }
    melde("OpenTextFile",2)
  }
  OpenRevbook<-function(){
    melde("OpenRevbook",1)
    h<-file.path(relax.path,"rev")
    filename<-tkgetOpenFile(filetypes="{{Paper Files} {.rev}}",
                            title="Choose paper to be loaded",
                            defaultextension=".rev", initialfile=workname.sys,
                            initialdir=h)
    if(!is.character(filename)) filename<-tclvalue(filename)
    if(filename=="") return("cancel")

    try.res<-try(myscan(filename,"",sep="\n",blank.lines.skip=FALSE))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
      lworkname.sys<-get("lworkname.sys",envir=revive.sys)
      tkconfigure(lworkname.sys,text=workname.sys)
      assign("workname.sys",workname.sys,envir=revive.sys)

      code.ch<-grep("^<<(.*)>>=",try.res)
      if(0<length(code.ch))
         try.res[code.ch]<-paste(try.res[code.ch], " (",1:length(code.ch),")",sep="")
         try.res<-WinToTcl.read(try.res)
          worktext<-paste(try.res, collapse="\n") # alter Inhalt wird entfernt
          if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
          tkdelete(tworkwin,"0.0","end")
          try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
          tksee(tworkwin,"end")
          melde("ak texthervor",1)
          tcl("markclear",tworkwin)
          tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
          tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
          tcl("marklinetypes",tworkwin)
          melde("ak texthervor",2)

          if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


        RunStart()
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); createandshow.all.plots(tworkwin) }

      melde(paste("r", workname.sys), "cmd.msg")
    } else { cat("ERROR: File",filename,"not found!!!\n") }
    melde("OpenRevbook",2)
  }
  LoadRwtools<-function(){
    melde("LoadRwtools",1)
    path<-file.path(relax.path,"rev/robj.R")
    fns<-readLines(path) #2.1.0
    try(eval(parse(text=fns),
                 envir=pos.to.env(which(.path.package("relax")==searchpaths()))))
    melde("LoadRwtools",2)
  }

  ViewReport.html<-function(){
    melde("ViewReport.html",1)
    n<-nchar(filename<-workname.sys)
    if(is.null(n)||5>n||substring(filename,n-3,n)!=".rev"){
      cat("ERROR: file name not ok!!!\n"); return()
    }
    if(version$os=="Win32" || version$os=="mingw32"){
           fname<-paste(sub("rev$","html",filename),sep="")
           browser.exe<- if(browser.sys=="") "start " else browser.sys
           res<-shell(paste(browser.exe, fname),wait=FALSE)
           if(res!=0){
             cat("ERROR: browser hasn't been started successfully \n")
             cat("please, start and load file on foot\n")
           }
      }else{
          fname<-paste(sub("rev$","html",filename),sep="")
          if(browser.sys!=""){
                try(system(paste(browser.sys,fname),wait=FALSE))
          }else{
            if(file.exists(options()$browser))
              try(system(paste(options()$browser,fname),wait=FALSE))
            else if(1==length(system("which firefox",TRUE))) 
                try(system(paste("firefox",fname),wait=FALSE))
              else if(1==length(system("which mozilla",TRUE))) 
                   try(system(paste("mozilla",fname),wait=FALSE))
                 else if(1==length(system("which epiphany",TRUE))) 
                      try(system(paste("epiphany",fname),wait=FALSE))
          }
    }
    melde("ViewReport.html",2)
  }
  EditReport<-function(){
    melde("EditReport",1)
    if(substring(version$os,1,7)=="darwin8" ){
       cat("Sorry: not implemented yet!\n"); return()
    }    
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    worktext<-TcltoWin.write(worktext)
    try.res <-try(cat(worktext,file=get("tmp.file.name",env=revive.sys)
,sep="\n"))
    if(is.function(try.res)){
      ok <- "OK"
    } else {
      if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
      ok<-try.res[1]
      if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
      if(!is.character(ok)) { ok <- "OK" }
    }
    if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
      ok<-FALSE
      cat(error.msg<-unclass(try.res),"\n")
      if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
         cat("A warning message stopped the evaluation!",
               "If you want to\nevaluate the code anyway",
               "evaluate code by:\n>WarnEval<")
      cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
    } else { ok<-TRUE }


    if(ok){
      ##  try.res <- try(system(paste(editor.sys, ..tmp..)))
      cmd<-paste(editor.sys, get("tmp.file.name",env=revive.sys)
)
      if((version$os=="Win32" || version$os=="mingw32")
){
        try.res<-try(shell(cmd,wait=TRUE))
      }else{
        try.res<-try(system(cmd))
      }
      if(is.function(try.res)){
        ok <- "OK"
      } else {
        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
        ok<-try.res[1]
        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
        if(!is.character(ok)) { ok <- "OK" }
      }
      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
        ok<-FALSE
        cat(error.msg<-unclass(try.res),"\n")
        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
           cat("A warning message stopped the evaluation!",
                 "If you want to\nevaluate the code anyway",
                 "evaluate code by:\n>WarnEval<")
        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
      } else { ok<-TRUE }


      if(ok){
      try.res<-worktext<-try(myscan(get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE))
        if(is.function(try.res)){
          ok <- "OK"
        } else {
          if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
          ok<-try.res[1]
          if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
          if(!is.character(ok)) { ok <- "OK" }
        }
        if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
          ok<-FALSE
          cat(error.msg<-unclass(try.res),"\n")
          if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
             cat("A warning message stopped the evaluation!",
                   "If you want to\nevaluate the code anyway",
                   "evaluate code by:\n>WarnEval<")
          cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
        } else { ok<-TRUE }


        if(ok){
          if((version$os=="Win32" || version$os=="mingw32")
)worktext<-WinToTcl.read(worktext)
          line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          code.start<-grep("^<<(.*)>>=",worktext)
          try(if(0<length(code.start)){ 
                 worktext[code.start]<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext[code.start])
                 worktext[code.start]<-paste(worktext[code.start]," (",1:length(code.start),")",sep="")
          })
          if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
          tkdelete(tworkwin,"0.0","end")
          try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
          tksee(tworkwin,"end")
          melde("ak texthervor",1)
          tcl("markclear",tworkwin)
          tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
          tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
          tcl("marklinetypes",tworkwin)
          melde("ak texthervor",2)

          if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


          tkmark.set(tworkwin, "insert", paste(line,"0",sep="."))
          tksee(tworkwin,paste(line,"0",sep="."))
          tkfocus(tworkwin)

          worktext <- paste(worktext,collapse="\n")
  #       worktext <- sub("\\\\end\{document\}","%end\{document\}",worktext)
        } else {  cat("ERROR: file open operation not successful!!!\n") }
      } else {  cat("ERROR: editor start not successful!!!\n") }
    } else { cat("ERROR: file write operation not successful!!!\n") }
    melde("EditReport",2)
  }

  RunAll<-function(){
    melde("RunAll",1)
    news<-paste("RunAll:",date(),"\n")
    if(!exists("toutwin"))
      toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
    pos.to.insert<-"end"
    news<-paste(gsub("\n+","\n",news),collapse="\n")
    try(tkinsert(toutwin,pos.to.insert,news))
    tksee(toutwin,"end - 0 lines")
    melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    news<-paste("\n@\n<<RunAllStartsAndStars>>=\n<<start>>\n<<*>>\n@")
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    pos.to.insert<-"end"
    if(0<length(grep("output-start",news))){
      tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
      ltail<-length(tail)
      if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
         any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
         news<-sub(".*output-start\n","",news)
         news<-sub("output-end","",news)
         h<-seq(along=h)[h][1]
         pos.to.insert<-paste("end -",h,"lines")
      }
    }
    try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
    tksee(tworkwin,"end - 0 lines")
    melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    tkmark.set(tworkwin,"insert","end")
    fWarnEval()
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    h<-grep("RunAllStartsAndStars",worktext)
    if(0<length(h)) worktext<-worktext[1:(h[1]-2)]
    if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
    tkdelete(tworkwin,"0.0","end")
    try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
    tksee(tworkwin,"end")
    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


    melde("RunAll",2)
  }

  RunStart<-function(){
    melde("RunStart",1)
    news<-paste("RunStart:",date(),"\n")
    if(!exists("toutwin"))
      toutwin<-get("toutwin",envir=get("revive.sys",envir=revive.env))
    pos.to.insert<-"end"
    news<-paste(gsub("\n+","\n",news),collapse="\n")
    try(tkinsert(toutwin,pos.to.insert,news))
    tksee(toutwin,"end - 0 lines")
    melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))


    news<-paste("\n@\n<<RunStarts>>=\n<<start>>\n@")
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    pos.to.insert<-"end"
    if(0<length(grep("output-start",news))){
      tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
      ltail<-length(tail)
      if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
         any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
         news<-sub(".*output-start\n","",news)
         news<-sub("output-end","",news)
         h<-seq(along=h)[h][1]
         pos.to.insert<-paste("end -",h,"lines")
      }
    }
    try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
    tksee(tworkwin,"end - 0 lines")
    melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    tkmark.set(tworkwin,"insert","end")
    fWarnEval()
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    h<-grep("RunStart",worktext)
    if(0<length(h)) worktext<-worktext[1:(rev(h)[1]-2)] ## 060310
    if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
    tkdelete(tworkwin,"0.0","end")
    try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
    tksee(tworkwin,"end")
    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


    melde("RunStart",2)
  }

  DeleteAll<-function(){
    melde("DeleteAll",1)
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    worktext<-TcltoWin.write(tclvalue(tkget(tworkwin,"0.0","end")))
    get("cat","package:base")(worktext,file="report-UnDo-bak.rev")

    worktext<-paste("% New Report:",date(),"\n")
    if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
    tkdelete(tworkwin,"0.0","end")
    try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
    tksee(tworkwin,"end")
    melde("ak texthervor",1)
    tcl("markclear",tworkwin)
    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
    tcl("marklinetypes",tworkwin)
    melde("ak texthervor",2)

    if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


    melde("DeleteAll",2)
  }

  UnDo<-function(){
    melde("UnDo",1)
    if(file.exists("report-UnDo-bak.rev")){
      worktext<-myscan(file="report-UnDo-bak.rev",
                       what="",sep="\n",blank.lines.skip=FALSE)
      if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
      tkdelete(tworkwin,"0.0","end")
      try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
      tksee(tworkwin,"end")
      melde("ak texthervor",1)
      tcl("markclear",tworkwin)
      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
      tcl("marklinetypes",tworkwin)
      melde("ak texthervor",2)

      if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


    }else{
      cat("ERROR: file report-UnDo-bak.rev not found!!!\n")
    }
    melde("UnDo",2)
  }

  DefineFKeys<-function(){
    melde("DefineFKeys",1)
    frage<-"number of function key?"; set.tclvalue("tvinfo",1)
    tkconfigure(linfo.tmp,text=frage)
    tkpack("forget",linfo.name,linfo); Sys.sleep(0.01)
    tkpack(linfo.tmp,einfo.tmp,side="left"); Sys.sleep(0.01)
    tkfocus(einfo.tmp)
    tkselection.range(einfo.tmp,"0","end") ## 051219
    tkbind(TopW,"<Escape>",function(){
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


      }
    )


    tkbind(TopW,"<Return>", function(){
        F.no<-tclvalue("tvinfo")
        tkbind(TopW,"<Return>","")
        tkpack("forget",einfo.tmp,linfo.tmp); Sys.sleep(0.01)
        tkpack(linfo.name,linfo,side="left",fill="x")


        if(!any(F.no==as.character(1:8))) return() else F.no<-as.numeric(F.no)
        .newl<-tktoplevel();tkwm.geometry(.newl,"+0+15");tkpack(tt<-tktext(.newl))
        twin<-tt; f.sonderzeichen<-function(zeichen){
                    function(){
                      tkinsert(twin,"insert",zeichen)
                      if((version$os=="Win32" || version$os=="mingw32")
)tkdelete(twin,"insert-1chars")
                      if(substring(version$os,1,7)=="darwin8")tkdelete(twin,"insert-1chars")
                    }
                  }
                  f.umlaut<-function(zeichen){
                    function(){
                      return()
                      #char337<-eval(parse(text='"\\337"'))
                      #if(zeichen==char337 & tclvalue(tkget(twin,"insert-1chars","insert"))=="\\") return()
                      #tkinsert(twin,"insert",zeichen); tkdelete(twin,"insert-2chars")
                    }
                  }
                  tkbind(twin,"<<LKeckig>>", f.sonderzeichen("["))
                  tkbind(twin,"<<RKeckig>>", f.sonderzeichen("]"))
                  tkbind(twin,"<<Tilde>>",   f.sonderzeichen("~"))
                  tkbind(twin,"<<LKgeschw>>",f.sonderzeichen("{"))
                  tkbind(twin,"<<RKgeschw>>",f.sonderzeichen("}"))
                  tkbind(twin,"<<Klammera>>",f.sonderzeichen("@"))
                  tkbind(twin,"<<Pipe>>",    f.sonderzeichen("|"))
                  tkbind(twin,"<<Backsl>>",  f.sonderzeichen("\\"))
                  renewhighlighting<-function(){
                    tworkwin<-get("tworkwin",env=revive.sys)
                    melde("ak texthervor",1)
                    tcl("markclear",tworkwin)
                    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                    tcl("marklinetypes",tworkwin)
                    melde("ak texthervor",2)

                  }
                  # tkbind(twin,"<<Klammeraffe>>",renewhighlighting)
                  tkbind(twin,"<Return>",renewhighlighting)

        tkwm.title(.newl,paste("contents of function key","Definition by Escape!"))
        F.buffers<-get("F.buffers",env=revive.sys)
        try(tkinsert(tt,"0.0",paste(F.buffers[F.no],collapse="\n")))
        abbruch<-function(){tkdestroy(.newl); set.tclvalue("tvscandone",2)}
        tkbind(.newl,"<Escape>", function(){
                   F.buffers[F.no]<-tclvalue(tkget(tt,"0.0","end"))
                   assign("F.buffers",F.buffers,env=revive.sys)
                   tkdestroy(.newl); set.tclvalue("tvscandone",2)
                 })
        tkfocus(.newl);tkwait.variable("tvndone")
        set.tclvalue("tvmess","relax")
      } # end of function
    )
    melde("DefineFKeys",2)
  }
                       ##setze Umgebung für Knopf\-funktionen##
          ##setze Umgebung für Testknopf\-funktion##   ##setze Umgebung für Zeilen-Funktionen##
  environment(myhead.menu)<-revive.sys

  loadwwwdata<-function(){
    GetWWWData<-function(){
      melde("GetWWWData",1)
      URL<-paste("http://www.wiwi.uni-bielefeld.de/fileadmin/stat/wolf/data")
      download.file(paste(URL,"00Contents",sep="/"),"r.tmp")
      choices<-scan(file="r.tmp","",sep="\n")
      listboxmenu<-function(choices,title="items",addexit=TRUE){
          if(addexit) choices<-c(choices,"EXIT")
          lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
          tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
          ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
          lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
          tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
          tkconfigure(sb ,command=function(...) tkyview(lb,...))
          tkpack(ltit,lbframe)
          tkpack(sb,side="right",fill="y"); tkpack(lb)
          for(i in seq(choices)) tkinsert(lb,"end",choices[i])
          lbmdone      <- tclVar()
          tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
          tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
          tkfocus(lb); tkwait.variable(lbmdone)
          choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
          if(tclvalue(lbmdone)=="0") return(0)
          ind <- match(choice, choices)
          choice <- if(addexit && ind==length(choices)) 0 else ind
          return(choice)
      }

      choice<-listboxmenu(choices,"gefundene Datensaetze")
      if(0==choice) return() else choice<-sub(" .*$",".R",choices[choice])
      download.file(paste(URL,choice,sep="/"),"www.data.R")
      cmds<-scan(file="www.data.R","",sep="\n")
      ok<-try(eval(parse(text=cmds),envir=revive.env))
      cat("\"",sub(".R$","",choice),"\" geladen\n",sep="")
      melde("GetWWWData",2)
    }
    GetWWWData()
  }

  generate.1.data<-function(){
   (function(){
    DS<-c("sample(99)","sample(99,replace=TRUE)","rnorm(99,mean=0,sd=1)","rexp(99)")
    listboxmenu<-function(choices,title="items",addexit=TRUE){
        if(addexit) choices<-c(choices,"EXIT")
        lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
        tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
        ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
        lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
        tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
        tkconfigure(sb ,command=function(...) tkyview(lb,...))
        tkpack(ltit,lbframe)
        tkpack(sb,side="right",fill="y"); tkpack(lb)
        for(i in seq(choices)) tkinsert(lb,"end",choices[i])
        lbmdone      <- tclVar()
        tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
        tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
        tkfocus(lb); tkwait.variable(lbmdone)
        choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
        if(tclvalue(lbmdone)=="0") return(0)
        ind <- match(choice, choices)
        choice <- if(addexit && ind==length(choices)) 0 else ind
        return(choice)
    }

    choice<-listboxmenu(DS,"Items:")
    if(0==length(choice)||is.na(choice)||0==choice) return()
    choice<-DS[choice]
    if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



    cmd<-paste("x <-",choice)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
    last.code<-grep("^<<(.*)>>=",worktext)
    if(0<length(last.code)){
      last.code<-rev(last.code[last.code<=line])[1]
      if(is.na(last.code)||last.code<last.text)
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
      else
                      {delta<-1; news<-paste(cmd,"\n",sep="")}
    }else{
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
    }
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        anzrows<-length(news)
        try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
        tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
        tksee(tworkwin,paste(line+anzrows,"0",sep="."))
    tkfocus(tworkwin)
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

   })()
  }

  get.dim.1.data<-function(){
   (function(){
    ds<-function(pos=1,type,mode="numeric",struc=is.vector){
      if(pos>0){
       obj<-ls(pos=pos)
       obj<-obj[unlist(lapply(obj ,function(o,pos)
         exists(o,where=pos,mode="numeric") &&
         eval(parse(text=paste("struc(get(\"",o,"\",pos=",pos,"))",sep=""))),pos))]
      }else{
       obj<-ls(env=revive.env)
       obj<-obj[unlist(lapply(obj ,function(o)
         exists(o,where=revive.env,mode="numeric") &&
         eval(parse(text=paste("struc(get(\"",o,"\",env=revive.env))",sep=""))) ))]
      }
      if(0==length(obj)) return(NULL) else return(obj)
    }
    ds.of.R<-function(type="vector"){
      dat<-ls(pos=grep("datasets",search()))
      dat.type<-unlist(lapply(dat,function(x) {       
             num<-mode(x<-eval(parse(text=x)))
             num<-ifelse(is.array(x),"array",num)
             num<-ifelse(is.list(x),"list",num)
             num<-ifelse(is.matrix(x),"matrix",num)
             num<-ifelse(is.data.frame(x),"matrix",num)
             num<-ifelse(num=="numeric","vector",num)
             num }))
      return(dat[dat.type==type])
    }



    DS<-c( ds.of.R("vector"),ds(-1), ds(1),
               ds(which(paste("package","relax"
,sep=":")==search())
),
               "-5:5 # integers from -5 to 5", "rep(7,5) # vector: (7,7,7,7,7)")

    listboxmenu<-function(choices,title="items",addexit=TRUE){
        if(addexit) choices<-c(choices,"EXIT")
        lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
        tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
        ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
        lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
        tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
        tkconfigure(sb ,command=function(...) tkyview(lb,...))
        tkpack(ltit,lbframe)
        tkpack(sb,side="right",fill="y"); tkpack(lb)
        for(i in seq(choices)) tkinsert(lb,"end",choices[i])
        lbmdone      <- tclVar()
        tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
        tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
        tkfocus(lb); tkwait.variable(lbmdone)
        choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
        if(tclvalue(lbmdone)=="0") return(0)
        ind <- match(choice, choices)
        choice <- if(addexit && ind==length(choices)) 0 else ind
        return(choice)
    }

    choice<-listboxmenu(DS,"Items:")
    if(0==length(choice)||is.na(choice)||0==choice) return()
    choice<-DS[choice]
    if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



    cmd<-paste("x <-",choice)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
    last.code<-grep("^<<(.*)>>=",worktext)
    if(0<length(last.code)){
      last.code<-rev(last.code[last.code<=line])[1]
      if(is.na(last.code)||last.code<last.text)
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
      else
                      {delta<-1; news<-paste(cmd,"\n",sep="")}
    }else{
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
    }
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        anzrows<-length(news)
        try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
        tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
        tksee(tworkwin,paste(line+anzrows,"0",sep="."))
    tkfocus(tworkwin)
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

   })()
  }

  get.dim.2.data<-function(){
   (function(){
    ds<-function(pos=1,type,mode="numeric",struc=is.vector){
      if(pos>0){
       obj<-ls(pos=pos)
       obj<-obj[unlist(lapply(obj ,function(o,pos)
         exists(o,where=pos,mode="numeric") &&
         eval(parse(text=paste("struc(get(\"",o,"\",pos=",pos,"))",sep=""))),pos))]
      }else{
       obj<-ls(env=revive.env)
       obj<-obj[unlist(lapply(obj ,function(o)
         exists(o,where=revive.env,mode="numeric") &&
         eval(parse(text=paste("struc(get(\"",o,"\",env=revive.env))",sep=""))) ))]
      }
      if(0==length(obj)) return(NULL) else return(obj)
    }
    ds.of.R<-function(type="vector"){
      dat<-ls(pos=grep("datasets",search()))
      dat.type<-unlist(lapply(dat,function(x) {       
             num<-mode(x<-eval(parse(text=x)))
             num<-ifelse(is.array(x),"array",num)
             num<-ifelse(is.list(x),"list",num)
             num<-ifelse(is.matrix(x),"matrix",num)
             num<-ifelse(is.data.frame(x),"matrix",num)
             num<-ifelse(num=="numeric","vector",num)
             num }))
      return(dat[dat.type==type])
    }



    DS<-c(ds.of.R("matrix"),ds(struc=is.matrix))
    DS<-c(DS,"matrix(sample(100),50,2)","cbind(1:100,rnorm(100,mean=0,sd=1))")
    DS<-c(DS, ds(-1,struc=is.matrix), ds(1,struc=is.matrix) ,
               ds(which(paste("package","relax"
,sep=":")==search())
 ,struc=is.matrix) )

    listboxmenu<-function(choices,title="items",addexit=TRUE){
        if(addexit) choices<-c(choices,"EXIT")
        lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
        tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
        ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
        lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
        tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
        tkconfigure(sb ,command=function(...) tkyview(lb,...))
        tkpack(ltit,lbframe)
        tkpack(sb,side="right",fill="y"); tkpack(lb)
        for(i in seq(choices)) tkinsert(lb,"end",choices[i])
        lbmdone      <- tclVar()
        tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
        tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
        tkfocus(lb); tkwait.variable(lbmdone)
        choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
        if(tclvalue(lbmdone)=="0") return(0)
        ind <- match(choice, choices)
        choice <- if(addexit && ind==length(choices)) 0 else ind
        return(choice)
    }

    choice<-listboxmenu(DS,"Items:")
    if(0==length(choice)||is.na(choice)||0==choice) return()
    choice<-DS[choice]
    if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



    cmd<-paste("xy <-",choice)
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
    last.code<-grep("^<<(.*)>>=",worktext)
    if(0<length(last.code)){
      last.code<-rev(last.code[last.code<=line])[1]
      if(is.na(last.code)||last.code<last.text)
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
      else
                      {delta<-1; news<-paste(cmd,"\n",sep="")}
    }else{
                {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
    }
    if(!exists("tworkwin"))
      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

        anzrows<-length(news)
        try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
        tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
        tksee(tworkwin,paste(line+anzrows,"0",sep="."))
    tkfocus(tworkwin)
    melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

    tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

   })()
  }

  removenafromvecx<-function(){
    (function(){
      cmd<-"x<-x[!is.na(x)]"
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)


      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

     })()
  }
  removenafrommatxy<-function(){
    (function(){
      cmd<-"xy<-xy[!apply(is.na(xy),1,any),,drop=FALSE]"
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)


      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

     })()
  }

  choosecol<-function(){
    (function(){
      ds<-function(pos=1,type,mode="numeric",struc=is.vector){
        if(pos>0){
         obj<-ls(pos=pos)
         obj<-obj[unlist(lapply(obj ,function(o,pos)
           exists(o,where=pos,mode="numeric") &&
           eval(parse(text=paste("struc(get(\"",o,"\",pos=",pos,"))",sep=""))),pos))]
        }else{
         obj<-ls(env=revive.env)
         obj<-obj[unlist(lapply(obj ,function(o)
           exists(o,where=revive.env,mode="numeric") &&
           eval(parse(text=paste("struc(get(\"",o,"\",env=revive.env))",sep=""))) ))]
        }
        if(0==length(obj)) return(NULL) else return(obj)
      }
      ds.of.R<-function(type="vector"){
        dat<-ls(pos=grep("datasets",search()))
        dat.type<-unlist(lapply(dat,function(x) {       
               num<-mode(x<-eval(parse(text=x)))
               num<-ifelse(is.array(x),"array",num)
               num<-ifelse(is.list(x),"list",num)
               num<-ifelse(is.matrix(x),"matrix",num)
               num<-ifelse(is.data.frame(x),"matrix",num)
               num<-ifelse(num=="numeric","vector",num)
               num }))
        return(dat[dat.type==type])
      }



      DS<-c(ds.of.R("matrix"),ds(struc=is.matrix))
      DS<-c(DS,"matrix(sample(100),50,2)","cbind(1:100,rnorm(100,mean=0,sd=1))")
      DS<-c(DS, ds(-1,struc=is.matrix), ds(1,struc=is.matrix) ,
                 ds(which(paste("package","relax"
,sep=":")==search())
 ,struc=is.matrix) )

      listboxmenu<-function(choices,title="items",addexit=TRUE){
          if(addexit) choices<-c(choices,"EXIT")
          lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
          tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
          ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
          lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
          tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
          tkconfigure(sb ,command=function(...) tkyview(lb,...))
          tkpack(ltit,lbframe)
          tkpack(sb,side="right",fill="y"); tkpack(lb)
          for(i in seq(choices)) tkinsert(lb,"end",choices[i])
          lbmdone      <- tclVar()
          tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
          tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
          tkfocus(lb); tkwait.variable(lbmdone)
          choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
          if(tclvalue(lbmdone)=="0") return(0)
          ind <- match(choice, choices)
          choice <- if(addexit && ind==length(choices)) 0 else ind
          return(choice)
      }

      choice<-listboxmenu(DS,"Items:")
      if(0==length(choice)||is.na(choice)||0==choice) return()
      choice<-DS[choice]
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



      choose.col<-function(obj,title="Which variable?"){
        if(is.character(obj)) try(obj<-eval(parse(text=obj)))
        if(!is.matrix(obj)) return()
        if(is.null(choices<-dimnames(obj)[[2]])) choices<-1:ncol(obj)
        listboxmenu<-function(choices,title="items",addexit=TRUE){
            if(addexit) choices<-c(choices,"EXIT")
            lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
            tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
            ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
            lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
            tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
            tkconfigure(sb ,command=function(...) tkyview(lb,...))
            tkpack(ltit,lbframe)
            tkpack(sb,side="right",fill="y"); tkpack(lb)
            for(i in seq(choices)) tkinsert(lb,"end",choices[i])
            lbmdone      <- tclVar()
            tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
            tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
            tkfocus(lb); tkwait.variable(lbmdone)
            choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
            if(tclvalue(lbmdone)=="0") return(0)
            ind <- match(choice, choices)
            choice <- if(addexit && ind==length(choices)) 0 else ind
            return(choice)
        }

        choice<-listboxmenu(choices,title)
        choice
      }

      if(0==(col.no<-choose.col(choice))) return()
      cmd<-paste("x<-",choice,"[,",col.no,"]",sep="")
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

     })()
  }

  get.dim.1.stats<-function(){
    (function(){
      methoden  <-c("median of x"                     ="median(x,rm.na=TRUE)"
                    ,"mean of x"                              ="mean(x,rm.na=TRUE)"
                    ,"standard deviation of x"         ="sd(x,rm.na=TRUE)"
                    ,"variance of x"                          ="var(x,rm.na=TRUE)"
                    ,"maximum of x"                   ="max(x,rm.na=TRUE)"
                    ,"minimum of x"                           ="min(x,rm.na=TRUE)"
                    ,"quantiles of x, prob: p"        ="quantile(x,p)"
                    ,"range of x"                             ="range(x,rm.na=TRUE)"
                    ,"inter quartile range of x"              ="IQR(x,rm.na=TRUE)"
                    ,"5 number summary of x"          ="five.num(x,rm.na=TRUE)"
                    ,"summary statistics of x"        ="summary(x,rm.na=TRUE)"
                    ,"sorted data of x"               ="sort(x)"
                    ,"ranks of x"                                     ="rank(x)"
                    ,"sum of x"               ="sum(x)"
                    ,"cumulative sum of x"                    ="cumsum(x)"
                    ,"length of x"                    ="length(x)"
                    ,"frequency table of x"                   ="table(x)"
                    ,"relative frequencies of x"                      ="table(x)/length(x)"
                    )
      DS<-names(methoden)

      listboxmenu<-function(choices,title="items",addexit=TRUE){
          if(addexit) choices<-c(choices,"EXIT")
          lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
          tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
          ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
          lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
          tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
          tkconfigure(sb ,command=function(...) tkyview(lb,...))
          tkpack(ltit,lbframe)
          tkpack(sb,side="right",fill="y"); tkpack(lb)
          for(i in seq(choices)) tkinsert(lb,"end",choices[i])
          lbmdone      <- tclVar()
          tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
          tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
          tkfocus(lb); tkwait.variable(lbmdone)
          choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
          if(tclvalue(lbmdone)=="0") return(0)
          ind <- match(choice, choices)
          choice <- if(addexit && ind==length(choices)) 0 else ind
          return(choice)
      }

      choice<-listboxmenu(DS,"Items:")
      if(0==length(choice)||is.na(choice)||0==choice) return()
      choice<-DS[choice]
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



      cmd<-methoden[choice]
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

     })()
  }
  get.dim.1.plots<-function(){
    (function(){
      methoden  <-c("1 dim plot of x"="plot(x)"
       ,"boxplot of x"              ="boxplot(x)"
       ,"jitterplot of x"              ="plot(jitter(x))"
       ,"histogram of x"              ="hist(x,prob=TRUE)"
       ,"histogram of x, breaks at br"           ="hist(x,breaks=br,prob=TRUE)"
       ,"plot of density trace of x, width w"="plot(density(x,width=w),type=\"l\")"
       ,"barplot of x"              ="barplot(x)"
       ,"barplot of relative frequencies of x"              ="plot(table(x)/length(x))"
       ,"barplot of frequencies h.i at x.i"      ="plot(x.i, h.i, type=\"h\")"
       ,"empirical distribution function  of x"              ="plot(ecdf(x))"
       ,"stem and leaf display of x"="if(exists(\"stem.leaf\")) stem.leaf(x) else stem(x)"
      )
      DS<-names(methoden)

      listboxmenu<-function(choices,title="items",addexit=TRUE){
          if(addexit) choices<-c(choices,"EXIT")
          lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
          tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
          ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
          lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
          tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
          tkconfigure(sb ,command=function(...) tkyview(lb,...))
          tkpack(ltit,lbframe)
          tkpack(sb,side="right",fill="y"); tkpack(lb)
          for(i in seq(choices)) tkinsert(lb,"end",choices[i])
          lbmdone      <- tclVar()
          tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
          tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
          tkfocus(lb); tkwait.variable(lbmdone)
          choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
          if(tclvalue(lbmdone)=="0") return(0)
          ind <- match(choice, choices)
          choice <- if(addexit && ind==length(choices)) 0 else ind
          return(choice)
      }

      choice<-listboxmenu(DS,"Items:")
      if(0==length(choice)||is.na(choice)||0==choice) return()
      choice<-DS[choice]
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



      cmd<-methoden[choice]
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

     })()
  }

  get.dim.2.methods<-function(){
    (function(){
      methoden  <-c("scatter plot of xy[,1:2]"="plot(xy[,1:2])",
                    "correlation of xy" ="cor(xy)",
                    "regression line of xy"  ="plot(xy[,1:2])\nabline(lsfit(xy[,1],xy[,2]))",
                    "mean values of cols of xy" ="apply(xy,2,mean)"
                  )
      DS<-names(methoden)

      listboxmenu<-function(choices,title="items",addexit=TRUE){
          if(addexit) choices<-c(choices,"EXIT")
          lbmTop<-tktoplevel();tkwm.geometry(lbmTop,"+0+15")
          tkwm.title(lbmTop,"Selection by Click and Return, Quit by Esc")
          ltit<-tklabel(lbmTop,text=title); lbframe<-tkframe(lbmTop)
          lb<-tklistbox(lbframe,height=8,width=60); sb<-tkscrollbar(lbframe)
          tkconfigure(lb,yscrollcommand=function(...) tkset(sb,...))
          tkconfigure(sb ,command=function(...) tkyview(lb,...))
          tkpack(ltit,lbframe)
          tkpack(sb,side="right",fill="y"); tkpack(lb)
          for(i in seq(choices)) tkinsert(lb,"end",choices[i])
          lbmdone      <- tclVar()
          tkbind(lbmTop,"<Return>",function()set.tclvalue(lbmdone,"1"))
          tkbind(lbmTop,"<Escape>",function()set.tclvalue(lbmdone,"0"))
          tkfocus(lb); tkwait.variable(lbmdone)
          choice<-tclvalue(tkget(lb,"active")); tkdestroy(lbmTop)
          if(tclvalue(lbmdone)=="0") return(0)
          ind <- match(choice, choices)
          choice <- if(addexit && ind==length(choices)) 0 else ind
          return(choice)
      }

      choice<-listboxmenu(DS,"Items:")
      if(0==length(choice)||is.na(choice)||0==choice) return()
      choice<-DS[choice]
      if(!exists("myscan")) myscan<-get("myscan",env=revive.sys)



      cmd<-methoden[choice]
      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
      tworkwin<-get("tworkwin",envir=revive.sys)
      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
      if(nchar(worktext)<10000){
        worktext<-strsplit(worktext,"\n")[[1]]
      }else{
        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
      }

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      last.text<-c(0,grep("^@",worktext)); last.text<-rev(last.text[last.text<=line])[1]
      last.code<-grep("^<<(.*)>>=",worktext)
      if(0<length(last.code)){
        last.code<-rev(last.code[last.code<=line])[1]
        if(is.na(last.code)||last.code<last.text)
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="") }
        else
                        {delta<-1; news<-paste(cmd,"\n",sep="")}
      }else{
                  {delta<-4; news<-paste("\n@\n<<*>>=\n",cmd,"\n@\n",sep="")}
      }
      if(!exists("tworkwin"))
        tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

          anzrows<-length(news)
          try(tkinsert(tworkwin,paste(line+1,"0",sep="."),paste(news,collapse="\n")))
          tkmark.set(tworkwin, "insert", paste(line+anzrows,"0",sep="."))
          tksee(tworkwin,paste(line+anzrows,"0",sep="."))
      tkfocus(tworkwin)
      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

      tkmark.set(tworkwin, "insert", paste(line+delta,"0",sep="."))

    })()
  }

  data.fns.menu<-function(){
    myhead.menu(rm.menu=TRUE, menu.no=0)
    # myhead.menu(item="load data via internet",code=loadwwwdata, menu.no=0)   # ok
    myhead.menu(item="1-dim data sets",code=get.dim.1.data,title="Data",menu.no=0)    # ok
    myhead.menu(item="1-dim random numbers",code=generate.1.data,menu.no=0) # ok
    myhead.menu(item="2-dim data sets",code=get.dim.2.data,menu.no=0)    # ok
    myhead.menu(item="delete NAs from vector x",code=removenafromvecx,menu.no=0)
    myhead.menu(item="delete NAs from matrix xy",code=removenafrommatxy,menu.no=0)
    myhead.menu(item="save col of matrix xy as x",code=choosecol,menu.no=0)
    myhead.menu(item="1-dim statistics",code=get.dim.1.stats,title="Methods",menu.no=1)   # ok
    myhead.menu(item="1-dim plots",code=get.dim.1.plots,title="Methods",menu.no=1)   # ok
    myhead.menu(item="2-dim methods",code=get.dim.2.methods,menu.no=1)   # ok
  }

  LAST.WARNING<-"no warnings"
  assign("LAST.WARNING",LAST.WARNING,env=revive.sys)
  Rversion<-as.numeric(R.version$major)*100+as.numeric(R.version$minor)

  REVFILE            <- "REVFILE"    # eingelesener RevFile
  RCHFILE            <- "RCHFILE"    # eingelesener Chunk-File
  fr.paper.sys       <- "forget"     #
  relax.version.sys<- "relax 1.1 - 081201"

  tvexit       <- tclVar("0")
  tvchoice     <- tclVar("0")
  tvndone      <- tclVar("0")
  tvscandone   <- tclVar("0")
  tvinfo       <- tclVar("")      # Variable des Kopf-Entry-Widget
  tvreadline   <- tclVar("0")
  tvmess       <- tclVar("relax")


  revpath.sys  <- getwd()         # Pfad zum Revwebpaperverzeichnis
  secno.sys    <- "0"             # aktuelle SecNo. in der Revdatei
  string.sys   <- ""              # Default-Suchstring
  sweave.args.sys<-""  # Default-Setzung weiterer Sweave-Argumente

  sizes<-c("8-80","10-100","12-120","14-140","18-180","24-240","*-2000")
  tfont.sys    <- "-Adobe-helvetica-Medium-R-Normal--14-140-*" # Text-Schrift
  tfont.sys<-sub("al--.+-.+-\\*",paste("al--",sizes[initial.font.size],"-*",sep=""),tfont.sys)
  outfont.sys  <- "-Adobe-courier-Medium-R-Normal--14-140-*"   # Output-Schrift
  outfont.sys<-sub("al--.+-.+-\\*",paste("al--",sizes[initial.font.size],"-*",sep=""),outfont.sys)
  workname.sys<-"out.rev"

  if((version$os=="Win32" || version$os=="mingw32")
 | substring(version$os,1,5)=="linux"
){
    tkevent.add("<<FindText>>",  "<Alt_L><f>")
    tkevent.add("<<EvalRCode>>",  "<Alt_L><e>")
    tkevent.add("<<WarnEval>>",  "<Alt_L><w>")
    tkevent.add("<<Down>>",  "<Alt_L><d>")
    tkevent.add("<<Up>>",  "<Alt_L><u>")
    tkevent.add("<<AdvanceNo>>", "<Alt_L><a>")
    tkevent.add("<<TrashROutput>>","<Alt_L><t>")
    tkevent.add("<<Next>>",      "<Alt_L><n>")
    tkevent.add("<<Help.R>>",   "<Alt_L><h>")
    tkevent.add("<<Modify>>",    "<Alt_L><m>")
    tkevent.add("<<PlanRCode>>",   "<Alt_L><p>")
    tkevent.add("<<FindReportText>>",   "<Control_L><f>")
    tkevent.add("<<GoToLine>>",   "<Control_L><g>")
    tkevent.add("<<SaveReport>>",   "<Control_L><s>")
    tkevent.add("<<ProcessReport>>",   "<Control_L><p>")
    tkevent.add("<<RemoveOut>>",   "<Alt_L><r>")
    tkevent.add("<<Insert>>",   "<Alt_L><i>")
    tkevent.add("<<SavePlot>>","<Alt_L><s>")
    tkevent.add("<<CopyToEnd>>",   "<Alt_L><c>")
    tkevent.add("<<CopyOld>>",   "<Alt_L><x>")
    tkevent.add("<<RunNo>>",     "<Alt_L><r>")
    tkevent.add("<<SetNo>>",     "<Alt_L><s>")
    tkevent.add("<<ActChunk>>",  "<Control_L><Next>")#Short-Cut copy.down / set
    tkevent.add("<<RKgeschw>>",  "<Alt_L><0>")
    tkevent.add("<<LKgeschw>>",  "<Alt_L><7>")
    tkevent.add("<<RKeckig>>",   "<Alt_L><9>")
    tkevent.add("<<LKeckig>>",   "<Alt_L><8>")
    tkevent.add("<<Tilde>>",     "<Alt_L><plus>")
    tkevent.add("<<Klammera>>",  "<Alt_L><q>")
    #tkevent.add("<<Backsl>>",    "<Alt_L><ssharp>")
    tkevent.add("<<Pipe>>",      "<Alt_L><less>")
    tkevent.add("<<aeumlaut>>",      "<adiaeresis><KeyRelease>")
    tkevent.add("<<oeumlaut>>",      "<odiaeresis><KeyRelease>")
    tkevent.add("<<ueumlaut>>",      "<udiaeresis><KeyRelease>")
    tkevent.add("<<Aeumlaut>>",      "<Adiaeresis><KeyRelease>")
    tkevent.add("<<Oeumlaut>>",      "<Odiaeresis><KeyRelease>")
    tkevent.add("<<Ueumlaut>>",      "<Udiaeresis><KeyRelease>")
    tkevent.add("<<szumlaut>>",      "<ssharp><KeyRelease>")
    tkevent.add("<<Backsl>>",    "<Alt_L><ssharp>")
  #  tkevent.add("<<Klammeraffe>>",  
  #                   "<ISO_Level3_Shift><KeyPress><KeyRelease><KeyRelease>")
  } else {                                               #f <hpguilder>
    tkevent.add("<<WarnEval>>",  "<ae>")
    tkevent.add("<<AdvanceNo>>", "<aring>")
    tkevent.add("<<TrashROutput>>","<hpmute_acute>") # vorher Drop:<eth>  #u <hpmute_diaeresis>
    tkevent.add("<<InsertPlot>>","<hpmute_asciitilde>") #c <ccedilla>
    tkevent.add("<<Next>>",      "<ordfeminine>")
    tkevent.add("<<Help.R>>",   "<yen>")
    tkevent.add("<<Modify>>",    "<mu>")
    tkevent.add("<<PlanRCode>>",   "<thorn>")
    tkevent.add("<<CopyOld>>",   "<ccedilla>")                #a aring
    tkevent.add("<<RunNo>>",     "<hpmute_acute>")
    tkevent.add("<<SetNo>>",     "<ssharp>")
    tkevent.add("<<ActChunk>>",  "<Control_L><Next>")#Short-Cut copy.down / set
    tkevent.add("<<RKgeschw>>",  "<braceright>")
    tkevent.add("<<LKgeschw>>",  "<braceleft>")
    tkevent.add("<<RKeckig>>",   "<bracketright>")
    tkevent.add("<<LKeckig>>",   "<bracketleft>")
    tkevent.add("<<Tilde>>",     "<asciitilde>")
    tkevent.add("<<Klammera>>",  "<at>")
    tkevent.add("<<Backsl>>",    "<Alt_R><backslash>")
    tkevent.add("<<Pipe>>",      "<Alt_R><bar>")
  }
  if(substring(version$os,1,7)=="darwin8"){
    tkevent.add("<<FindText>>",  "<Meta_L><f>")
    tkevent.add("<<EvalRCode>>",  "<Meta_L><e>")
    tkevent.add("<<WarnEval>>",  "<Meta_L><w>")
    tkevent.add("<<Down>>",  "<Meta_L><d>")
    tkevent.add("<<Up>>",  "<Meta_L><u>")
    tkevent.add("<<AdvanceNo>>", "<Meta_L><a>")
    tkevent.add("<<TrashROutput>>","<Meta_L><t>")
    tkevent.add("<<Next>>",      "<Meta_L><n>")
    tkevent.add("<<Help.R>>",   "<Meta_L><h>")
    tkevent.add("<<Modify>>",    "<Meta_L><m>")
    tkevent.add("<<PlanRCode>>",   "<Meta_L><p>")
    tkevent.add("<<FindReportText>>",   "<Control_L><f>")
    tkevent.add("<<GoToLine>>",   "<Control_L><g>")
    tkevent.add("<<SaveReport>>",   "<Control_L><s>")
    tkevent.add("<<ProcessReport>>",   "<Control_L><p>")
    tkevent.add("<<RemoveOut>>",   "<Meta_L><r>")
    tkevent.add("<<Insert>>",   "<Meta_L><i>")
    tkevent.add("<<SavePlot>>","<Meta_L><s>")
    tkevent.add("<<CopyToEnd>>",   "<Alt_L><c>")
    tkevent.add("<<CopyOld>>",   "<Alt_L><x>")
    tkevent.add("<<RunNo>>",     "<Meta_L><r>")
    tkevent.add("<<SetNo>>",     "<Meta_L><s>")
    #tkevent.add("<<ActChunk>>",  "<Control_L><Next>")#Short-Cut copy.down / set
    tkevent.add("<<RKgeschw>>",  "<Meta_L><0>")
    tkevent.add("<<LKgeschw>>",  "<Meta_L><7>")
    tkevent.add("<<RKeckig>>",   "<Meta_L><9>")
    tkevent.add("<<LKeckig>>",   "<Meta_L><8>")
    #tkevent.add("<<Tilde>>",     "<Meta_L><plus>")
    tkevent.add("<<Klammera>>",  "<Meta_L><q>")
    #tkevent.add("<<Backsl>>",    "<Meta_L><ssharp>")
    tkevent.add("<<Pipe>>",      "<Meta_L><less>")
    #tkevent.add("<<aeumlaut>>",      "<adiaeresis><KeyRelease>")
    #tkevent.add("<<oeumlaut>>",      "<odiaeresis><KeyRelease>")
    #tkevent.add("<<ueumlaut>>",      "<udiaeresis><KeyRelease>")
    #tkevent.add("<<Aeumlaut>>",      "<Adiaeresis><KeyRelease>")
    #tkevent.add("<<Oeumlaut>>",      "<Odiaeresis><KeyRelease>")
    #tkevent.add("<<Ueumlaut>>",      "<Udiaeresis><KeyRelease>")
    #tkevent.add("<<szumlaut>>",      "<ssharp><KeyRelease>")
    #tkevent.add("<<Backsl>>",    "<Meta_L><ssharp>")
  #  tkevent.add("<<Klammeraffe>>",  
  #                   "<ISO_Level3_Shift><KeyPress><KeyRelease><KeyRelease>")
  } 
         ##definiere Testknopf-Ereignis>>
  implement.but<-
  function(but,frame,mess=" ",side="right",relief="raised",short.cut=TRUE,bwf=1,job=""){ ## 071115
    if(is.character(frame)) frame<-eval(parse(text=frame))
    b<-tkbutton(frame, text=but, relief=relief, pady="-3"
,
                width=floor(10
*bwf), font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
    if(short.cut) tkconfigure(b,underline=0)
    tkbind(b,"<Enter>",function()set.tclvalue("tvmess",mess))
    tkbind(b,"<Leave>",function(){ set.tclvalue("tvmess","relax") })
    tkbind(b,"<Button-1><ButtonRelease>",job) ## 071115
    tkpack(b, side=side)
    assign(but, b, envir=revive.sys)
  }
  melde("Implement.but defined",3)

  TopW<-tktoplevel(); tkwm.geometry(TopW,"+0+15")
  tkwm.title(TopW,paste("RELAX -- R Editor for Literate Analysis and lateX:",
                        "relax 1.1 - 081201"))
  tkwm.protocol(TopW,"WM_DELETE_WINDOW",function(){
                     if(!exists("tworkwin"))
                       tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

                     worktext<-TcltoWin.write(tclvalue(tkget(tworkwin,"0.0","end")))
                     get("cat","package:base")(worktext,file="report-UnDo-bak.rev")

                     cat("... not a nice way to exit relax !!!\n")
                     cat("backup file: report-UnDo-bak.rev\n")
                     if( (version$os=="Win32" || version$os=="mingw32")
 ){
                        cat("--> to proceed the R session try: Quit R, then don't quit!\n")
                     }
                     if(substring(version$os,1,5)=="linux"
) cat("-- try: <Ctrl-C> to proceed the R session!\n")
                     set.tclvalue("tvexit","fertig"); tkdestroy(TopW)
             }
  )


  fhead   <- tkframe(TopW, relief="raised", bd="1")
  finfo     <- tkframe(TopW)
  # finout   <- tkframe(TopW,height="700",width="700") 
  finout   <- tkframe(TopW,height=relaxwindow.height.sys,width=relaxwindow.width.sys) 
  tkpack(fhead, side="top",fill="x")
  tkpack(finfo,   side="top")
  tkpack(finout, side="top",fill="both",expand="1")

  fworkwin<-tkframe(finout)
  fout        <-tkframe(finout)
  fhandle   <-tkframe(finout,bd="2",relief="raised", bg="#BEE3D2",   # #87ff9D",
                                     cursor="sb_v_double_arrow")
  tkplace(fworkwin, relwidth="1",rely="0",height="-1",anchor="nw")
  tkplace(fout, relwidth="1",rely="1",height="-1",anchor="sw")
  tkplace(fhandle, relx="1",width="1500",height="6",anchor="e")

  proc<-paste(
    paste("bind ",TopW$ID," <Configure> {" ),
        paste("set H [winfo height ",finout$ID, "].0" ),
        paste("set Y0  [winfo rooty ",finout$ID, "]" ),
    "}",
    paste("bind",fhandle$ID," <B1-Motion> {"),
      "set fract [expr (%Y -$Y0)/$H]",
      "if { $fract < 0.1 } {",
      "  set fract 0.1",
      "}",
      "if { $fract > 0.95 } {",
      "  set fract 0.95",
      "}",
      paste("place ",fworkwin$ID," -relheight $fract"),
      paste("place ",fhandle$ID," -rely $fract"),
      paste("place ",fout," -relheight [expr 1.0 - $fract]"),
    "}",
    sep="\n")
  .Tcl(proc)

  fract<- 0.5
  tkplace(fworkwin, relheight=fract)
  tkplace(fhandle, rely=fract)
  tkplace(fout, relheight=1-fract)

  fworkcmds<-tkframe(fout, relief="raised", bd="0")
  foutcmds<-tkframe(fout, relief="raised", bd="0") # ,background="#37D70F")
  foutwin   <- tkframe(fout)
  tkpack(fworkcmds,foutcmds,fill="x")
  tkpack(foutwin,fill="both",expand="yes")

  mbFile<-tkmenubutton(fhead, text="File", font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
,
                       relief="flat", width=10
) # alternativ groove
  mbEdit<-tkmenubutton(fhead, text="Edit", font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
,
                       relief="flat", width=10
)
  mbOptions<-tkmenubutton(fhead, text="Options", font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
,
                          relief="flat", width=10
)
  if(but.Wizardry){
    mbRevweb<-tkmenubutton(fhead, text="Wizardry", font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
,
                          relief="flat", width=10
)
  }

  f<-function(mess) function()set.tclvalue("tvmess",mess)
  tkbind(mbFile, "<Enter>", f("file operations and exit"))
  tkbind(mbEdit, "<Enter>", f("searching and other operations"))
  tkbind(mbOptions,  "<Enter>", f("change settings"))
  if(but.Wizardry)  tkbind(mbRevweb,  "<Enter>", f("process document and specials"))
  tkbind(mbFile, "<Leave>", function(){ set.tclvalue("tvmess","relax") })
  tkbind(mbEdit, "<Leave>", function(){ set.tclvalue("tvmess","relax") })
  tkbind(mbOptions, "<Leave>", function(){ set.tclvalue("tvmess","relax") })
  if(but.Wizardry)tkbind(mbRevweb, "<Leave>", function(){ set.tclvalue("tvmess","relax") })
  tkbind(finfo, "<Enter>", f("activate interactive icon"))
  tkbind(finfo, "<Leave>", function(){ set.tclvalue("tvmess","relax") })

  tkpack(mbFile,mbEdit,mbOptions,side="left")
  if(but.Wizardry)tkpack(mbRevweb,side="left")
  implement.but("Help.R", "fhead", "show online documentation of R object",job=fHelp.R)

  mbFile.menu<-tkmenu(mbFile,font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
  tkconfigure(mbFile, menu=mbFile.menu)

  mbEdit.menu<-tkmenu(mbEdit,font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
  tkconfigure(mbEdit, menu=mbEdit.menu)
  tkadd(mbEdit.menu, "command", command=ShowAboutRelax,
        label="ShowAboutRelax:   what's relax?")
  tkadd(mbEdit.menu, "command", command=ShowShortCuts,
        label="ShowShortCuts:   show short cuts for text field")
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=ReloadPlots,
        label="ReloadPlots:   reload jpeg-plot of text field")
  tkadd(mbEdit.menu, "command", command=ReloadReportWidget,
        label="ReloadReportWidget:   reconstruct REPORT WINDOW in case of strange appearances")
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=GoToLine,
        label="GoToLine:   go to line ... ")
  tkadd(mbEdit.menu, "command", command=FindRFns,
        label="FindRFns:   search R function by keyword")
  tkadd(mbEdit.menu, "command", command=FindReportText,
        label="FindReportText:   search text string in text field  (Crtl+F)")
  tkadd(mbEdit.menu, "command", command=FindReportChunk,
        label="FindReportChunk:   search code chunk in text field")
  tkadd(mbEdit.menu, "command", command=FindLaTeXSection,
        label="FindLaTeXSection:   search for \\section, \\subsection and \\subsubsection")
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=InsertLaTeXEnv,
        label="InsertLaTeXEnv:   insert LaTeX environment")

  mbOptions.menu<-tkmenu(mbOptions, font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
  tkconfigure(mbOptions, menu=mbOptions.menu)
  tkadd(mbOptions.menu,"command", command=SetOutputLength,
        label="SetOutputLength: define maximal lines of output")
  tkadd(mbOptions.menu, "separator")
  tkadd(mbOptions.menu,"command", command=SetFontType,
        label="SetFontType:   define font type")
  tkadd(mbOptions.menu,"command", command=SetFontSize,
        label="SetFontSize:   define font size")
  tkadd(mbOptions.menu, "separator")
  tkadd(mbOptions.menu,"command", command=SetPlotHeight,
        label="SetPlotHeight:  define height of plot (-> latex)")
  tkadd(mbOptions.menu,"command", command=SetPSDesignWidth,
        label="SetPSWidth:   define width of ps-graphics")
  tkadd(mbOptions.menu,"command", command=SetPSDesignHeight,
        label="SetPSHeight:  define height of ps-graphics")
  tkadd(mbOptions.menu,"command", command=SetPSRotation,
        label="SetPSRotation:  define non normal PS rotation")
  tkadd(mbOptions.menu,"command", command=SetJPGSize,
        label="SetJPGSize:  define size of jpeg-graphics")
  tkadd(mbOptions.menu, "separator")
  tkadd(mbOptions.menu,"command", command=ConfigRelax,
        label="Configure Relax: view or change parameters of relax")
  if(but.Wizardry){
   mbRevweb.menu<-tkmenu(mbRevweb,font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
   tkconfigure(mbRevweb, menu=mbRevweb.menu)
   tkadd(mbRevweb.menu,"command", command=ProcessReport,
        label="ProcessReport:  SaveReport, WeaveReport, and LatexReport")
   tkadd(mbRevweb.menu,"command", command=ViewReport,
        label="ViewReport:   show formated report")
   tkadd(mbRevweb.menu, "separator")
   tkadd(mbRevweb.menu,"command", command=LaTeX.head,
        label="LaTeX.head:   include simple LaTeX head")
   tkadd(mbRevweb.menu,"command", command=LatexReport,
        label="LatexReport:   format Latex file")
   tkadd(mbRevweb.menu,"command", command=ShowLogFile,
        label="ShowLogFile:   open Latex log file by editor")
   tkadd(mbRevweb.menu,"command", command=DvipdfReport,
        label="DvipdfReport:   translate dvi- in pdf-file")
   tkadd(mbRevweb.menu, "separator")
   tkadd(mbRevweb.menu,"command", command=WebReport,
        label="WebReport:   weave and tangle source file")
   tkadd(mbRevweb.menu,"command", command=WeaveReport,
        label="WeaveReport:   weave source file")
   tkadd(mbRevweb.menu,"command", command=WeaveReportNoCode,
        label="WeaveReportNoCode:   weave source file, hide code")
   tkadd(mbRevweb.menu,"command", command=WeaveReportNoText,
        label="WeaveReportNoText:   weave source file, hide text")
   tkadd(mbRevweb.menu,"command", command=TangleReport,
        label="TangleReport:   tangle source file")
   tkadd(mbRevweb.menu,"command", command=TangleReportChunk,
        label="TangleReportChunk:   tangle source file ask for root chunk")
   tkadd(mbRevweb.menu,"command", command=TangleReportNoComments,
        label="TangleReportNoComments:   tangle source file without added comment lines")
   tkadd(mbRevweb.menu,"command", command=ConstructDemoFunction,
        label="ConstructDemoFunction:   construct demo showing code chunks")
   tkadd(mbRevweb.menu, "separator")
   tkadd(mbRevweb.menu,"command", command=SWEAVE,
        label="SWEAVE:   save, Sweave and LaTeX file")
   tkadd(mbRevweb.menu,"command", command=SWEAVEB,
        label="SWEAVE:   save, Sweave (new process) and LaTeX file")
   ##tkadd(mbRevweb.menu, "separator")
   FTL.ok<-FALSE
   if( (version$os=="Win32" || version$os=="mingw32")
 ){
         libpath<-file.path(relax.path,"lib")
         if(file.exists(file.path(libpath,"pnmcrop.exe")) &&
            file.exists(file.path(libpath,"ppmtojpeg.exe")) &&
            file.exists(file.path(libpath,"netpbm.dll")) &&
            file.exists(file.path(libpath,"libjpeg.dll"))) FTL.ok<-TRUE
   }
   if(substring(version$os,1,5)=="linux"
){
      if(0<length(system("which convert",TRUE,TRUE)) &&
         0<length(system("which dvips",TRUE,TRUE))) FTL.ok<-TRUE
     }
   if(FTL.ok){
     tkadd(mbRevweb.menu, "separator")
     tkadd(mbRevweb.menu, "command", command=FormatTeXLines,
           label="FormatTeXLines:   format text chunk by LaTeX and insert  jpeg-file")
   }

   ##erstelle Menüeintrag für Funktionstasten-Puffer##
   ## tkadd(mbRevweb.menu, "separator")
   ## tkadd(mbRevweb.menu,"command", command=LoadRwtools,
       ## label="LoadR-wtools:   load some functions: stem.leaf, ... and some data sets")
  }

  tkadd(mbFile.menu,"command", command=SetWorkPath,
        label="SetWorkPath:   change working path")
  tkadd(mbFile.menu, "separator")
  tkadd(mbFile.menu,"command", command=OpenReport,
        label="OpenReport:   >>APPEND<< file to text field")
  tkadd(mbFile.menu,"command", command=SaveReport,
        label="SaveReport:   save text field to rev file")
  tkadd(mbFile.menu, "separator")
  tkadd(mbFile.menu,"command", command=SaveHtml,
        label="SaveHtml:   save text field to a rev file and a HTML file")

  tkadd(mbFile.menu,"command", command=ViewReport.html,
        label="ViewReport.html:   view html representation of report")
  tkadd(mbFile.menu,"separator")
  tkadd(mbFile.menu,"command", command=LoadEnvironment,
        label="LoadEnvironment:   load objects from dump file into relax environment")
  tkadd(mbFile.menu,"command", command=DumpEnvironment,
        label="DumpEnvironment:   save objects of relax environment as dump file")
  tkadd(mbFile.menu,"command", command=SaveEnvironment,
        label="SaveEnvironment:   save objects of relax environment in binary file")
  tkadd(mbFile.menu,"command", command=CleanEnvironment,
        label="CleanEnvironment:   delete objects of environment")
  tkadd(mbFile.menu,"separator")
  tkadd(mbFile.menu,"command", command=OpenTextFile,
        label="OpenTextFile:  open not-rev-file, translate console style and >>APPEND<< it to text field")
  tkadd(mbFile.menu,"command", command=SaveAsTextFile,
        label="SaveAsTextFile:   dump report in console style, and as .script.R-file, but not as a rev-file")
  tkadd(mbFile.menu,"separator")

  if(but.Wizardry){
    tkadd(mbFile.menu,"command", command=OpenRevbook,
        label="OpenCompbook:   load compbook from library relax/rev")
    tkadd(mbFile.menu, "separator")
  }
  tkadd(mbFile.menu, "command", label="Exit:   quit RELAX",
        command=Exit)


  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=EditReport,
        label=paste("EditReport:   use editor",editor.sys,"for editing"))
  tkadd(mbEdit.menu, "command", command=DumpCodeChunk,
        label=paste("DumpCodeChunk:   save code chunk in a file"))
  tkadd(mbEdit.menu, "command", command=CopyToEnd,
        label=paste("CopyToEnd:   copy output to end of text"))
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=RunAll,
        label="RunAll:   run all start- and *-code chunks")
  tkadd(mbEdit.menu, "command", command=RunStart,
        label="RunStart:   run all start-code chunks")
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=DeleteAll,
        label="DeleteAll:   clear text field")
  tkadd(mbEdit.menu, "separator")
  tkadd(mbEdit.menu, "command", command=UnDo,
        label="UnDo:   load report-UnDo-bak.rev (report before last evaluation)")

  melde("frame head defined",3)

  implement.but("TrashROutput", "fworkcmds", "remove output the mouse is pointing at from report",job=fTrashROutput) ## 071115
  implement.but("WarnEval",   "fworkcmds", "evaluate code chunk of cursor even if warnings occur",job=fWarnEval)
  implement.but("EvalRCode",   "fworkcmds", "evaluate code and stop in case of warnings",job=fEvalRCode)
  implement.but("PlanRCode",    "fworkcmds", "insert new code chunk",job=fPlanRCode)

  tkpack(tklabel(fworkcmds, text=" Report:",
         pady="-3"
,font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
),
         # ipady=7,
         side="left")
  lworkname.sys<-tklabel(fworkcmds, text=workname.sys,
                      pady="-3"
, relief="ridge")
  tkpack(lworkname.sys, side="left")
  llineno.sys<-tklabel(fworkcmds, text=":",width="4")
  tkpack(llineno.sys, side="left")

  implement.but("Down",   "fworkcmds", "jump forwards",side="left",bwf=0.40,job=fDown)
  implement.but("Up",   "fworkcmds", "jump backwards",side="left",bwf=0.40,job=fUp)

  implement.but("RemoveOut",foutcmds,"remove output from output field",job=fRemoveOut) ## 071115
  implement.but("FindText",foutcmds,"find some text string in report field",job=fFindText)
  implement.but("SavePlot",foutcmds,
            "save plot as jpeg and postscript file and insert it into report field",job=fSavePlot)
  implement.but("Insert",foutcmds,"insert output into report",job=fInsert)

  tkpack(tklabel(foutcmds, text=" Result(s):",pady="-3"
,
          font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
), 
          #ipady=7, 
          side="left")


           ##implementiere Testknopf##
  tworkwin<-tktext(fworkwin, background="#f7fffF", font=tfont.sys) #cff#d0f0ff#c81e6ff23#f0fffF
  try(tkconfigure(tworkwin, undo=1)) # 050704
  workbar<-tkscrollbar(fworkwin)
  tkconfigure(tworkwin,yscrollcommand=function(...) tkset(workbar,...))
  tkconfigure(workbar, command=function(...) tkyview(tworkwin,...))
  tkpack(workbar ,side="right",fill="y")
  tkpack(tworkwin,fill="both",expand="yes")
  tkinsert(tworkwin,"0.0",paste("% New Report:",date(),"\n"))
  ### CLIPBOARD-Funktion PASTE a la windows / clipboard
  if(substring(version$os,1,7)=="darwin8" ){
    # mac-PASTE, ctrl-v
    mac.paste<-function(...){
       try({.Tcl("clipboard append hello")
              .Tcl("clipboard clear")
              news<-base::scan(file=pipe("pbpaste","r"),what="",
                                           sep="\n",blank.lines.skip=FALSE)
              tkinsert(tworkwin,"insert",paste(news,collapse="\n"))})
              tclvalue(tkyview(tworkwin,"scroll","-1","pages"))
    }
    ### tkevent.add("<<Paste>>", "<Control_L><v>")
    tkbind(tworkwin,"<Control_L><v>",mac.paste)
    # mac-COPY
    mac.copy<-function(...){  ## Ctrl-C
       news<-""
       try(news<-tclvalue(.Tcl("if {[catch {clipboard get}]} {set aa empty} {set aa full}")))
       if(news=="empty") return()
       try({news<-tclvalue(.Tcl("set aaa [selection get -selection CLIPBOARD]"))
              base::cat(news,file=get("tmp.file.name",env=revive.sys)
)
              system(paste("pbcopy < ",get("tmp.file.name",env=revive.sys)
))
              .Tcl("clipboard append hello")
              .Tcl("clipboard clear")})
    }
    tkbind(tworkwin,"<Control_L><c>",mac.copy)
    # mac-extract
    tkevent.add("<<extract>>",  "<Control_L><c><KeyRelease>")
    tkbind(tworkwin,"<<extract>>",mac.copy)
  }else{
    tkevent.add("<<Paste>>",   "<Control_L><v>")
    tkbind(tworkwin,"<<Paste>> { catch {%W insert insert [selection get -selection CLIPBOARD] } }")
  }
  toutwin<-tktext(foutwin,height=8,background="#ffffee", font=outfont.sys) #fff080 #fc8f16a23#ffffcc
  outbar<-tkscrollbar(foutwin)
  tkconfigure(toutwin,yscrollcommand=function(...) tkset(outbar,...))
  tkconfigure(outbar,        command=function(...) tkyview(toutwin,...))
  tkpack(outbar ,side="right",fill="y")
  tkpack(toutwin,fill="both",expand="yes")
  if((version$os=="Win32" || version$os=="mingw32")
) #{}
  { # 110505
  # fout<-get("fout",env=revive.sys); toutwin<-get("toutwin",env=revive.sys)
   tkbind(fout,"<Configure>","")
   config.fns<-function(...){
    if("1"==tclvalue(tkwinfo("ismapped",toutwin))){
        tkpack("forget",toutwin) #; tkbind(toutwin,"<Configure>","")
        Sys.sleep(.01)
        if("0"==tclvalue(tkwinfo("ismapped",toutwin))){
          tkpack(toutwin,fill="both",expand="yes")
        }
    }
   }
   tkbind(fout,"<Configure>",config.fns)
  }

  ( # def. namenslose Funktion:
  function(tworkwin){
   tktag.configure(tworkwin,"tld",   foreground="#C8162315126C", relief="raised",
                   borderwidth="2") # alternativ: #aaa222111
   tktag.configure(tworkwin,"tex",   background="#fffffffff", relief="raised",
                   borderwidth="2")
   tktag.configure(tworkwin,"code",  foreground="#d21", font=outfont.sys) # ddd222222
   tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
   tktag.configure(tworkwin,"emph", background="#999999999")
   tktag.configure(tworkwin,"jpeg", background="#ffffee",borderwidth="2",relief="raised")
   proc<-paste(
    "proc emphline {w mustera musterz} {",
      "scan [$w index end] %d anzzeilen", "set emphline 0\n",
      "for {set i 1} {$i < $anzzeilen} {incr i} {",
        "set actline [$w get $i.0 $i.end]",
        "if {[regexp $mustera $actline]} {",
          "if {[regexp $musterz $actline]} {",
            "uplevel $w tag add emph $i.0 $i.end",
          "}",
        "}",
      "}",
    "}", # type-semantics: 0=text, 1=tld, 2=code, 3=output
    "proc marklinetypes {w} {",
      "scan [$w index end] %d anzzeilen",
      "set type 0\n",
      "for {set i 1} {$i < $anzzeilen} {incr i} {",
       "set zeile [$w get $i.0 $i.end]",
       "set iv [ expr $i-1 ]",
       "if {$type==1}                           {\n set type 2\n}",
       "if {[regexp \"^@\"             $zeile]} {\n set type 0\n}",
       "if {[regexp \"^<<.*>>=\"     $zeile]} {\n set type 1\n}",
       "if {[regexp \"^\\\\\\\\s(u|e)\" $zeile]} {\n set type 4\n}",
       "if {[regexp \"^......img src\"  $zeile]} {\n set type 5\n}",
       "if {[regexp \"^output-start\"  $zeile]}  {\n set type 3\n}",
       "if {[regexp \"^\\\\\\\\begin\\{verbatim\\}\" $zeile]} {\n set type 3\n}",
       # "if {$type==2} {\n uplevel $w tag add code    $i.0 $i.end \n}",
       "if {$type==2} {\n uplevel $w tag add code    $iv.end $i.end \n}",
       # "if {$type==1} {\n uplevel $w tag add tld     $i.0 $i.end \n}",
       "if {$type==1} {\n uplevel $w tag add tld     $i.0 $i.end ",
                                                   "; set type 2 \n}",
       ### "if {$type==1} {\n uplevel $w tag add tld     $i.0 $i.end \n ; ",
       ###                      " \n uplevel $w tag add code $i.end $i.end \n ; ",
       ###                                            "; set type 2 \n } ",
       "if {$type==3} {\n uplevel $w tag add output  $i.0 $i.end \n}",
       "if {$type==4} {\n uplevel $w tag add tex     $i.0 $i.end",
                                                   "; set type 0 \n}",
       "if {$type==5} {\n uplevel $w tag add jpeg    $i.0 $i.end",
                                                   "; set type 0 \n}",
       "if {[regexp \"^output-end\"  $zeile]} {\n set type 0\n}",
       "if {[regexp \"^\\\\\\\\end\\{verbatim\\}\" $zeile]} {\n set type 0\n}",
      "}",
    "}",
    "proc markclear w {",
      "$w tag remove tld    1.0 end",
      "$w tag remove tex    1.0 end",
      "$w tag remove code   1.0 end",
      "$w tag remove output 1.0 end",
    "}", sep="\n")
    .Tcl(proc)
   tktag.bind(tworkwin,"jpeg","<Enter>", #081121
              function() set.tclvalue("tvmess","click button to display jpeg by browser!"))
   
   tktag.bind(tworkwin,"jpeg","<Leave>",function(){ set.tclvalue("tvmess","relax") })
   tktag.bind(tworkwin,"jpeg","<ButtonRelease>",
     function(){
       line <-floor(as.numeric(tkindex(tworkwin,"insert")))

       res<-tkmessageBox(message=
               if(language=="german") 
                  paste("jpeg-Datei im Browser angezeigen?")
               else paste("show jpeg graphics by browser"),
               title="Display JPEG?",icon="warning",type="yesnocancel",default="yes")
       res<-tclvalue(res); if(res=="cancel"||res=="no") return()
       fname<-tclvalue(tkget(tworkwin,paste(line,"0",sep="."),paste(line,"40",sep=".")))
       fname<-sub("^.*img src..","",fname)
       fname<-sub("(.jpg).*$","\\1",fname)
       browser.sys<-get("browser.sys",env=revive.sys)
       cat(fname, "will be displayed by browser in a few seconds!\n")
       if( (version$os=="Win32" || version$os=="mingw32")
 ){
         browser.exe<- if(browser.sys=="") "start " else browser.sys
             res<-try(shell(paste(browser.exe, fname),wait=FALSE))  
         if(res!=0){ cat("ERROR: browser hasn't been started successfully \n") }
       } else {
         if(browser.sys!=""){
                try(system(paste(browser.sys,fname),wait=FALSE))
         }
       }
       "relax"
    }
  )
  }  # und Aufruf der namenslosen Funktion:
  )(tworkwin)

  linfo.name <- tklabel(finfo, text=" ", font="-Adobe-helvetica-Medium-R-Normal--12-140-*" # ,foreground="#124800"
)
  linfo      <- tklabel(finfo, text=" ", textvariable="tvmess",
                        width=80
, relief="ridge")
  tkpack(linfo.name,linfo,side="left",fill="x")

  tkbind(linfo,"<ButtonPress>",function(){
                                 res<-tkmessageBox(message=if(language=="german") "Soll das interaktive Icon aktiert werden?"
                                           else "Do you want to activate interactive icon?",
                                                   title="RELAX-ICON",icon="warning",type="yesnocancel",default="no")
                                 if("externalptr"==mode(res))  res<-tclvalue(res)
                                 if(res=="cancel") return()
                                 if(res=="no") return()

                               chair<-function(xcenter=1.2,xspread=1,ycenter=1.2,yspread=1,lwd=4){
                                 # cat("chair: ", xcenter,xspread,ycenter,yspread)
                                 x<-seq(0,1.6,length=(n<-40))
                                 xshift<-.5;ystretch<-1.2; height<-.4/yspread
                                 y<-0.5*cosh(ystretch*x-xshift);
                                 y<-scale(y-y[1]-.06,-ycenter,1/yspread)
                                 x<-scale(x/1.6,-xcenter,1/xspread)
                                 segments(x[1],y[1],x[1]*.1+x[n]*.9,y[1]-height,lwd=lwd)
                                 segments(x[1],y[1]-height,x[n],y[n],lwd=lwd)
                                 lines(x,y,lwd=lwd)
                               }
                               tree<-function(xcenter=.51,xspread=1.5,ycenter=0,yspread=2,lwd=3,n=5,n.l=10){
                                 # cat(xcenter,xspread,ycenter,yspread)
                                 yspread<-yspread/2.5; ycenter<-ycenter+.15
                                 leaf<-function(von,start,delta,len,n.l=10,krum.par){
                                  xy.mat<-matrix(von,2,n.l)
                                  h<-start*2*pi/360
                                  xy.delta<-c(cos(h),sin(h))/n.l
                                  delta<-delta*2*pi/360*len/n.l
                                  for(i in 2:n.l){
                                   w<-delta
                                   xy.delta<-matrix(c(cos(w),-sin(w),sin(w),cos(w)),2,2)%*%xy.delta
                                   xy.mat[,i]<-xy.mat[,i-1]+xy.delta
                                  }
                                  delta.y<-qbeta((0:n.l)/n.l,2,3)
                                  delta.y<-.04*cumsum(sin((0:(n.l-1))/(n.l)*2*pi))
                                  xy<-t(xy.mat) # +von-xy.mat[,1])
                                  xy[,1]<-scale(xy[,1]*xspread/2,-xcenter,1)#2/xspread)
                                  xy[,2]<-(xy[,2]*yspread+ycenter)
                                  delta.y<-(delta.y*yspread)
                                  #lines(xy[,1],xy[,2],lwd=3) #lines(xy[,1],xy[,2]+delta.y,lwd=3)
                                  polygon(c(xy[,1],rev(xy[,1])),c(xy[,2],rev(xy[,2]+delta.y)),col="green",border=NA,xpd=NA)
                                 }
                                 x<-seq(-50,90,length=floor(n/2))
                                 for(i in x){
                                   leaf(von=c(0,0),start=i,delta=5,len=10,n.l=n.l)
                                   x<-seq(-50,90,length=ceiling(n/2))
                                 }
                                 for(i in x) leaf(von=c(0,0),start=190-i,delta=-5,len=10,n.l=n.l)
                                 segments(xcenter-0.02*xspread,ycenter,
                                          xcenter-0.05*xspread,ycenter-1.5*yspread,lwd=1)
                                 segments(xcenter+0.02*xspread,ycenter,
                                          xcenter+0.05*xspread,ycenter-1.5*yspread,lwd=1)
                               } # end of tree
                               redo<-function(...){
                                 ytrsc<-xtrsc<-slider(no=1); xchsc<-slider(no=2)
                                 ychsc<-slider(no=3);hori<-slider(no=4);n.leafs<-slider(no=5)
                                 leaf.sty=slider(no=6)
                                 h<-slider(obj.name="tree.center"); xtrcenter<-h[1]; ytrcenter<-h[2]
                                 h<-slider(obj.name="chair.center"); xchcenter<-h[1]; ychcenter<-h[2]
                                 a<-par(family="mono")
                                 plot(-2:2,-2:2,type="n",axes=FALSE,xlab="",ylab=""); # text(.6,1.8,"relax",cex=4)
                                 polygon(2*c(-2,-2,2,2),c(hori,-3,-3,hori),col="#fcf1a2",border=NA,xpd=NA)
                                 polygon(2*c(-2,-2,2,2),c(hori,3,3,hori),col="#c8e6f2",border=NA,xpd=NA)
                                 text(0.5,-.5,"relax",cex=4)
                                 tree(xcenter=xtrcenter,xspread=xtrsc,n=n.leafs,n.l=leaf.sty,
                                      ycenter=ytrcenter,yspread=ytrsc,lwd=3)
                                 chair(xcenter=xchcenter,xspread=xchsc,
                                       ycenter=ychcenter,yspread=ychsc,lwd=3)
                                 par(family=a)
                               }
                                 slider(obj.name="tree.center",obj.value=c(-1.1,.8))
                                 slider(obj.name="chair.center",obj.value=c(.3,-1.2))
                                 set.tree<-function(...){
                                   # cat("click on desired position in graphics device!!!\n")
                                   # xy<-unlist(locator(n=1)); xy<-c(xy$x,xy$y)
                                   xy<-c(runif(1,-1,1),runif(1,-1.5,1.0))
                                   slider(obj.name="tree.center",obj.value=xy)
                                   redo()
                                 }
                                 set.chair<-function(...){ #   xy<-locator(n=1); xy<-c(xy$x,xy$y)
                                   xy<-c(runif(1,-1,1),runif(1,-1.5,0))
                                   slider(obj.name="chair.center",obj.value=xy)
                                   redo()
                                 }
                                 slider(redo,
                                   c("tree scale factor", "x scale chair",
                                     "design of chair", "size of beach",
                                     "number of leafs", "style of leafs"),
                                   c(.1,.1,.1,-2,7,5),c(5,2.4,2.4,2,17,30),
                                   c(.1,.1,.1,.1,1,1),c(3.5,1.2,1.2,-0.7,9,12),
                                   list(set.tree,set.chair),
                                   but.names=c("tree location","new chair location")
                                 )
                                 "relax"
                               }
)
  einfo.tmp <- tkentry(finfo,textvariable="tvinfo",background="#7ff",
                       width=floor(as.numeric(80
)*3/5))
  linfo.tmp <- tklabel(finfo)

  melde("frame info defined",3)

  tkbind(TopW, "<<FindReportText>>", FindReportText)
  tkbind(TopW,"<F3>", function(){  # 050628
        set.tclvalue("tvmess","relax")
        if(!exists("string.sys",env=revive.sys)) return()
        such<-get("string.sys",env=revive.sys)
        if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
        tworkwin<-get("tworkwin",envir=revive.sys)
        worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
        if(nchar(worktext)<10000){
          worktext<-strsplit(worktext,"\n")[[1]]
        }else{
          base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
          worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
        }

        if(nchar(such)==0) {
          tcl("findclear",tworkwin); tkfocus(tworkwin); return()
        }
        repl.pat<-gsub("(.)","\\\\\\1","^!$%&/()=?{}}+*#,.-;:\\_[") ## 070830
        repl.pat<-paste("([",repl.pat,"])",collapse="")
        such<-gsub(repl.pat,"\\\\\\1",such)
        if(length(found<-grep(such,worktext))>0){
          line <- floor(as.numeric(tkindex(tworkwin,"insert")))
          line <- if(any(found>line)) found[found>line][1] else found[1]
          tksee(tworkwin,paste(line,".1",sep=""))
          h<-worktext[line]
          h<--1+charmatch(such,substring(h,1:nchar(h))); if(is.na(h)) h<-0 # 060605
          h<-paste(line,".",h,sep="")
          tkmark.set(tworkwin, "insert", h); tkfocus(tworkwin)
        } else set.tclvalue("tvmess",paste("Warning: search string >",such,"< not found!!!"))
      } # end of function
  )


  tkbind(TopW, "<<GoToLine>>", GoToLine)

  tkbind(TopW, "<<ProcessReport>>", ProcessReport)

  # tkconfigure(Help.R,command=fHelp.R)
  tkbind(TopW, "<<Help.R>>", fHelp.R)
  # tkconfigure(Up,command=fUp)
  tkbind(TopW, "<<Up>>", fUp)
  # tkconfigure(Down,command=fDown)
  tkbind(TopW, "<<Down>>", fDown)

  # tkconfigure(PlanRCode,command=fPlanRCode)
  tkbind(TopW, "<<PlanRCode>>", fPlanRCode)
  # tkconfigure(EvalRCode,command=fEvalRCode) ## 071115
  tkbind(TopW, "<<EvalRCode>>", fEvalRCode)
  # tkconfigure(WarnEval,command=fWarnEval)
  tkbind(TopW, "<<WarnEval>>", fWarnEval)
  # tkconfigure(TrashROutput,command=fTrashROutput)
  tkbind(TopW, "<<TrashROutput>>", fTrashROutput)

  # tkconfigure(Insert,command=fInsert)
  tkbind(TopW, "<<Insert>>", fInsert)
  # tkconfigure(SavePlot,command=fSavePlot)
  tkbind(TopW, "<<SavePlot>>", fSavePlot)
  # tkconfigure(FindText,command=fFindText)
  tkbind(TopW, "<<FindText>>", fFindText)
  # tkconfigure(RemoveOut,command=fRemoveOut) ## 071115
  tkbind(TopW, "<<RemoveOut>>", fRemoveOut)

  #tkconfigure(CopyToEnd,command=CopyToEnd)
  #tkbind(TopW, "<<CopyToEnd>>", CopyToEnd)

  tkbind(TopW, "<<SaveReport>>", SaveReport)   ##implementiere Eigenschaften vom Testknopf##
  twin<-tworkwin; f.sonderzeichen<-function(zeichen){
                    function(){
                      tkinsert(twin,"insert",zeichen)
                      if((version$os=="Win32" || version$os=="mingw32")
)tkdelete(twin,"insert-1chars")
                      if(substring(version$os,1,7)=="darwin8")tkdelete(twin,"insert-1chars")
                    }
                  }
                  f.umlaut<-function(zeichen){
                    function(){
                      return()
                      #char337<-eval(parse(text='"\\337"'))
                      #if(zeichen==char337 & tclvalue(tkget(twin,"insert-1chars","insert"))=="\\") return()
                      #tkinsert(twin,"insert",zeichen); tkdelete(twin,"insert-2chars")
                    }
                  }
                  tkbind(twin,"<<LKeckig>>", f.sonderzeichen("["))
                  tkbind(twin,"<<RKeckig>>", f.sonderzeichen("]"))
                  tkbind(twin,"<<Tilde>>",   f.sonderzeichen("~"))
                  tkbind(twin,"<<LKgeschw>>",f.sonderzeichen("{"))
                  tkbind(twin,"<<RKgeschw>>",f.sonderzeichen("}"))
                  tkbind(twin,"<<Klammera>>",f.sonderzeichen("@"))
                  tkbind(twin,"<<Pipe>>",    f.sonderzeichen("|"))
                  tkbind(twin,"<<Backsl>>",  f.sonderzeichen("\\"))
                  renewhighlighting<-function(){
                    tworkwin<-get("tworkwin",env=revive.sys)
                    melde("ak texthervor",1)
                    tcl("markclear",tworkwin)
                    tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                    tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                    tcl("marklinetypes",tworkwin)
                    melde("ak texthervor",2)

                  }
                  # tkbind(twin,"<<Klammeraffe>>",renewhighlighting)
                  tkbind(twin,"<Return>",renewhighlighting)

  ##definiere Wirkung von [[Strg Pagedown]] im Reportfenster## ab 1.02 abgeschaltet
  proc<-c(  # 060704
     "proc showbracket {w orta ortb} {",
     "$w tag remove secondbracket    1.0 end",
     "  scan [$w index end] %d numLines",
     "     $w mark set first \"$orta\"",
     "     $w mark set last \"$orta  + 1 chars\"",
     "     uplevel [$w tag add secondbracket first last]",
     "     $w mark set first \"$ortb\"",
     "     $w mark set last \"$ortb  + 1 chars\"",
     "     uplevel [$w tag add secondbracket first last]",
     "}",
      "proc showbracketsclear w {",
        "$w tag remove secondbracket    1.0 end",
      "}"
    )
  .Tcl(paste(proc,collapse="\n"))
  tktag.configure(tworkwin,"secondbracket",background="#000dddfff",relief="raised")
  MarkSecondBracket<-function(){
    tworkwin<-get("tworkwin",env=revive.sys)
    left.of.cursor<-tclvalue(tkget(tworkwin,"insert - 1 chars"))
    open<-0
    if(left.of.cursor %in% c("{","[","(")){ ## print("Klammer auf")
      open<-1
      a<-tclvalue(tkget(tworkwin,"insert - 1 chars","end"))
      n<-min(nchar(a),1500); aa<-substring(a,1,n); aaa<-substring(aa,1:n,1:n)
      auf<-aaa %in% c("{","[","("); zu<-aaa %in% c("}","]",")")
      count<-cumsum(auf)-cumsum(zu)
      second<-which(count==0)[1]-2
      first<-paste(tclvalue(tkindex(tworkwin,"insert")),"- 1 chars")
      if(!is.na(second)){
        tcl("showbracket",tworkwin,first,paste("insert +",second,"chars"))
      }
    }
    if(left.of.cursor %in% c("}","]",")")){ ## print("Klammer zu"); 
      open<- -1
      a<-tclvalue(tkget(tworkwin,"0.0","insert"))
      n<-nchar(a); aa<-substring(a,max(1,n-1500),n)
      aaa<-substring(aa,nchar(aa):1,nchar(aa):1) 
      auf<-aaa %in% c("{","[","("); zu<-aaa %in% c("}","]",")")
      count<-cumsum(zu)-cumsum(auf)
      second<-which(count==0)[1]
      first<-paste(tclvalue(tkindex(tworkwin,"insert")),"- 1 chars")
      if(!is.na(second)){
        tcl("showbracket",tworkwin,first,paste("insert -",second,"chars"))
      }
    }
    ## if(exists("second")) print(second)  ##; print(open)
    if(open==0) tcl("showbracketsclear",tworkwin)
    ### Zeilennummeraktualisieren: 
    line <-floor(as.numeric(tkindex(tworkwin,"insert")))

    tkconfigure(llineno.sys,text=paste(":",line))

  }
  tkbind(tworkwin,"<KeyRelease>",MarkSecondBracket)
  ### Zeilennummeraktualisieren falls Mouse-Op: 
  tkbind(tworkwin,"<ButtonRelease>",function(){
      tworkwin<-get("tworkwin",revive.sys)
      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

      tkconfigure(llineno.sys,text=paste(":",line))
    }
  )

   tkbind(tworkwin,"<Tab><KeyRelease>", function() {
    #  tworkwin<-get("tworkwin",env=revive.sys)
    try(tkdelete(tworkwin,"insert - 1 char"))
    if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
    tworkwin<-get("tworkwin",envir=revive.sys)
    worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
    if(nchar(worktext)<10000){
      worktext<-strsplit(worktext,"\n")[[1]]
    }else{
      base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
      worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
    }

    line<-line.orig<-(tclvalue(tkindex(tworkwin,"insert")))
    pos<-as.numeric(sub("(.*)\\.","",line))
    line<-as.numeric(line) 
    w<-worktext[floor(line)]
    anfang<-paste(rev(substring(w,1:pos,1:pos)),collapse="")
    anfang<-sub("(^[a-zA-Z0-9._]*)(.*)","\\1",anfang)
    anfang<-paste(rev(substring(anfang,
          1:nchar(anfang),1:nchar(anfang))),collapse="")
    suchstring<-paste("^",anfang,sep="")
    objekt<-grep(suchstring,ls(env=revive.env),ignore.case=FALSE,value=TRUE)
    objekt<-c(objekt,apropos(suchstring,ignore.case=FALSE))
    if(length(objekt)==0) return();  if(length(objekt)>20) return()
    if(length(objekt)==1){
      objtail<-sub(suchstring,"",objekt)
      objtail<-substring(objekt,nchar(suchstring)) # 1+(nchar(.)-1) 
      try(tkinsert(tworkwin,line.orig,objtail));    return()
    }
    ooo<-sapply(objekt,function(x) substring(x,1:20,1:20))
    n.equal<-which.min(apply(ooo[,1]==ooo,1,all))-1
    if(n.equal>nchar(anfang)){
      objekt<-paste(ooo[1:n.equal,1],collapse="")
      objtail<-sub(suchstring,"",objekt)
      try(tkinsert(tworkwin,line.orig,objtail));    return()
    } else { print(objekt); return() }
   })
   if( name.complete.sys!=TRUE ) {
     tkbind(tworkwin,"<Tab><KeyRelease>", function() { "relax" } ) 
   }

  ##definiere Funktionstasten-Puffer##
  Execute.cmds<-function(){
    melde("Execute.cmds",1)
    res<-ls(pattern="^cmds$",envir=revive.env)
    if(length(res)>0){
      cmds<-try(eval(parse(text="cmds"),envir=revive.env))
      if(length(cmds)>0){
        eval(parse(text="cmds<-NULL"),envir=revive.env)
        #repeat{
          melde("beginn repeat",1)
          if(length(cmds) == 0) break
          cmd <- substring(cmds[1],1,1); choice<-substring(cmds[1],2)
          cmds<-cmds[-1]; assign("cmds",cmds,env=revive.env)
          switch(cmd
            ,"s" = {
                     if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
                     tworkwin<-get("tworkwin",envir=revive.sys)
                     worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
                     if(nchar(worktext)<10000){
                       worktext<-strsplit(worktext,"\n")[[1]]
                     }else{
                       base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
                       worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
                     }

                     line<-grep("^<<(.*)>>=",worktext)
                     if(class(try(no<-line[as.numeric(choice)]))!="try-error" && !is.na(no)){
                       line<-paste(no[1],"0",sep=".")
                       tkmark.set(tworkwin,"insert",line);fWarnEval()
                       tksee(tworkwin,"end")
                     }
                    }
            ,"q" = { Exit() }
            ,">" = {
                    news<-paste("\n@\n<<*>>=\n",choice,"",sep="")
                    if(!exists("tworkwin"))
                      tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

                    pos.to.insert<-"end"
                    if(0<length(grep("output-start",news))){
                      tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
                      ltail<-length(tail)
                      if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                         any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                         news<-sub(".*output-start\n","",news)
                         news<-sub("output-end","",news)
                         h<-seq(along=h)[h][1]
                         pos.to.insert<-paste("end -",h,"lines")
                      }
                    }
                    try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
                    tksee(tworkwin,"end - 0 lines")
                    melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

                     tkmark.set(tworkwin,"insert","end");fWarnEval()
                     tksee(tworkwin,"end")
                   }
            ,"p" = {
                    psname <- choice
                    if(!is.null(bildname)&&nchar(bildname)>0){
                      n<-nchar(bildname<-gsub(" ","",bildname))
                      bildname<-sub(".ps$","",bildname)
                      if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
                      tworkwin<-get("tworkwin",envir=revive.sys)
                      worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
                      if(nchar(worktext)<10000){
                        worktext<-strsplit(worktext,"\n")[[1]]
                      }else{
                        base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
                        worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
                      }

                    # Postscript:
                      psname <-paste(bildname,".ps", sep="")
                      news<-paste("@\n \\begin{center}","\\includegraphics[",
                                           "height=",psheight.sys,"]{",bildname,"}\\end{center}\n",sep="") #081121
                      try.res<-try({dev.copy(postscript,psname,horizontal=pshorizontal.sys,
                                           width=psdesignwidth.sys,height=psdesignheight.sys);dev.off()})
                      if(is.function(try.res)){
                        ok <- "OK"
                      } else {
                        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
                        ok<-try.res[1]
                        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
                        if(!is.character(ok)) { ok <- "OK" }
                      }
                      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
                        ok<-FALSE
                        cat(error.msg<-unclass(try.res),"\n")
                        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
                           cat("A warning message stopped the evaluation!",
                                 "If you want to\nevaluate the code anyway",
                                 "evaluate code by:\n>WarnEval<")
                        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
                      } else { ok<-TRUE }


                      if(!ok) cat("Error: *ps file not generated by dev.copy!!!\n")
                    # jpeg:
                      jpgname<-paste(bildname,".jpg",sep="")
                      news<-paste(news,'\n% <p><img src="',jpgname,'">\n@\n', sep="" )
                      if((version$os=="Win32" || version$os=="mingw32")
){ # width=width in pixel, 72 dpi
                        try.res<-try({dev.copy(jpeg,jpgname,width=jpgdesignsize.sys*72,
                              height=jpgdesignsize.sys*72,quality=100,pointsize=7);dev.off()})
                      }else{
                        try.res<-try({dev.copy(bitmap,type="jpeg",jpgname,
                             width=jpgdesignsize.sys,height=jpgdesignsize.sys);dev.off()})
                      }
                    # gif:
                      if(substring(version$os,1,7)=="darwin8" ){
                        gifname<-sub("jpg$","gif",jpgname) 
                        try.res<-try({system(paste("convert",jpgname,gifname))})
                      }
                      if(is.function(try.res)){
                        ok <- "OK"
                      } else {
                        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
                        ok<-try.res[1]
                        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
                        if(!is.character(ok)) { ok <- "OK" }
                      }
                      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
                        ok<-FALSE
                        cat(error.msg<-unclass(try.res),"\n")
                        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
                           cat("A warning message stopped the evaluation!",
                                 "If you want to\nevaluate the code anyway",
                                 "evaluate code by:\n>WarnEval<")
                        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
                      } else { ok<-TRUE }


                      if(!ok) cat("Error: jpg of gif file not generated by dev.copy!!!\n")
                      ##hole ggf. [[tworkwin]]>>
                      line <-floor(as.numeric(tkindex(tworkwin,"insert")))

                      ##lese Arbeitsfenster auf [[worktext]] ein>>
                      textstart<-grep("^@",worktext)-1; textstart<-textstart[textstart>=line][1]
                      codestart<-grep("^<<(.*)>>=",worktext)-1; codestart<-codestart[codestart>=line][1]
                      if(is.na(codestart))codestart<-Inf; if(is.na(textstart))textstart<-Inf
                      insertline<-if(codestart==textstart) NA else min(codestart,textstart)
                      anzrows<-length(unlist(strsplit(news,"\n")))
                      if(is.na(insertline)){
                          insertline<-"end"
                          try(tkinsert(tworkwin,"end","\n"))
                          try(tkinsert(tworkwin,"end",paste(news,collapse="\n")))
                          tkmark.set(tworkwin, "insert","end - 2 lines")
                          tksee(tworkwin,"end")  # paste(insertline+anzrows,"0",sep="."))
                          insertline<-length(worktext)
                      }else{
                        # in einem Text-Chunks muss ein Kl-Affe eingebaut werden.
                          if(length(grep("<<\\*>>=",news[1]))>0 && codestart < textstart) news<-c(news,"@\n")
                          try(tkinsert(tworkwin,paste(insertline+1,"0",sep="."),paste(news,collapse="\n")))
                          tkmark.set(tworkwin, "insert", paste(insertline+anzrows,"0",sep="."))
                          tksee(tworkwin,paste(insertline+anzrows,"0",sep="."))
                      }
                      ## melde(insertline)
                      melde("ak texthervor",1)
                      tcl("markclear",tworkwin)
                      tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                      tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                      tcl("marklinetypes",tworkwin)
                      melde("ak texthervor",2)

                      ##zeige Bilder im Textfenster an##
                      tkfocus(tworkwin)
                      melde("inserted characters: \n",3,substring(news[1:min(7,length(news))],1,80))

                      insertline<-insertline+3
                      melde(paste("p", psname), "cmd.msg")
                    }
                   }
            ,"r" = {
                    choice<-gsub(" ","",choice)
                    if(!file.exists(choice)){
                      cat("ERROR:",choice,"not found!!!\n")
                      ok<-FALSE
                    }else{
                      filename<-choice
                      try.res<-try(myscan(filename,"",sep="\n",blank.lines.skip=FALSE))
                      if(is.function(try.res)){
                        ok <- "OK"
                      } else {
                        if(mode(try.res)=="externalptr"||mode(try.res)=="environment") try.res<-"ok"
                        ok<-try.res[1]
                        if(is.null(ok) ||is.na(ok)|| is.name(ok) || is.list(ok) || is.numeric(ok)) ok <- "OK"
                        if(!is.character(ok)) { ok <- "OK" }
                      }
                      if(0!=length(ok)&&("Error"==substring(ok,1,5) | "Fehler"==substring(ok,1,6))){
                        ok<-FALSE
                        cat(error.msg<-unclass(try.res),"\n")
                        if(0<length(grep("Warnung",error.msg))||0<length(grep("warning",error.msg)))
                           cat("A warning message stopped the evaluation!",
                                 "If you want to\nevaluate the code anyway",
                                 "evaluate code by:\n>WarnEval<")
                        cat("sorry, operation failed in:",as.character(sys.call()),"!!!\n")
                      } else { ok<-TRUE }


                    }
                    if(ok){
                        workname.sys<-sub(paste(".*",.Platform$file.sep,sep=""),"",filename)
                        lworkname.sys<-get("lworkname.sys",envir=revive.sys)
                        tkconfigure(lworkname.sys,text=workname.sys)
                        assign("workname.sys",workname.sys,envir=revive.sys)

                        try.res<-WinToTcl.read(try.res)
                          ## Eintrag mit Entfernung des bisherigen Inhalts:
                          ## worktext<-paste(try.res, collapse="\n")
                          ## <<schreibe [[worktext]] ins Arbeitsfenster>>
                        news<-c("",try.res)
                        if(!exists("tworkwin"))
                          tworkwin<-get("tworkwin",envir=get("revive.sys",envir=revive.env))

                        pos.to.insert<-"end"
                        if(0<length(grep("output-start",news))){
                          tail<-rev(strsplit(tclvalue(tkget(tworkwin,"end - 3 lines","end")),"\n")[[1]])
                          ltail<-length(tail)
                          if( (0==length(grep("<<[*]>>=",tail[1:ltail]))) &&
                             any(h<-("output-end"==substring(tail[1:ltail],1,11)))){
                             news<-sub(".*output-start\n","",news)
                             news<-sub("output-end","",news)
                             h<-seq(along=h)[h][1]
                             pos.to.insert<-paste("end -",h,"lines")
                          }
                        }
                        try(tkinsert(tworkwin,pos.to.insert,paste(news,collapse="\n")))
                        tksee(tworkwin,"end - 0 lines")
                        melde("appended characters: \n",3,substring(news[1:min(7,length(news))],1,80))

                        if(!exists("revive.sys")) revive.sys<-get("revive.sys",envir=revive.env)
                        tworkwin<-get("tworkwin",envir=revive.sys)
                        worktext<-tclvalue(tkget(tworkwin,"0.0","end"))
                        if(nchar(worktext)<10000){
                          worktext<-strsplit(worktext,"\n")[[1]]
                        }else{
                          base::cat(worktext,file=get("tmp.file.name",env=revive.sys)
)
                          worktext<-myscan(file=get("tmp.file.name",env=revive.sys)
,"",sep="\n",blank.lines.skip=FALSE)
                        }

                        line <-floor(as.numeric(tkindex(tworkwin,"insert")))

                        code.start<-grep("^<<(.*)>>=",worktext)
                        try(if(0<length(code.start)){ 
                               worktext[code.start]<-sub("^<<(.*)>>=(.*)","<<\\1>>=",worktext[code.start])
                               worktext[code.start]<-paste(worktext[code.start]," (",1:length(code.start),")",sep="")
                        })
                        if(length(worktext)>1) worktext<-paste(worktext,collapse="\n")
                        tkdelete(tworkwin,"0.0","end")
                        try(tkinsert(tworkwin,"0.0",paste(worktext,collapse="\n")))
                        tksee(tworkwin,"end")
                        melde("ak texthervor",1)
                        tcl("markclear",tworkwin)
                        tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                        tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                        tcl("marklinetypes",tworkwin)
                        melde("ak texthervor",2)

                        if(!no.plots) { exclude.plots(tworkwin); show.plots.again(tworkwin) }


                        tkmark.set(tworkwin, "insert", paste(line,"0",sep="."))
                        tksee(tworkwin,paste(line,"0",sep="."))
                        tkfocus(tworkwin)

                        RunStart()
                        melde("ak texthervor",1)
                        tcl("markclear",tworkwin)
                        tktag.configure(tworkwin,"output",foreground="#111222999", font=outfont.sys)
                        tktag.configure(tworkwin,"code",  foreground="#ddd222222", font=outfont.sys)
                        tcl("marklinetypes",tworkwin)
                        melde("ak texthervor",2)

                        if(!no.plots) { exclude.plots(tworkwin); createandshow.all.plots(tworkwin) }

                        melde(paste("r", filename), "cmd.msg")
                        Execute.cmds()
                    } else { cat("ERROR: file not found!!!\n") }
                   }
          )

        #}
      } else remove(list="cmds",envir=revive.env)
    }
    melde("Execute.cmds",2)
  }
  tkbind(TopW, "<<Acticmds>>", Execute.cmds)

  ##definiere Bindung zur Warnungsabarbeitung##
  melde("initialization of RELAX finished",3)
  if(!missing(file.name)){
     file.name<-as.character(substitute(file.name)) ## 071115
     file.name<-gsub("\\\\","/",file.name)
     if(0<length(grep("/",file.name))){
       path<-sub("^(.*)/(.*)","\\1",file.name)
       print(path)
       if(nchar(path)>0) try(setwd(path))
       file.name<-sub("^(.*)/","",file.name)
     }
     if(0==length(grep(".rev$",file.name))) file.name<-paste(file.name,"rev",sep=".")
     print(file.name)
     workname.sys<-file.name
     assign("cmds",paste("r",file.name),env=revive.env)
     Execute.cmds()
  }
  if(0<length(cmds) && cmds[1]!="") {
    assign("cmds",cmds,env=revive.env)
    Execute.cmds()
  }

  ##definiere Logik zum Eintrag der Zeilennummer##
  data.fns.menu()
  ReloadReportWidget() # to repair defect report widget
  cat( "relax 1.1 - 081201" ,"\n")
  if(language=="german"){
    cat("relax Initialisierung abgeschlossen!\nR-Editor wird erneut durch  relax()  gestartet!\n")
  }else{
    cat("initialisation of relax completed!\nrestart relax by: relax()\n")
  }
#  tkwait.variable("tvexit")  # version 1.082
  return()
}

## DEBUG<<-T
cat("relax geladen!\n")
cat("R-Editor  mit  relax()  starten!\n")
## cat("Soll der Manager gestartet werden? (n=nein)\n")
## if("n"!=substring(readline(),1,1)) r()

