weaveR<-function(in.file,out.file){
  # german documentation of the code:
  # look for file webR.pdf, P. Wolf 050204, 060517
  pat.use.chunk<-paste("<","<(.*)>",">",sep="")
  pat.chunk.header<-paste("^<","<(.*)>",">=",sep="")
  pat.verbatim.begin<-"\\\\begin\\{verbatim\\}"
  pat.verbatim.end<-"\\\\end\\{verbatim\\}"
  pat.leerzeile<-"^(\\ )*$"
  lcctype<-grep("LC_CTYPE",strsplit(Sys.getlocale(),";")[[1]],value=T)
  UTF<-(1==length(grep("UTF",lcctype))) 
  UTF<- UTF | nchar(deparse("\xc3")) > 3
  if(UTF) cat("character set: UTF\n") else cat("character set: ascii\n")

  if(!file.exists(in.file)) in.file<-paste(in.file,"rev",sep=".")
  if(!file.exists(in.file)){
    cat(paste("ERROR:",in.file,"not found!!??\n"))
    return("Error in weave: file not found")
  }
  # input<-scan(in.file,what="",sep="\n",blank.lines.skip = FALSE)
  input<-readLines(in.file) # 2.1.0
  length.input<-length(input)

  input<-gsub("@>>","DoSpCloseKl-esc",gsub("@<<","DoSpOpenKl-esc",input))
  input<-gsub("@\\]\\]","DoEckCloseKl-esc",gsub("@\\[\\[","DoEckOpenKl-esc",input))

  empty.index<-grep(pat.leerzeile,input)
  text.start.index<-which("@"==substring(input,1,1))

  a<-rep(0,length.input)
  a[grep(pat.verbatim.begin,input)]<-1
  a[grep(pat.verbatim.end,input)]<- -1
  a<-cumsum(a)
  verb.index<-which(a>0)

  code.start.index<-grep(pat.chunk.header,input)
  use.index<-grep(pat.use.chunk,input)
  use.index<-use.index[is.na(match(use.index,code.start.index))]

  a<-rep(0,length.input)
  a[text.start.index]<- -1; a[code.start.index]<-2
  a<-cbind(c(text.start.index,code.start.index),
    c(rep(-1,length(text.start.index)),rep(1,length(code.start.index))))
  a<-a[order(a[,1]),,drop=F]
  b<-a[a[,2]!=c(-1,a[-length(a[,1]),2]),,drop=F]
  a<-rep(0,length.input); a[b[,1]]<-b[,2]
  a<-cumsum(a); a[code.start.index]<-0; a[empty.index]<-0
  code.index<-which(a>0)
  code.index<-code.index[is.na(match(code.index,use.index))]

  line.typ<-rep("TEXT" ,length.input)
  line.typ[empty.index]<-"EMPTY"
  line.typ[text.start.index]<-"TEXT-START"
  line.typ[verb.index]<-"VERBATIM"
  line.typ[use.index]<-"USE"
  line.typ[code.start.index]<-"HEADER"
  line.typ[code.index]<-"CODE"


  input[text.start.index]<-""

  code.chunk.names<-code.start.lines<-sub(pat.chunk.header,"\\1",input[code.start.index])
  use.lines<-input[use.index]
  code.lines<-input[code.index]

  no<-1:length(code.start.index)
  def.ref.no<-match(gsub("\\ ","",code.start.lines), gsub("\\ ","",code.start.lines))
  code.start.lines<-paste(
        # "\\rule{0mm}{0mm}\\\\\\hspace*{-3em}", "\\makebox[0mm]{",no,"}\\hspace*{3em}", # old: margin.no by number
        "\\makemarginno ", # new: margin.no by counter
        "$\\langle${\\it ",code.start.lines,"}\\ $",def.ref.no,
        "\\rangle",ifelse(no!=def.ref.no,"+",""),"\\equiv$\\newline",sep="")
  input[code.start.index]<-code.start.lines

  use.lines<-input[use.index]
  leerzeichen.vor.use<-paste("\\verb|",sub("[^ ](.*)$","",use.lines),"|",sep="")
  use.lines<-substring(use.lines,nchar(leerzeichen.vor.use)-8)
  for(i in seq(use.lines)){
    uli<-use.lines[i]
    repeat{
      if(0==length(cand<-grep("<<(.*)>>",uli))) break
      uli.h<-gsub("(.*)<<(.*)>>(.*)","\\1bReAkuSeChUnK\\2bReAk\\3",uli)
      uli<-unlist(strsplit(uli.h,"bReAk"))
    }
    cand<-grep("uSeChUnK",uli); uli<-sub("uSeChUnK","",uli)
    ref.no<-match(uli[cand],code.chunk.names)
    uli[cand]<-paste("$\\langle${\\it ",uli[cand],"} ",ref.no,"$\\rangle$",sep="")
    if(length(uli)!=length(cand)){
      if(!UTF){ 
        uli[-cand]<-paste("\\verb\267",uli[-cand],"\267",sep="") #050612
      }else{
        uli[-cand]<-paste("\\verb\140",uli[-cand],"\140",sep="") #060516
      }
    }
    use.lines[i]<-paste(uli,collapse="")
  }
  input[use.index]<-paste(leerzeichen.vor.use,use.lines,"\\newline")

  if(!UTF){
    input[code.index]<-paste("\\verb\267",code.lines,"\267\\newline")
  }else{
    input[code.index]<-paste("\\verb\140",code.lines,"\140\\newline") #060516
  }

  typ<-"TEXT"
  index<-which(line.typ==typ)
  code.im.text.index<-index[grep("\\[\\[(.*)\\]\\]",input[index])]

  if(0<length(code.im.text.index)){
    lines.to.check<-input[code.im.text.index]
    lines.to.check<-strsplit(lines.to.check," ") # Zerlegung in Worte
    lines.to.check<-unlist(lapply(lines.to.check,function(x){
        ind.cand<-grep("^\\[\\[(.*)\\]\\]$",x)
        if(0<length(ind.cand)){
          cand<-gsub("^\\[\\[(.*)\\]\\]$","\\1",x[ind.cand])
          cand<-gsub("\\[\\[","DoEckOpenKl-esc",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-esc",cand)
          cand<-gsub("\\\\","\\\\char'134 ",cand)
          cand<-gsub("([#$&_%{}])","\\\\\\1",cand) #2.1.0
          cand<-gsub("\\~","\\\\char'176 ",cand)
          cand<-gsub("\\^","\\\\char'136 ",cand)
          cand<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",cand) # 050612
          cand<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",cand) # 050612
          x[ind.cand]<-paste("{\\tt ",cand,"}",sep="")
        }
        x<-paste(x,collapse=" ")}
    )) # end of unlist(apply(..))

    ind.cand<-grep("\\[\\[(.*)\\]\\]",lines.to.check)
    if(0<length(ind.cand)) {
      # zerlege Zeile in token der Form [[,  ]] und sonstige
      zsplit<-lapply(strsplit(lines.to.check[ind.cand],"\\[\\["),function(x){
         zs<-strsplit(rbind("[[",paste(x[],"\333",sep=""))[-1],"\\]\\]")
         zs<-unlist(lapply(zs,function(y){ res<-rbind("]]",y[])[-1]; res }))
         gsub("\333","",zs)
      })
      # suche von vorn beginnend zusammenpassende [[-]]-Paare
      z<-unlist(lapply(zsplit,function(x){
        repeat{
          cand.sum<-cumsum((x=="[[")-(x=="]]"))
          if(is.na(br.open<-which(cand.sum==1)[1])) break
          br.close<-which(cand.sum==0)
          if(is.na(br.close<-br.close[br.open<br.close][1])) break
          if((br.open+1)<=(br.close-1)){
            h<-x[(br.open+1):(br.close-1)]; h<-gsub("\\\\","\\\\char'134 ",h)
            h<-gsub("([#$&_%{}])","\\\\\\1",h); h<-gsub("\\~","\\\\char'176 ",h) #2.1.0
            h<-gsub(" ","\\\\ ",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",h) # 050612
            h<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",h) # 050612
          x[(br.open+1):(br.close-1)]<-gsub("\\^","\\\\char'136 ",h)
          }
          x[br.open]<-"{\\tt "; x[br.close]<-"}"
          x<-c(paste(x[1:br.close],collapse=""), x[-(1:br.close)])
        }
        paste(x,collapse="")
      }))
      lines.to.check[ind.cand]<-z
    }

    input[code.im.text.index]<-lines.to.check
  }

  typ<-"HEADER"
  index<-which(line.typ==typ)
  code.im.text.index<-index[grep("\\[\\[(.*)\\]\\]",input[index])]

  if(0<length(code.im.text.index)){
    lines.to.check<-input[code.im.text.index]
    lines.to.check<-strsplit(lines.to.check," ") # Zerlegung in Worte
    lines.to.check<-unlist(lapply(lines.to.check,function(x){
        ind.cand<-grep("^\\[\\[(.*)\\]\\]$",x)
        if(0<length(ind.cand)){
          cand<-gsub("^\\[\\[(.*)\\]\\]$","\\1",x[ind.cand])
          cand<-gsub("\\[\\[","DoEckOpenKl-esc",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-esc",cand)
          cand<-gsub("\\\\","\\\\char'134 ",cand)
          cand<-gsub("([#$&_%{}])","\\\\\\1",cand) #2.1.0
          cand<-gsub("\\~","\\\\char'176 ",cand)
          cand<-gsub("\\^","\\\\char'136 ",cand)
          cand<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",cand) # 050612
          cand<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",cand) # 050612
          x[ind.cand]<-paste("{\\tt ",cand,"}",sep="")
        }
        x<-paste(x,collapse=" ")}
    )) # end of unlist(apply(..))

    ind.cand<-grep("\\[\\[(.*)\\]\\]",lines.to.check)
    if(0<length(ind.cand)) {
      # zerlege Zeile in token der Form [[,  ]] und sonstige
      zsplit<-lapply(strsplit(lines.to.check[ind.cand],"\\[\\["),function(x){
         zs<-strsplit(rbind("[[",paste(x[],"\333",sep=""))[-1],"\\]\\]")
         zs<-unlist(lapply(zs,function(y){ res<-rbind("]]",y[])[-1]; res }))
         gsub("\333","",zs)
      })
      # suche von vorn beginnend zusammenpassende [[-]]-Paare
      z<-unlist(lapply(zsplit,function(x){
        repeat{
          cand.sum<-cumsum((x=="[[")-(x=="]]"))
          if(is.na(br.open<-which(cand.sum==1)[1])) break
          br.close<-which(cand.sum==0)
          if(is.na(br.close<-br.close[br.open<br.close][1])) break
          if((br.open+1)<=(br.close-1)){
            h<-x[(br.open+1):(br.close-1)]; h<-gsub("\\\\","\\\\char'134 ",h)
            h<-gsub("([#$&_%{}])","\\\\\\1",h); h<-gsub("\\~","\\\\char'176 ",h) #2.1.0
            h<-gsub(" ","\\\\ ",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",h) # 050612
            h<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",h) # 050612
          x[(br.open+1):(br.close-1)]<-gsub("\\^","\\\\char'136 ",h)
          }
          x[br.open]<-"{\\tt "; x[br.close]<-"}"
          x<-c(paste(x[1:br.close],collapse=""), x[-(1:br.close)])
        }
        paste(x,collapse="")
      }))
      lines.to.check[ind.cand]<-z
    }

    input[code.im.text.index]<-lines.to.check
  }

  typ<-"USE"
  index<-which(line.typ==typ)
  code.im.text.index<-index[grep("\\[\\[(.*)\\]\\]",input[index])]

  if(0<length(code.im.text.index)){
    lines.to.check<-input[code.im.text.index]
    lines.to.check<-strsplit(lines.to.check," ") # Zerlegung in Worte
    lines.to.check<-unlist(lapply(lines.to.check,function(x){
        ind.cand<-grep("^\\[\\[(.*)\\]\\]$",x)
        if(0<length(ind.cand)){
          cand<-gsub("^\\[\\[(.*)\\]\\]$","\\1",x[ind.cand])
          cand<-gsub("\\[\\[","DoEckOpenKl-esc",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-esc",cand)
          cand<-gsub("\\\\","\\\\char'134 ",cand)
          cand<-gsub("([#$&_%{}])","\\\\\\1",cand) #2.1.0
          cand<-gsub("\\~","\\\\char'176 ",cand)
          cand<-gsub("\\^","\\\\char'136 ",cand)
          cand<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",cand) # 050612
          cand<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",cand) # 050612
          x[ind.cand]<-paste("{\\tt ",cand,"}",sep="")
        }
        x<-paste(x,collapse=" ")}
    )) # end of unlist(apply(..))

    ind.cand<-grep("\\[\\[(.*)\\]\\]",lines.to.check)
    if(0<length(ind.cand)) {
      # zerlege Zeile in token der Form [[,  ]] und sonstige
      zsplit<-lapply(strsplit(lines.to.check[ind.cand],"\\[\\["),function(x){
         zs<-strsplit(rbind("[[",paste(x[],"\333",sep=""))[-1],"\\]\\]")
         zs<-unlist(lapply(zs,function(y){ res<-rbind("]]",y[])[-1]; res }))
         gsub("\333","",zs)
      })
      # suche von vorn beginnend zusammenpassende [[-]]-Paare
      z<-unlist(lapply(zsplit,function(x){
        repeat{
          cand.sum<-cumsum((x=="[[")-(x=="]]"))
          if(is.na(br.open<-which(cand.sum==1)[1])) break
          br.close<-which(cand.sum==0)
          if(is.na(br.close<-br.close[br.open<br.close][1])) break
          if((br.open+1)<=(br.close-1)){
            h<-x[(br.open+1):(br.close-1)]; h<-gsub("\\\\","\\\\char'134 ",h)
            h<-gsub("([#$&_%{}])","\\\\\\1",h); h<-gsub("\\~","\\\\char'176 ",h) #2.1.0
            h<-gsub(" ","\\\\ ",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-esc","\\\\verb|<<|",h) # 050612
            h<-gsub("DoSpCloseKl-esc","\\\\verb|>>|",h) # 050612
          x[(br.open+1):(br.close-1)]<-gsub("\\^","\\\\char'136 ",h)
          }
          x[br.open]<-"{\\tt "; x[br.close]<-"}"
          x<-c(paste(x[1:br.close],collapse=""), x[-(1:br.close)])
        }
        paste(x,collapse="")
      }))
      lines.to.check[ind.cand]<-z
    }

    input[code.im.text.index]<-lines.to.check
  }




  if(!UTF){
   # im Tcl/Tk-Textfenster eingegeben -> iso-8859-1 (man iso-8859-1 / Latin1 / unicode
    input<-gsub("\283","",input)
    input<-chartr("\244\266\274\204\226\234\237","\344\366\374\304\326\334\337",input)
    # Latin1 -> TeX-Umlaute
    input<-gsub("\337","{\\\\ss}",input)
    input<-gsub("(\344|\366|\374|\304|\326|\334)","\\\\\"\\1",input)
    input<-chartr("\344\366\374\304\326\334","aouAOU",input)
  }else{
    input<-gsub("\283\237","{\\\\ss}",input)
    input<-gsub("(\283\244|\283\266|\283\274|\283\204|\283\226|\283\234)",
                              "\\\\\"\\1",input)
    input<-chartr("\283\244\283\266\283\274\283\204\283\226\283\234", 
                                "aouAOU", input)
  }
  cat("german Umlaute replaced\n")
  input<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",input))
  input<-gsub("DoEckCloseKl-esc","]]",gsub("DoEckOpenKl-esc","[[",input))

  input[1]<-paste(
       "\\newcounter{Rchunkno}",
       "\\newcommand{\\makemarginno}{\\stepcounter{Rchunkno}",
       "\\rule{0mm}{0mm}\\\\\\hspace*{-3em}\\makebox[0mm]{",
       "\\arabic{Rchunkno}",
       "}\\hspace*{3em}}",
       input[1],sep="")
  if(missing(out.file)||in.file==out.file){
    out.file<-sub("\\.([A-Za-z])*$","",in.file)
  }
  if(0==length(grep("\\.tex$",out.file)))
    out.file<-paste(out.file,".tex",sep="")
  get("cat","package:base")(input,sep="\n",file=out.file)
  cat("weave process finished\n")

}

