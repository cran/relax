weaveRhtml<-function(in.file,out.file){
  # german documentation of the code:
  # look for file webR.pdf, P. Wolf 060920
  verbose<-FALSE
  pat.use.chunk<-paste("<","<(.*)>",">",sep="")
  pat.chunk.header<-paste("^<","<(.*)>",">=",sep="")
  pat.verbatim.begin<-"\\\\begin\\{verbatim\\}"
  pat.verbatim.end<-"\\\\end\\{verbatim\\}"
  pat.leerzeile<-"^(\\ )*$"
  lcctype<-grep("LC_CTYPE",strsplit(Sys.getlocale(),";")[[1]],value=T)
  UTF<-(1==length(grep("UTF",lcctype))) 
  UTF<- UTF | nchar(deparse("\xc3")) > 3
  if(verbose)  {if(UTF) cat("character set: UTF\n") else cat("character set: ascii\n")}

  get.argument<-function(command,txt,default="",kla="{",kle="}",dist=TRUE){
  ## print("get.argument")
    command<-paste("\\\\",command,sep="")
    if(0==length(grep(command,txt))) return(default)
    txt<-unlist(strsplit(paste(txt,collapse="\n"),command))[-1]
    arg<-lapply(txt,function(x){ 
         n<-nchar(x); if(n<3) return(x)
         x<-substring(x,1:n,1:n)
         h<-which(x==kla)[1]; if(is.na(h)) h<-1
         if(dist)x<-x[h:length(x)]
         k<-which(cumsum((x==kla)-(x==kle))==0)[1]
         paste(x[2:(k-1)],collapse="")
    })
    arg
  }
  get.head.argument<-function(command,txt,default="",kla="{",kle="}",dist=TRUE){
  ## print("get.head.argument")
    command<-paste("\\\\",command,sep="")
    txt<-unlist(strsplit(paste(txt,collapse="\n"),command))[-1]
    arg<-lapply(txt,function(x){ 
         n<-nchar(x); x<-substring(x,1:n,1:n)
         if(dist)x<-x[which(x==kla)[1]:length(x)]
         k<-which(cumsum((x==kla)-(x==kle))==0)[1]
         paste(x[2:(k-1)],collapse="")
    })
    unlist(arg)
  }
  transform.command<-function(command,txt,atag="<i>",etag="</i>",
                         kla="{",kle="}"){
  ## print("transform.command")
    command<-paste("\\\\",command,sep="")
    ##  if(0==length(grep(command,txt))){print("hallo"); return(txt)}
    txt<-unlist(strsplit(paste(txt,collapse="\n"),command))
    tx<-unlist(lapply(txt[-1],function(x){ 
         n<-nchar(x); if(n<4) return(x)
         x<-substring(x,1:n,1:n)
         an<-which(x==kla)[1]
         en<-which(cumsum((x==kla)-(x==kle))==0)[1]
        if(!is.na(an)) 
         paste(atag,paste(x[(an+1):(en-1)],collapse=""),etag,
              paste(x[-(1:en)],collapse="")) else x
    }))
    unlist(strsplit(c(txt[1],tx),"\n"))
  }
  transform.command.line<-function(command,txt,atag="<i>",etag="</i>",
                         kla="{",kle="}"){
    command<-paste("\\\\",command,sep="")
    if(0==length(ind<-grep(command,txt))){return(txt)}
    txt.lines<-txt[ind]
    txt.lines<-strsplit(txt.lines,command)
    txt.lines<-lapply(txt.lines,function(xxx){ 
      for(i in 2:length(xxx)){
        m<-nchar(xxx[i])
        if(is.na(m)) break
        x.ch<-substring(xxx[i],1:m,1:m); x.info<-rep(0,m)
        x.info<-cumsum((x.ch=="{") - (x.ch=="}"))
        h<-which(x.info==0)[1]
        if(!is.na(h)) {x.ch[1]<-atag; x.ch[h]<- etag }
        xxx[i]<-paste(x.ch,collapse="")
      }
      paste(xxx,collapse="")
    })
    txt[ind]<-unlist(txt.lines)
    txt
  }
  transform.structure.command<-function(command,txt,atag="<i>",etag="</i>",
                         kla="{",kle="}"){
  ## print("transform.structure.command")
    command<-paste("\\\\",command,sep="")
    ##  if(0==length(grep(command,txt))){print("hallo"); return(txt)}
    txt<-unlist(strsplit(paste(txt,collapse="\n"),command))
    tx<-unlist(lapply(txt[-1],function(x){ 
         n<-nchar(x); if(n<4) return(x)
         x<-substring(x,1:n,1:n)
         an<-which(x==kla)[1]
         en<-which(cumsum((x==kla)-(x==kle))==0)[1]
        if(!is.na(an)) 
         paste(atag,paste(x[(an+1):(en-1)],collapse=""),etag,
              paste(x[-(1:en)],collapse=""))
                else x
    }))
    unlist(strsplit(c(txt[1],tx),"\n"))
  }


  if(!file.exists(in.file)) in.file<-paste(in.file,"rev",sep=".")
  if(!file.exists(in.file)){
    cat(paste("ERROR:",in.file,"not found!!??\n"))
    return("Error in weaveRhtml: file not found")
  }
  input<-readLines(in.file) 
  input<-gsub("\t","      ",input)

  length.input<-length(input)

  h<-grep("^[ ]*%",input)
  if(0<length(h)) input<-input[-h]

  input<-gsub("@>>","DoSpCloseKl-ESC",gsub("@<<","DoSpOpenKl-ESC",input))
  input<-gsub("@\\]\\]","DoEckCloseKl-ESC",gsub("@\\[\\[","DoEckOpenKl-ESC",input))

  empty.index<-grep(pat.leerzeile,input)
  text.start.index<-which("@"==substring(input,1,1))

  a<-rep(0,length(input))
  an<-grep(pat.verbatim.begin,input)
  if(0<length(an)) {
    a[an]<- 1
    en<-grep(pat.verbatim.end,input); a[en]<- -1
    input[a==1]<-"<code><FONT COLOR=\"#0000FF\">"
    input[a==-1]<-"</font></code><br>"
    a<-cumsum(a)
  }
  verb.index<-which(a>0)
  input[verb.index]<-paste(input[verb.index],"<br>")

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


  input<-sub("\\\\begin\\{center}","<center>",input)
  input<-sub("\\\\end\\{center}","</center>",input)
  input<-sub("\\\\begin\\{quote}","<ul>",input)
  input<-sub("\\\\end\\{quote}","</ul>",input)
  input<-sub("\\\\begin\\{itemize}","<ul>",input)
  input<-sub("\\\\end\\{itemize}","</ul>",input)
  input<-sub("\\\\item","</li><li>",input)

  input[text.start.index]<-"<p>"    # vorher: @
  lz<-grep("^[ ]*$",input)
  if(0<length(lz)) input[lz]<-"<br>"

  plz.ind<-grep("\\\\includegraphics",input)
  if(0<length(plz.ind)){
    plz<-input[plz.ind]
    h<-unlist(get.argument("includegraphics",plz))
    h<-paste("<img SRC=\"",sub(".ps$",".jpg",h),"\">",sep="")
    input[plz.ind]<-h
  }

  code.chunk.names<-code.start.lines<-sub(pat.chunk.header,"\\1",input[code.start.index])
  use.lines<-input[use.index]
  code.lines<-input[code.index]
  ## print(input[code.start.index])

  no<-1:length(code.start.index)
  def.ref.no<-match(gsub("\\ ","",code.start.lines), gsub("\\ ","",code.start.lines))
  code.start.lines<-paste(
        "<a name=\"codechunk",no,"\"></a>",
        "<a href=\"#codechunk",1+(no%%max(no)),"\">",
        "<br>Chunk:",no," <i>&lt;",code.start.lines,def.ref.no,
        "&gt;",ifelse(no!=def.ref.no,"+",""),"=</i></a><br>",sep="")
  input[code.start.index]<-code.start.lines

  use.lines<-input[use.index]
  leerzeichen.vor.use<-sub("[^ ](.*)$","",use.lines)
  use.lines<-substring(use.lines,nchar(leerzeichen.vor.use))
  leerzeichen.vor.use<-gsub("\\ ","&nbsp;",leerzeichen.vor.use)
  for(i in seq(use.lines)){
    uli<-use.lines[i]
    such<-paste("(.*)<","<(.*)>",">(.*)",sep="")
    repeat{
      if(0==length(cand<-grep("<<(.*)>>",uli))) break
      uli.h<-gsub(such,"\\1BrEaKuSeCHUNK\\2BrEaK\\3",uli)
      uli<-unlist(strsplit(uli.h,"BrEaK"))
    }
    cand<-grep("uSeCHUNK",uli); uli<-sub("uSeCHUNK","",uli)
    ref.no<-match(uli[cand],code.chunk.names)
    uli[cand]<-paste("<code>&lt;",uli[cand]," ",ref.no,"&gt;</code>",sep="")
    if(length(uli)!=length(cand)){
      if(!UTF){ 
        uli[-cand]<-paste("",uli[-cand],"",sep="") #050612
      }else{
        uli[-cand]<-paste("",uli[-cand],"",sep="") #060516
      }
    }
    use.lines[i]<-paste(uli,collapse="")
  }
  input[use.index]<-paste(leerzeichen.vor.use,use.lines,"<br>")

  leerzeichen.vor.c<-gsub("\t","      ",code.lines)
  leerzeichen.vor.c<-sub("[^ ](.*)$","",leerzeichen.vor.c)
  leerzeichen.vor.c<-gsub("\\ ","&nbsp;",leerzeichen.vor.c)
  if(!UTF){
    input[code.index]<-paste(leerzeichen.vor.c,"<code>",code.lines,"</code><br>")
  }else{
    input[code.index]<-paste(leerzeichen.vor.c,"<code>",code.lines,"</code><br>") 
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
          cand<-gsub("\\[\\[","DoEckOpenKl-ESC",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-ESC",cand)
          cand<-gsub("DoSpOpenKl-ESC","<<",cand) # 050612
          cand<-gsub("DoSpCloseKl-ESC",">>",cand) # 050612
          x[ind.cand]<-paste("<code>",cand,"</code>",sep="")
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
            h<-x[(br.open+1):(br.close-1)]
            h<-gsub(" ","&nbsp;",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-ESC","<<",h)
            h<-gsub("DoSpCloseKl-ESC",">>",h)
            x[(br.open+1):(br.close-1)]<-h
          }
          x[br.open]<-"<code> "; x[br.close]<-"</code>"
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
          cand<-gsub("\\[\\[","DoEckOpenKl-ESC",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-ESC",cand)
          cand<-gsub("DoSpOpenKl-ESC","<<",cand) # 050612
          cand<-gsub("DoSpCloseKl-ESC",">>",cand) # 050612
          x[ind.cand]<-paste("<code>",cand,"</code>",sep="")
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
            h<-x[(br.open+1):(br.close-1)]
            h<-gsub(" ","&nbsp;",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-ESC","<<",h)
            h<-gsub("DoSpCloseKl-ESC",">>",h)
            x[(br.open+1):(br.close-1)]<-h
          }
          x[br.open]<-"<code> "; x[br.close]<-"</code>"
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
          cand<-gsub("\\[\\[","DoEckOpenKl-ESC",cand)
          cand<-gsub("\\]\\]","DoEckCloseKl-ESC",cand)
          cand<-gsub("DoSpOpenKl-ESC","<<",cand) # 050612
          cand<-gsub("DoSpCloseKl-ESC",">>",cand) # 050612
          x[ind.cand]<-paste("<code>",cand,"</code>",sep="")
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
            h<-x[(br.open+1):(br.close-1)]
            h<-gsub(" ","&nbsp;",h) # Leerzeichen nicht vergessen! 060116
            h<-gsub("DoSpOpenKl-ESC","<<",h)
            h<-gsub("DoSpCloseKl-ESC",">>",h)
            x[(br.open+1):(br.close-1)]<-h
          }
          x[br.open]<-"<code> "; x[br.close]<-"</code>"
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
    input<-gsub("\337","&szlig;",input) # SZ
    input<-gsub("(\344|\366|\374|\304|\326|\334)","&\\1uml;",input)
    input<-chartr("\344\366\374\304\326\334","aouAOU",input)
  }else{
    input<-gsub("\283\237","&szlig;",input)
    input<-gsub("(\283\244|\283\266|\283\274|\283\204|\283\226|\283\234)",
                              "&\\1uml;",input)
    input<-chartr("\283\244\283\266\283\274\283\204\283\226\283\234", 
                                "aouAOU", input)
  }
  if(verbose) cat("german Umlaute replaced\n")
  #input<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",input))
  input<-gsub("DoSpCloseKl-ESC","&gt;&gt;",gsub("DoSpOpenKl-ESC","&lt;&lt;",input))
  input<-gsub("DoEckCloseKl-ESC","]]",gsub("DoEckOpenKl-ESC","[[",input))

  atag<-"<h2>"; etag<-"</h2>"; command<-"section"
  command.n<-nchar(command)+2; command.links<-NULL
  kla<-"{"; kle<-"}"
  ## print("STRUKTUR")
  if(0<length(com.lines<-grep(paste("^\\\\",command,sep=""),input))){
    sec<-NULL
    for(i in seq(com.lines)){
      txt<-input[com.lines[i]+0:2]
      txt<-paste(txt,collapse="\n"); n<-nchar(txt)   
      x<-substring(txt,command.n:n,command.n:n)
      en<-which(cumsum((x==kla)-(x==kle))==0)[1]
      x[1]<-paste("<a name=\"",command,i,"\">",atag,sep="")
      x[en]<-etag;  txt<-paste(x,collapse="")
      sec<-c(sec,paste(x[2:(en-1)],collapse=""))
      input[com.lines[i]+0:2]<-unlist(strsplit(txt,"\n"))
    }
    command.links<-paste("<a href=\"#",command,seq(com.lines),"\">",sec,"</a>",sep="")
  }

  sec.links<-command.links
  sec.no<-com.lines
  atag<-"<h3>"; etag<-"</h3>"; command<-"subsection"
  command.n<-nchar(command)+2; command.links<-NULL
  kla<-"{"; kle<-"}"
  ## print("STRUKTUR")
  if(0<length(com.lines<-grep(paste("^\\\\",command,sep=""),input))){
    sec<-NULL
    for(i in seq(com.lines)){
      txt<-input[com.lines[i]+0:2]
      txt<-paste(txt,collapse="\n"); n<-nchar(txt)   
      x<-substring(txt,command.n:n,command.n:n)
      en<-which(cumsum((x==kla)-(x==kle))==0)[1]
      x[1]<-paste("<a name=\"",command,i,"\">",atag,sep="")
      x[en]<-etag;  txt<-paste(x,collapse="")
      sec<-c(sec,paste(x[2:(en-1)],collapse=""))
      input[com.lines[i]+0:2]<-unlist(strsplit(txt,"\n"))
    }
    command.links<-paste("<a href=\"#",command,seq(com.lines),"\">",sec,"</a>",sep="")
  }

  atag<-"<h4>"; etag<-"</h4>"; command<-"subsubsection"
  command.n<-nchar(command)+2; command.links<-NULL
  kla<-"{"; kle<-"}"
  ## print("STRUKTUR")
  if(0<length(com.lines<-grep(paste("^\\\\",command,sep=""),input))){
    sec<-NULL
    for(i in seq(com.lines)){
      txt<-input[com.lines[i]+0:2]
      txt<-paste(txt,collapse="\n"); n<-nchar(txt)   
      x<-substring(txt,command.n:n,command.n:n)
      en<-which(cumsum((x==kla)-(x==kle))==0)[1]
      x[1]<-paste("<a name=\"",command,i,"\">",atag,sep="")
      x[en]<-etag;  txt<-paste(x,collapse="")
      sec<-c(sec,paste(x[2:(en-1)],collapse=""))
      input[com.lines[i]+0:2]<-unlist(strsplit(txt,"\n"))
    }
    command.links<-paste("<a href=\"#",command,seq(com.lines),"\">",sec,"</a>",sep="")
  }

  atag<-"<br><b>"; etag<-"</b>"; command<-"paragraph"
  command.n<-nchar(command)+2; command.links<-NULL
  kla<-"{"; kle<-"}"
  ## print("STRUKTUR")
  if(0<length(com.lines<-grep(paste("^\\\\",command,sep=""),input))){
    sec<-NULL
    for(i in seq(com.lines)){
      txt<-input[com.lines[i]+0:2]
      txt<-paste(txt,collapse="\n"); n<-nchar(txt)   
      x<-substring(txt,command.n:n,command.n:n)
      en<-which(cumsum((x==kla)-(x==kle))==0)[1]
      x[1]<-paste("<a name=\"",command,i,"\">",atag,sep="")
      x[en]<-etag;  txt<-paste(x,collapse="")
      sec<-c(sec,paste(x[2:(en-1)],collapse=""))
      input[com.lines[i]+0:2]<-unlist(strsplit(txt,"\n"))
    }
    command.links<-paste("<a href=\"#",command,seq(com.lines),"\">",sec,"</a>",sep="")
  }

  subsec.links<-command.links
  subsec.no<-com.lines
  contents<-c(paste(seq(sec.links),sec.links),paste("&nbsp;&nbsp;",subsec.links))[order(c(sec.no,subsec.no))]
  ## print(contents)
  ## if(verbose) print("head")
  head<-grep("^\\\\title|^\\\\author|^\\\\date",input)
  if(0<length(head)){
    h<-min(max(head)+5,length(input))
    head<-input[1:h]
    titel<-get.head.argument("title",head)[1]
    titel<-sub("Report: \\\\rule\\{(.*)\\}","Report: ................",titel)
    autor<-get.head.argument("author",head)[1]
    autor<-sub("File: \\\\jobname.rev",paste("File:",sub(".*/","",in.file)),autor)
    datum<-get.head.argument("date",head)[1]
    if(is.null(datum)) datum<-date()
    ## print(datum)
  } else {
    head<-""; titel<-paste("File:",in.file); autor<-"processed by weaveRhtml"; datum<-date()
  }
  if(0<length(h<-grep("\\\\begin\\{document\\}",input)))
    input<-input[-(1:h[1])]
  input[1]<-paste(collapse="\n",
    "<!--  generated by weaveRhtml --><html><head>",
    "<meta content=\"text/html; charset=ISO-8859-1\">",
    "<title>",titel,"</title></head>",
    "<body bgcolor=\"#FFFFFF\">",
    "<h1>",if(!is.null(titel))titel,"</h1>",
    "<h2>",if(!is.null(autor))autor,"</h2>",
    "<h3>",if(!is.null(datum))datum,"</h3>",
    "<h4>",paste(contents,collapse="<br>"),"</h4>"
  )


  if(0<length(h<-grep("\\\\myemph",input))){
    input<-transform.command.line("myemph",input,"<i>","</i>")
  }
  if(0<length(h<-grep("\\\\texttt",input))){
    input<-transform.command.line("texttt",input,"<code>","</code>")
  }


  input<-gsub("\\\\newpage","",input)
  input<-gsub("\\\\tableofcontents","",input)
  input<-gsub("\\\\raggedright","",input)
  input<-gsub("\\\\\\\\","<br>",input)
  h<-grep("\\\\maketitle|\\\\author|\\\\date|\\\\title|\\\\end\\{document\\}",input)
  if(0<length(h)) input<-input[-h]

  if(missing(out.file)||in.file==out.file){
    out.file<-sub("\\.([A-Za-z])*$","",in.file)
  }
  if(0==length(grep("\\.html$",out.file)))
    out.file<-paste(out.file,".html",sep="")
  ## out.file<-"/home/wiwi/pwolf/tmp/out.html"
  get("cat","package:base")(input,sep="\n",file=out.file)
  cat("weaveRhtml process finished\n")

  "ok"
}

