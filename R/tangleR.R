tangleR<-
function(in.file,out.file,expand.roots=NULL,expand.root.start=TRUE){
  # german documentation of the code:
  # look for file webR.pdf, P. Wolf 050204
  if(!file.exists(in.file)) in.file<-paste(in.file,"rev",sep=".")
  if(!file.exists(in.file)){
    cat(paste("ERROR:",in.file,"not found!!??\n"))
    return("Error in tangle: file not found")
  }
  # code.ch<-scan(in.file,sep="\n",what=" ")
  code.ch<-readLines(in.file) # 2.1.0

  code.ch<-c(code.ch,"@")
  code.a<- grep("^<<(.*)>>=",code.ch)
  if(0==length(code.a)){return("Warning: no code found!!!!")}
  code.z<-grep("^@",code.ch)
  code.z    <-unlist(sapply(code.a ,function(x,y)min(y[y>x]),code.z))
  code.n    <-length(code.ch)
  change    <-rep(0,code.n); change[c(code.a ,code.z)]<-1
  code.ch   <-code.ch[1==(cumsum(change)%%2)]
  code.n    <-length(code.ch)

  code.ch<-gsub("@>>","DoSpCloseKl-esc",gsub("@<<","DoSpOpenKl-esc",code.ch))

  code.ch<-gsub("(.*)<<(.*)>>=(.*)","cOdEdEf\\2",code.ch)
  repeat{
    if(0==length(cand<-grep("<<(.*)>>",code.ch))) break
    code.ch<-unlist(strsplit(gsub("(.*)<<(.*)>>(.*)",
               "\\1bReAkuSeChUnK\\2bReAk\\3",code.ch),"bReAk"))
  }
  code.ch<-code.ch[code.ch!=""]
  code.n<-length(code.ch)
  if(exists("DEBUG")) print(code.ch)

  line.typ  <-rep("C",code.n)
  code.a    <-grep("cOdEdEf",code.ch)
  code.ch[code.a]<-substring(code.ch[code.a],8)
  line.typ[code.a]<-"D"
  code.use    <-grep("uSeChUnK",code.ch)
  code.ch[code.use]<-substring(code.ch[code.use],9)
  line.typ[code.use]<-"U"

  code.out<-NULL

  def.names<-code.ch[code.a]
  use.names<- code.ch[code.use]
  code.z<-c(if(length(code.a)>1) code.a[-1]-1, code.n)
  code.ch<-paste(line.typ,code.ch,sep="")
  if(exists("DEBUG")) print(code.ch)
  if(expand.root.start){
    if(exists("DEBUG")) cat("bearbeite start\n")
    code.out<-c(code.out,"#0:","##start:##")
    if(any(ch.no <-def.names=="start")){
       ch.no     <-seq(along=def.names)[ch.no]; rows<-NULL
       for(i in ch.no)
          if((code.a[i]+1)<=code.z[i]) rows<-c(rows, (code.a[i]+1):code.z[i])
       code.stack<-code.ch[rows]
       repeat{
         if(0==length(code.stack))break
         if("C"==substring(code.stack[1],1,1)){
           n.lines<-sum(cumprod("C"==substring(code.stack,1,1)))
           code.out<-c(code.out, substring(code.stack[1:n.lines],2))
           code.stack<-code.stack[-(1:n.lines)]
         }else{
           if(any(found<-def.names==substring(code.stack[1],2))){
             found<-seq(along=def.names)[found]; rows<-NULL
             for(no in found){
               row.no<-c((code.a[no]+1),code.z[no])
               if(row.no[1]<=row.no[2]) rows<-c(rows,row.no[1]:row.no[2])
             }
             code.stack<-c(code.ch[rows],code.stack[-1])
             cat(found,", ",sep="")
           }
         }

       }
    }
    code.out<-c(code.out,"##:start##","#:0")

  }
  root.no<-is.na(match(def.names,use.names))&def.names!="start"
  root.no<-seq(along=root.no)[root.no]
  roots  <-def.names[root.no]
  if(!is.null(expand.roots)){
    h<-!is.na(match(roots,expand.roots))
    roots<-roots[h]; root.no<-root.no[h]
  }

  if(exists("DEBUG")) cat("bearbeite Sektion-Nr./Name\n")
  for(r in seq(along=roots)){
    if(exists("DEBUG")) cat(root.no[r],":",roots[r],", ",sep="")
    row.no<-c((code.a[root.no[r]]+1),code.z[root.no[r]])
    if(row.no[1]<=row.no[2]){
      code.stack<-code.ch[row.no[1]:row.no[2]]
      code.out<-c(code.out,paste("#",root.no[r],":",sep=""),
                           paste("##",roots[r],":##",sep=""))
      repeat{
       if(0==length(code.stack))break
       if("C"==substring(code.stack[1],1,1)){
         n.lines<-sum(cumprod("C"==substring(code.stack,1,1)))
         code.out<-c(code.out, substring(code.stack[1:n.lines],2))
         code.stack<-code.stack[-(1:n.lines)]
       }else{
         def.line<-substring(code.stack[1],2)
         if(any(found<-def.names==def.line)){
           code.stack<-code.stack[-1]
           found<-rev(seq(along=def.names)[found])
           for(no in found)
             row.no<-c((code.a[no]+1),code.z[no])
             if(row.no[1]<=row.no[2]){
               code.stack<-c(paste("C#"  ,no,":"  ,sep=""      ),
                           paste("C##" ,def.line,":##",sep=""),
                           code.ch[row.no[1]:row.no[2]] ,
                           paste("C##:",def.line, "##",sep=""),
                           paste("C#:" ,no      ,sep=""      ),
                           code.stack)
               }
         }
       }

      }
      code.out<-c(code.out,paste("##:",roots[r],"##",sep=""),
                           paste("#:",root.no[r],sep=""))
    }
  }

  code.out<-gsub("DoSpCloseKl-esc",">>",gsub("DoSpOpenKl-esc","<<",code.out))

  if(missing(out.file)||in.file==out.file){
    out.file<-sub("\\.([A-Za-z])*$","",in.file)
  }
  if(0==length(grep("\\.R$",out.file)))
    out.file<-paste(out.file,".R",sep="")
  get("cat","package:base")(code.out,sep="\n",file=out.file)
  cat("tangle process finished\n")



}

