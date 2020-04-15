getf <- function(input) {
      sens=input$sens/100
      spec=input$spec/100
      imm=input$imm/100
      res=list(N=10000,
           imm=imm,
           nim=1-imm,
           sens=sens,
           spec=spec,
           imm.pos=imm*sens,
           imm.neg=imm*(1-sens),
           nim.pos=(1-imm)*(1-spec),
           nim.neg=(1-imm)*spec)
      res$pos=res$imm.pos+res$nim.pos
      res$neg=res$imm.neg+res$nim.neg
      res
}

roundpc <- function(x) {
  x=as.character(round(100*x))
  x[x=="100"] = ">99"
  x[x=="0"] = "<1"
  return(paste0(x,"%"))
}
