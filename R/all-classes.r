#### reutils classes definitions. All classes are S4.

## Class Unions
setClassUnion("listOrNULL", c("list","NULL"))
setClassUnion("dfOrNULL", c("data.frame","NULL"))
setClassUnion("XMLOrChar", c("XMLInternalDocument","character"))
setClassUnion("charOrNULL", c("character","NULL"))
setClassUnion("numOrNULL", c("numeric","NULL"))
setClassUnion("POSOrNULL", c("POSIXlt","NULL"))

## Class "eutil"
setClass("eutil",
         representation(url = "character",
                        error= "listOrNULL",
                        data = "XMLOrChar"),
         prototype(url = character(),
                   error = NULL,
                   data = ""))

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:

