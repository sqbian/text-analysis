library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(XML)
library(methods)


word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()


person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")

organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}


setwd("C:/Users/bian0553/Desktop/extractentity/WHITMAN_ARCHIVE/correspondence")
col1 = character()
col2 = character()
for(i in 1:length(list.files())){
  file = list.files()[i]
  a = readLines(file)
  a = paste(a, collapse = " ")
  a = gsub("<.*?>"," ",a)
  a = gsub("\\s+"," ",a)
  a_annotations <- annotate(a, pipeline)
  a_doc <- AnnotatedPlainTextDocument(a, a_annotations)
  
  place = entities(a_doc,kind = "location")
  if(length(place) == 0){
    place = NA
  }
  if(length(place) > 1){
    place = unique(place)
    place = paste(place,collapse = "; ")
  }
#entities(a_doc,kind = "person")
#entities(a_doc,kind = "organization")
  col1 = c(col1,file)
  col2 = c(col2,place)
  print(i)
}

correspondence = data.frame(file = col1, place = col2)
setwd("C:/Users/bian0553/Desktop/extractentity/")
write.table(correspondence, file = "correspondence")



formdf = function(num){
  folder = list.files()[num]
  col1 = character()
  col2 = character()
    for(j in 1:length(list.files(path=folder))){
      file = list.files(path=folder)[j]
      a = readLines(paste(folder,file,sep = "/"))
      a = paste(a, collapse = " ")
      a = gsub("<.*?>"," ",a)
      a = gsub("\\s+"," ",a)
      a_annotations <- annotate(a, pipeline)
      a_doc <- AnnotatedPlainTextDocument(a, a_annotations)
      
      place = entities(a_doc,kind = "location")
      if(length(place) == 0){
        place = NA
      }
      if(length(place) > 1){
        place = unique(place)
        place = paste(place,collapse = "; ")
      }
      #entities(a_doc,kind = "person")
      #entities(a_doc,kind = "organization")
      col1 = c(col1,file)
      col2 = c(col2,place)
      print(j)
    }
  data.frame(file = col1, place = col2)
}

place1 = character()
formcol = function(num,filenum){
  folder = list.files()[num]
    file = list.files(path=folder)[filenum]
    a0 = readLines(paste(folder,file,sep = "/"))
    a = a0
    a = paste(a, collapse = " ")
    a = gsub("<.*?>"," ",a)
    a = gsub("\\s+"," ",a)
    a_annotations <- annotate(a, pipeline)
    a_doc <- AnnotatedPlainTextDocument(a, a_annotations)
    
    place = entities(a_doc,kind = "location")
    if(length(place) == 0){
      place = NA
    }
    if(length(place) > 1){
      place = unique(place)
      place = paste(place,collapse = "; ")
    }
    c(file,place)
}

