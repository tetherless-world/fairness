library(XML)
library(RCurl)
library(rdflib)
library(stringi)
library(memoise)

# MODE_REMOTE == TRUE: Query the endpoint, save result in cache
# MODE_REMOTE == FALSE: Query the cache, use rdflib as a fallback
MODE_REMOTE <- FALSE

sparqlns <- c('s'='http://www.w3.org/2005/sparql-results#')
commonns <- c('xsd','<http://www.w3.org/2001/XMLSchema#>',
              'rdf','<http://www.w3.org/1999/02/22-rdf-syntax-ns#>',
              'rdfs','<http://www.w3.org/2000/01/rdf-schema#>',
              'owl','<http://www.w3.org/2002/07/owl#>',
              'skos','<http://www.w3.org/2004/02/skos/core#>',
              'dc','<http://purl.org/dc/elements/1.1/>',
              'foaf','<http://xmlns.com/foaf/0.1/>',
              'wgs84','<http://www.w3.org/2003/01/geo/wgs84_pos#>',
              'qb','<http://purl.org/linked-data/cube#>')

sparqltest <- function(...) {
  SPARQL(url='http://semanticweb.cs.vu.nl/lop/sparql/',
         query='SELECT ?et ?r ?at ?t 
                WHERE { 
                  ?e sem:eventType ?et .
                  ?e sem:hasActor ?a . 
                  ?a sem:actorType ?at . 
                  ?e sem:hasPlace ?p . 
                  ?p eez:inPiracyRegion ?r .
                  ?e sem:hasTimeStamp ?t . }',
         ns=c('lop','<http://semanticweb.cs.vu.nl/poseidon/ns/instances/>',
              'eez','<http://semanticweb.cs.vu.nl/poseidon/ns/eez/>'),
         ...)
}

#load ontology via rdflib
file <- rdf_parse(".\\fairnessmetrics.owl")

#load cache
cache <<- readRDS(file = "cache.Rds")

# code to reset cache:
#cache <<- cache <- list(query= list(), result= list())

saveToCache <- function(query,result){
  if(!(query %in% cache$query)){
    cache$query <<- append(cache$query,query)
    cache$result <<- append(cache$result,list(result))
    saveRDS(cache, file = "cache.Rds", compress=F)
  }
  else{
    #print("Result already cached")
  }
}

SPARQL <- function(...){
  
  if (MODE_REMOTE){
    #print("querying BLAZEGRAPH...")
    df = SPARQL_remote_cache(...)
  }
  else {
    #print("querying CACHE/RDFLIB...")
    df = SPARQL_local(...)
  }
  
  return(df)
}

SPARQL_local <- memoise(function(url="http://localhost/", query="", update="", 
                             ns=NULL, param="", extra=NULL, format="xml", curl_args=NULL, parser_args=NULL) {
  
  #check cache
  
  qloc <- match(query,cache$query)
  if(!is.na(qloc)){
    return(cache$result[[qloc]])
  }
  
  #try rdflib
  
  query = gsub("\\+","",query)
  query= gsub("FILTER NOT EXISTS \\{\\?x fmo:mapsTo \\?notion_uri\\}","",query)
  query= gsub("FILTER NOT EXISTS {?categorization_uri fmo:tag 'exclude'.}","",query)
  query= gsub("FILTER NOT EXISTS {?category_uri fmo:tag 'exclude'.}","",query)
  #print(query)
  df = rdf_query(file,query)
  #print("")
  #browser()
  if(!is.null(ns)){
    ns_old <- ns
    ns = matrix(ns,ncol=2,byrow=TRUE)
    ns <- gsub("<","",ns)
    ns <- gsub(">","",ns)
    ns[,1] = apply(ns,1,function(x){paste(x[1],":",sep="")})
    
    df <- data.frame(apply(df,2,function(x){
      stri_replace_all_regex(x,ns[,2],ns[,1],vectorize=FALSE)
    }))
    res = list(results=df,namespaces=ns_old)
  }
  else{
    res=list(results=df)
  }
  
  return(res)
})


SPARQL_remote_cache <- memoise(function(url="",query="",...){
  

  #print(query)
  
  result = SPARQL_remote(url,query,...)
  #print("Caching...")
  saveToCache(query,result)
  return(result)
})

#
# Read SPARQL results from end-point
SPARQL_remote <- function(url="http://localhost/", query="", update="", 
                   ns=NULL, param="", extra=NULL, format="xml", curl_args=NULL, parser_args=NULL) {
  if (!is.null(extra)) {
    extrastr <- paste('&', sapply(seq(1,length(extra)),
                                  function (i) { paste(names(extra)[i],'=',URLencode(extra[[i]]), sep="") }),
                      collapse="&", sep='')
  } else {
    extrastr <- ""
  }
  tf <- tempfile()
  if (query != "") {
    if (param == "") {
      param <- "query"
    }
    if(format == 'xml') {
      tf <- do.call(getURL,append(list(url = paste(url, '?', param, '=', gsub('\\+','%2B',URLencode(query,reserved=TRUE)), extrastr, sep=""),
                                       httpheader = c(Accept="application/sparql-results+xml")),
                                  curl_args))
      DOM <- do.call(xmlParse, append(list(tf), parser_args))
      if(length(getNodeSet(DOM, '//s:result[1]', namespaces=sparqlns)) == 0) {
        rm(DOM)
        df <- data.frame(c())
      } else {
        attrs <- unlist(xpathApply(DOM,
                                   paste('//s:head/s:variable', sep=""),
                                   namespaces=sparqlns,
                                   quote(xmlGetAttr(x, "name"))))			
        ns2 <- noBrackets(ns)
        res <- get_attr(attrs, DOM, ns2)

        df <- data.frame(res)

		rm(res)
        rm(DOM)
        
        # FIXME: find neater way to unlist columns
        n = names(df)
        for(r in 1:length(n)) {
          name <- n[r]
          df[name] <- as.vector(unlist(df[name]))
        }
        
      }
    } else if (format == 'csv') {
      tf <- do.call(getURL,append(list(url=paste(url, '?', param, '=', gsub('\\+','%2B',URLencode(query,reserved=TRUE)), extrastr, sep="")),
                                  curl_args))
      df <- do.call(readCSVstring,append(list(tf, blank.lines.skip=TRUE, strip.white=TRUE),
                                         parser_args))
      if (!is.null(ns)) 
        df <- dropNS(df,ns)
    } else if (format == 'tsv') {
      tf <- do.call(getURL,append(list(url=paste(url, '?', param, '=', gsub('\\+','%2B',URLencode(query,reserved=TRUE)), extrastr, sep="")),
                                  curl_args))
      df <- do.call(readTSVstring,append(list(tf, blank.lines.skip=TRUE, strip.white=TRUE),
                                         parser_args))
      if (!is.null(ns)) 
        df <- dropNS(df,ns)
    } else {
      cat('unknown format "',format,'"\n\n',sep="")
      return(list(results=NULL,namespaces=ns))
    }
    list(results=df, namespaces=ns)
  } else if (update != "") {
    if (param == "") {
      param <- "update"
    }
    extra[[param]] <- update
    do.call(postForm,append(list(url, .params=extra),curl_args))
  }
}

readTSVstring <- function(text, ...) {
  dfr <- read.delim(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

readCSVstring <- function(text, ...) {
  dfr <- read.csv(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

get_attr <- function(attrs, DOM, ns) {
  rs <- getNodeSet(DOM,'//s:result',namespaces=sparqlns)
  t(sapply(rs,
           function(r) { 
             sapply(attrs,
                    function(attr) {
                      get_value(getNodeSet(xmlDoc(r),
                                           paste('//s:binding[@name="',attr,'"]/*[1]',
                                                 sep=''),
                                           namespaces=sparqlns)[[1]],
                                ns)
                    },simplify=FALSE)
           },simplify=TRUE))
}

get_value <- function(node,ns) {
  # FIXME: very slow...
  if (is.null(node)) { return(NA) }
  doc <- xmlDoc(node)
  uri = xpathSApply(doc, '/s:uri', xmlValue, namespaces=sparqlns)
  if(length(uri) == 0) {
    literal = xpathSApply(doc, '/s:literal', xmlValue, namespaces=sparqlns)
    if(length(literal) == 0) {
      bnode = xpathSApply(doc, '/s:bnode', xmlValue, namespaces=sparqlns)
      if (length(bnode) == 0) { # error
        '***oops***'
      } else { # found bnode
        paste('_:genid', bnode, sep='')
      }
    } else { # found literal
      lang = xpathApply(doc, '/s:literal', xmlGetAttr, "xml:lang", namespaces=sparqlns)
      if(is.null(lang[[1]])) {
        type = xpathApply(doc, '/s:literal', xmlGetAttr, "datatype", namespaces=sparqlns)
        if(is.null(type[[1]])) {
          literal
        } else {
          interpret_type(type,literal,ns)
        }
      } else {
        paste('"', literal, '"@', lang, sep='')
      }
    }
  } else { # found URI
    qname = qnames(uri, ns)
    if(qname == uri)
      paste('<', uri, '>', sep="")
    else
      qname
  }
}

noBrackets <- function(ns) {
  sapply(ns,function(br_ns) {
    if(substr(br_ns,1,1)=='<')
      substr(br_ns,2,nchar(br_ns)-1)
    else
      br_ns
  })
}

substNS <- function(str0, ns) {
  regex <- paste('^', ns[2], sep="")
  gsub(regex, paste(ns[1], ":", sep=""), str0)
}

qnames <- function(str0, ns_list) {
  if(!length(ns_list))
    str0
  else
    substNS(qnames(str0, ns_list[-1:-2]), ns_list[1:2])
}

interpret_type <- function(type, literal,ns) {
  qname <- qnames(type, ns)
  if(unlist(qname) == unlist(type))
    type_uri <- paste('<', type, '>', sep="")
  else
    type_uri <- qname
  # FIXME: work out all simple types
  if(type == "http://www.w3.org/2001/XMLSchema#double" ||
    type == "http://www.w3.org/2001/XMLSchema#float" ||
    type == "http://www.w3.org/2001/XMLSchema#decimal")
    as.double(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#integer" ||
    type == "http://www.w3.org/2001/XMLSchema#int" ||
    type == "http://www.w3.org/2001/XMLSchema#long" ||
    type == "http://www.w3.org/2001/XMLSchema#short" ||
    type == "http://www.w3.org/2001/XMLSchema#byte" ||
    type == "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" ||
    type == "http://www.w3.org/2001/XMLSchema#unsignedLong" ||
    type == "http://www.w3.org/2001/XMLSchema#unsignedShort" ||
    type == "http://www.w3.org/2001/XMLSchema#unsignedInt" ||
    type == "http://www.w3.org/2001/XMLSchema#unsignedByte" ||
    type == "http://www.w3.org/2001/XMLSchema#positiveInteger" ||
    type == "http://www.w3.org/2001/XMLSchema#nonPositiveInteger" ||
    type == "http://www.w3.org/2001/XMLSchema#negativeInteger")
  as.integer(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#boolean")
    as.logical(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#string" ||
    type == "http://www.w3.org/2001/XMLSchema#normalizedString")
    literal
  else if(type == "http://www.w3.org/2001/XMLSchema#dateTime")
    as.POSIXct(literal,format="%FT%T")
  else if(type == "http://www.w3.org/2001/XMLSchema#time")
    as.POSIXct(literal,format="%T")
  else if(type == "http://www.w3.org/2001/XMLSchema#date")
    as.POSIXct(literal)
  else if(type == "http://www.w3.org/2001/XMLSchema#gYearMonth")
    as.POSIXct(literal,format="%Y-%m")
  else if(type == "http://www.w3.org/2001/XMLSchema#gYear")
    as.POSIXct(literal,format="%Y")
  else if(type == "http://www.w3.org/2001/XMLSchema#gMonthDay")
    as.POSIXct(literal,format="--%m-%d")
  else if(type == "http://www.w3.org/2001/XMLSchema#gDay")
    as.POSIXct(literal,format="---%d")
  else if(type == "http://www.w3.org/2001/XMLSchema#gMonth")
    as.POSIXct(literal,format="--%m")
  else
    paste('"', literal, '"^^', type_uri, sep="")
}

dropNS <- function(df,ns) {
  data.frame(lapply(df, 
                    function(c) {
                      if(is.factor(c)) {
                        c <- as.character(c)
                        c <- qnames(c,ns)
                        return(as.factor(c))
                      }
                      if (is.character(c))
                        return(qnames(c,ns))
                      return(c)
                      } ))
}
