## FMO Interface Library

## Helper code to interface between FMO (hosted on the Blazegraph instance
## here: https://lp01.idea.rpi.edu/blazegraph/) and R

source("SPARQL.R") # requires library(XML) & library(RCurl)
library(dplyr)
library(stringr)

## Setup

# Sets some global vars

#endpoint <- "http://10.0.0.80:9999/blazegraph/namespace/6-9-2023/sparql"
endpoint <- "http://10.0.0.83:9999/blazegraph/namespace/6-24-2023/sparql"

#endpoint <- "https://lp01.idea.rpi.edu/blazegraph/namespace/fmo-6-15-2023/sparql"

options <- NULL

ret_prefix <- c(
  'dc','<http://purl.org/dc/elements/1.1/>',
  'skos','<http://www.w3.org/2004/02/skos/core#>',
  'fmo','<https://purl.org/twc/fmo#>',
  'rdfs','<http://www.w3.org/2000/01/rdf-schema#>',
  'dc', '<http://purl.org/dc/elements/1.1/>',
  'iao', '<http://purl.obolibrary.org/obo/IAO_>',
  'owl', '<http://www.w3.org/2002/07/owl#>',
  'sio', '<http://semanticscience.org/resource/SIO_>'
)

sparql_prefix <- "
  PREFIX fmo: <https://purl.org/twc/fmo#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX dc: <http://purl.org/dc/elements/1.1/>
  PREFIX iao: <http://purl.obolibrary.org/obo/IAO_>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX sio: <http://semanticscience.org/resource/SIO_>
"

## Function Library

#Populate categories pane:

get_categories <- function() {
  q <- paste(sparql_prefix,"
    SELECT ?category_label ?category_uri 
    WHERE {
       ?category_uri rdfs:subClassOf fmo:fairness_notion.
       ?category_uri rdfs:label ?category_label_.
       BIND(str(?category_label_) AS ?category_label).
    }
  ")
  q <- paste(sparql_prefix,"
  SELECT DISTINCT ?categorization_label ?categorization_uri ?categorization_header ?category_label ?category_uri ?category_header ?query_notion ?ord ?ord2
  WHERE {
    ?categorization_uri a fmo:fairness_notion_categorization.
    ?categorization_uri rdfs:label ?categorization_label_.
    OPTIONAL{?categorization_uri skos:prefLabel ?categorization_preflabel_.}
    BIND(str(COALESCE(?categorization_preflabel_,?categorization_label_)) as ?categorization_label).
    OPTIONAL{?categorization_uri fmo:category_header ?categorization_header_.}
    BIND(str(COALESCE(?categorization_header_,?categorization_preflabel_,?categorization_label_)) AS ?categorization_header).
    ?category_uri skos:topConceptOf ?categorization_uri.
    ?category_uri rdfs:label ?category_label_.
    OPTIONAL{?category_uri skos:prefLabel ?category_preflabel_.}
    BIND(str(COALESCE(?category_preflabel_,?category_label_)) as ?category_label).
    OPTIONAL{?category_uri fmo:category_header ?category_header_.}
    BIND(str(COALESCE(?category_header_,?category_preflabel_,?category_label_)) AS ?category_header).
    OPTIONAL{?categorization_uri fmo:display_order ?ord.}
    OPTIONAL{?category_uri fmo:display_order ?ord2.}
    ?categorization_uri fmo:queryNotion ?query_notion_.
    BIND(str(?query_notion_) AS ?query_notion).
    FILTER NOT EXISTS {?categorization_uri fmo:tag 'exclude'.}
    FILTER NOT EXISTS {?category_uri fmo:tag 'exclude'.}
  }
  ORDER BY ?ord ?ord2
  ")
  res <- SPARQL(endpoint,q,ns=ret_prefix,extra=options)$results
  #browser()
  # replace variables in the query snippet
  res <- res %>% 
    mutate(query_notion = str_replace(query_notion, "\\?category_uri", category_uri))
  
  return(res)
}


# return the valid subcategories for each supercategory
get_subcategories <- function(selected_category_uri = NULL) {
  if (!is.null(selected_category_uri)){
    q <- paste(sparql_prefix,"
  SELECT DISTINCT ?categorization_label ?categorization_uri ?categorization_header ?category_label ?category_uri ?category_header ?query_notion ?ord ?ord2
  WHERE {
    BIND(",selected_category_uri," AS ?supercategory_uri)
    ?supercategory_uri skos:related ?category_uri.
    ?category_uri skos:topConceptOf ?categorization_uri.
    ?categorization_uri a fmo:fairness_notion_subcategorization.
    ?categorization_uri rdfs:label ?categorization_label_.
    OPTIONAL{?categorization_uri fmo:category_header ?categorization_header_.}
    BIND(str(COALESCE(?categorization_header_,?categorization_preflabel_,?categorization_label_)) AS ?categorization_header).
    OPTIONAL{?categorization_uri skos:prefLabel ?categorization_preflabel_.}
    BIND(str(COALESCE(?categorization_preflabel_,?categorization_label_)) as ?categorization_label).
    ?category_uri rdfs:label ?category_label_.
    OPTIONAL{?category_uri skos:prefLabel ?category_preflabel_.}
    OPTIONAL{?category_uri fmo:category_header ?category_header_.}
    BIND(str(COALESCE(?category_header_,?category_preflabel_,?category_label_)) AS ?category_header).
    BIND(str(COALESCE(?category_preflabel_,?category_label_)) as ?category_label).
    OPTIONAL{?categorization_uri fmo:display_order ?ord.}
    OPTIONAL{?category_uri fmo:display_order ?ord2.}
    ?categorization_uri fmo:queryNotion ?query_notion_.
    BIND(str(?query_notion_) AS ?query_notion).
  }
  ORDER BY ?ord ?ord2
  ")
  } else {
    q <- paste(sparql_prefix,"
  SELECT DISTINCT ?supercategorization_uri ?supercategory_uri ?categorization_label ?categorization_uri ?category_label ?category_uri ?query_notion ?super_ord ?super_ord2 ?ord ?ord2
    WHERE {
      ?supercategorization_uri a fmo:fairness_notion_categorization.
      ?supercategory_uri skos:topConceptOf ?supercategorization_uri.
      ?supercategory_uri skos:related ?category_uri.
      ?category_uri skos:topConceptOf ?categorization_uri.
      ?categorization_uri a fmo:fairness_notion_subcategorization.
      ?categorization_uri rdfs:label ?categorization_label_.
      OPTIONAL{?categorization_uri skos:prefLabel ?categorization_preflabel_.}
      BIND(str(COALESCE(?categorization_preflabel_,?categorization_label_)) as ?categorization_label).
      ?category_uri rdfs:label ?category_label_.
      OPTIONAL{?category_uri skos:prefLabel ?category_preflabel_.}
      BIND(str(COALESCE(?category_preflabel_,?category_label_)) as ?category_label).
      OPTIONAL{?supercategorization_uri fmo:display_order ?super_ord.}
      OPTIONAL{?supercategory_uri fmo:display_order ?super_ord2.}
      OPTIONAL{?categorization_uri fmo:display_order ?ord.}
      OPTIONAL{?category_uri fmo:display_order ?ord2.}
      ?categorization_uri fmo:queryNotion ?query_notion_.
      BIND(str(?query_notion_) AS ?query_notion).
    }
    ORDER BY ?super_ord ?super_ord2 ?ord ?ord2
    ")
  }
  res <- SPARQL(endpoint,q,ns=ret_prefix,extra=options)$results
  
  # replace variables in the query snippet
  if (length(res) > 0){
    res <- res %>% 
      mutate(query_notion = str_replace(query_notion, "\\?category_uri", category_uri))
  }
  
  return(res)
}


#Populate list pane:

get_notions <- function(selected_categorizations = NULL) {
  
  filter_cat <- ""
  if (!is.null(selected_categorizations) && length(selected_categorizations)!=0){
    categorizations <- lapply(selected_categorizations,function(cats) {
      if(length(cats)==0) return("")
      return(paste("{?notion_uri rdfs:subClassOf+",cats,".}",collapse=' UNION '))
    })
    filter_cat <- paste(categorizations,collapse='\n')
  }
  
  filter_cat <- ""
  if (!is.null(selected_categorizations) && length(selected_categorizations)!=0){
    categorizations <- lapply(selected_categorizations,function(cat_querynotion) {
      if(length(cat_querynotion)==0) return("")
      return(paste("{",cat_querynotion,"}",collapse=' UNION '))
    })
    filter_cat <- paste(categorizations,collapse='\n')
  }
  
  q <- paste(sparql_prefix,"
    SELECT DISTINCT ?notion_label ?definition ?notion_uri ?ord
    WHERE {
       ?notion_uri rdfs:subClassOf+ fmo:fairness_notion.
       FILTER (NOT EXISTS {?x fmo:mapsTo ?notion_uri}
         && NOT EXISTS {?notion_uri fmo:tag 'hide'} 
         && NOT EXISTS {?notion_uri rdfs:subClassOf+ [fmo:tag 'exclude']}
       )
       ?notion_uri rdfs:label ?notion_label_.
       BIND(str(?notion_label_) AS ?notion_label).
       ?notion_uri skos:definition ?definition.
       BIND(IF(EXISTS{?notion_uri rdfs:subClassOf+ fmo:regression_fairness_notion},1,2) AS ?ord)
       ",filter_cat,"
    } ORDER BY ?ord
  ")
  res <- SPARQL(endpoint,q,ns=ret_prefix,extra=options)$results
  
  #if(length(res)==0){
    #print("Warning: get notions query returned zero results!")
    #print("")
    #print(q)
    #print("res:")
    #print(res)
  #}
  
  return(res)
}

get_named_notions <- function(cat = NULL) {
  
  notions <- get_notions(cat)
  
  named_notions <- notions$notion_uri
  names(named_notions) <- notions$notion_label
  
  if(is.null(named_notions)) named_notions <- "";
  
  return(named_notions)
}

get_metrics <- function(selected_categorizations = NULL) {
  
  filter_cat <- ""
  if (!is.null(selected_categorizations) && length(selected_categorizations)!=0){
    categorizations <- lapply(selected_categorizations,function(cats) {
      if(length(cats)==0) return("")
      return(paste("{?notion_uri rdfs:subClassOf+",cats,".}",collapse=' UNION '))
    })
    filter_cat <- paste(categorizations,collapse='\n')
  }
  
  filter_cat <- ""
  if (!is.null(selected_categorizations) && length(selected_categorizations)!=0){
    categorizations <- lapply(selected_categorizations,function(cat_querynotion) {
      if(length(cat_querynotion)==0) return("")
      return(paste("{",cat_querynotion,"}",collapse=' UNION '))
    })
    filter_cat <- paste(categorizations,collapse='\n')
  }
  
  q <- paste(sparql_prefix,"
  SELECT DISTINCT ?metric_uri ?metric_label ?notion_label ?definition ?notion_uri ?ord
  WHERE {
    ?metric_uri rdfs:subClassOf+ fmo:fairness_metric.
    ?metric_uri rdfs:label ?metric_label_.
    FILTER (NOT EXISTS {?metric_uri fmo:tag 'hide'} 
            && NOT EXISTS {?metric_uri rdfs:subClassOf+ [fmo:tag 'exclude']}
           )
    BIND(str(?metric_label_) AS ?metric_label).
    ?metric_uri skos:definition ?definition.
    
    ?metric_uri rdfs:subClassOf+ [a owl:Restriction; owl:onProperty sio:000215; owl:someValuesFrom fmo:fairness_notion].
    OPTIONAL{
      ?metric_uri rdfs:subClassOf+ [a owl:Restriction; owl:onProperty sio:000215; owl:someValuesFrom ?notion_uri_].
      ?notion_uri_ rdfs:subClassOf+ fmo:fairness_notion.
    }
    BIND(COALESCE(?notion_uri_,fmo:fairness_notion) AS ?notion_uri)
    
    OPTIONAL {
      ",filter_cat,"
      BIND('TRUE' AS ?matches_filters )
    }
    
    ?notion_uri rdfs:label ?notion_label_.
    BIND(str(?notion_label_) AS ?notion_label).
    
    BIND(IF(EXISTS{?notion_uri rdfs:subClassOf+ fmo:regression_fairness_notion},1,2) AS ?ord)
    
    FILTER (?notion_uri=fmo:fairness_notion || bound(?matches_filters))
  } ORDER BY ?ord ?notion_label ?metric_label
  ")
  res <- SPARQL(endpoint,q,ns=ret_prefix,extra=options)$results
  return(res)
}

get_named_metrics <- function(cat = NULL) {
  
  metrics <- get_metrics(cat)
  
  named_metrics <- metrics$metric_uri
  names(named_metrics) <- metrics$metric_label
  
  if(is.null(named_metrics)) named_metrics <- "";
  
  return(named_metrics)
}


# Populate class view pane:

get_class_info <- function(class_uri) {
  
    q <- paste(sparql_prefix,"
 
      SELECT DISTINCT ?label ?definition ?mathematical_definition ?probabilistic_definition ?source ?alt_term ?superclass_uri ?superclass_label ?notion_uri ?notion_label ?metric_uri ?metric_label
      WHERE {
          BIND (",class_uri," AS ?class_uri)
          ?class_uri rdfs:label ?label_.
          BIND(str(?label_) AS ?label).
          {
          ?class_uri skos:definition ?definition_.
          BIND(str(?definition_) AS ?definition).
          }
          UNION {
          ?class_uri dc:source ?source_.
          BIND(str(?source_) AS ?source).
          }
          UNION {
          ?class_uri iao:0000118 ?alt_term_.
          BIND(str(?alt_term_) AS ?alt_term).
          }
          UNION {
            ?class_uri fmo:mathematical_definition ?mathematical_definition_.
            BIND(str(?mathematical_definition_) AS ?mathematical_definition).
          }
          UNION {
            ?class_uri fmo:probabilistic_definition ?probabilistic_definition_.
            BIND(str(?probabilistic_definition_) AS ?probabilistic_definition).
          }
          UNION {
            ?class_uri rdfs:subClassOf+ ?superclass_uri.
            ?superclass_uri rdfs:label ?superclass_label_.
            BIND(str(?superclass_label_) AS ?superclass_label).
          }
          UNION {
            ?class_uri rdfs:subClassOf+ fmo:fairness_metric.
            ?class_uri rdfs:subClassOf [a owl:Restriction; owl:onProperty sio:000215; owl:someValuesFrom ?notion_uri].
            ?notion_uri rdfs:subClassOf* fmo:fairness_notion.
            ?notion_uri rdfs:label ?notion_label_.
            BIND(str(?notion_label_) AS ?notion_label).
          }
          UNION {
            ?class_uri rdfs:subClassOf+ fmo:fairness_notion.
            ?metric_uri rdfs:subClassOf+ fmo:fairness_metric.
            ?metric_uri rdfs:subClassOf [a owl:Restriction; owl:onProperty sio:000215; owl:someValuesFrom [rdfs:subClassOf* ?class_uri]].
            ?metric_uri rdfs:label ?metric_label_.
            BIND(str(?metric_label_) AS ?metric_label).
          }
      }
    ")

    res <- SPARQL(endpoint,q,ns=ret_prefix,extra=options)$results
    return(res)
  }

#         OPTIONAL{?source_ rdfs:label ?source_label_.}
#         BIND(str(COALESCE(?source_label_.,?source_)) as ?source_label).

# Cleans a column up for display -- 
#   returning either the first result (for single=TRUE), 
#   or all results with NAs removed (single=FALSE)
clean_col <- function(col, single=TRUE) {
  uniques <- unique(na.omit(col))
  if(single){
    if(length(uniques)==0) return("")
    if(length(uniques) > 1){
      #print("WARNING: Returned too many results, taking first result from list")
      #print(" Results returned:")
      #print(paste(" -",uniques))
      return(uniques[1])
    }
  }
  return(uniques)
}
