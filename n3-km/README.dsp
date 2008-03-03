The SVN version of Henry provides transaction logic constructs allowing
to process audio signals in order to answer a particular query. All of that
is done on-demand (when querying, an analysis process can be triggered).


Using this can will be done through the following interfaces (the first one
is already working)

 * SPARQL access: files accessible through HTTP can be dynamically discovered
 by Henry, if they are mentioned in the SPARQL query
 * Simplified REST access: PUT your audio file, and get back the location of
 the corresponding signal URI, which you might crawl from to get more information



