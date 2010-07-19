var fireforg_doi = {
    /* DOI HANDLING */
    getDOIFromHtml: function( html ) {
        // Note: this presumes that "<" and ">" are encoded in the DOI identifier
        // The DOI specification only _recommends_ "<" and ">" to be encoded inside xml documents.
        // Let's hope everybody does so...
        var doiRegexp = /doi ?[:\/] ?([0123456789]+\.[^ \/]+\/[^ <>\"]+)/i;
        var doiRegexpResult = doiRegexp.exec( html );
        if( !doiRegexpResult || doiRegexpResult.length < 1 ) {
            return null;
        } else {
            return doiRegexpResult[1];
        }
    },
    doiToURL: function ( string ) {
        return "http://dx.doi.org/" + string.replace(/%/g,"%25").replace(/"/g,"%22").replace(/#/g,"%23").replace(/ /g,"%20"); // "
                                                                         },
        // removes duplicate heading entries that are children to the given root node
        // simple unoptimized solution
        removeDuplicateAnnotations: function  ( rootNode ) {
            
            var childrenArray = fireforg.jQuery.makeArray( rootNode.children() );
                        
            var allChildren = rootNode.children();
            var uniqueChildren = allChildren;
            //            var test = fireforg.jQuery.unique( fireforg.jQuery.makeArray( allChildren ) );
            rootNode = rootNode.empty();
            allChildren.each( function () {
                    var currentEntry = this;
                    uniqueChildren = uniqueChildren.filter( function () { 
                            return !( (fireforg.jq(this).attr("file") == fireforg.jq(currentEntry).attr("file")
                                       && (fireforg.jq(this).attr("point") == fireforg.jq(currentEntry).attr("point"))  )) }).add( currentEntry );

                        });
                return rootNode.empty().append( uniqueChildren );
        }

        };
}