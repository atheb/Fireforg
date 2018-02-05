var fireforg_doi = {
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
        return "https://doi.org/" + string.replace(/%/g,"%25").replace(/"/g,"%22").replace(/#/g,"%23").replace(/ /g,"%20"); // "
    }
}
