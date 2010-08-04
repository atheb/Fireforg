var fireforg_protocol = {
    orgProtocolSendURL: function (url) {
        if( fireforg_pref.macWorkaround() ) { // Workaround
            var tmpFileName = fireforg_pref.macWorkaroundFile();
            var file = Components.classes["@mozilla.org/file/local;1"]
            .createInstance(Components.interfaces.nsILocalFile);
            file.initWithPath( tmpFileName );
            if(file.exists() == false) {
                file.create( Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 420);
            }

            var stream = Components.classes["@mozilla.org/network/file-output-stream;1"]
            .createInstance(Components.interfaces.nsIFileOutputStream);

            stream.init(file, 0x02 | 0x08 | 0x10, 0666, 0);
            var finalString = "org-protocol://" + url + "\n";
            stream.write( finalString, finalString.length);
            stream.close();

        } else {
            switch( fireforg_pref.orgProtocolSendMethod() ) {

            case "EMACSCLIENT": var req = new XMLHttpRequest();
            try {
                req.open('POST', "org-protocol://" + url,true);
                req.send(null);
            } catch (ex) { }
            break;
            
            case "HTTPD":
            var req = new XMLHttpRequest();
            try {
                req.open('GET', "http://localhost:" + fireforg_pref.httpPort() + "/org-protocol://" + url,true);
                req.send(null);
            } catch (ex) { }
            break;
            }
        }
    },
    orgProtocolRemember: function ( rememberTemplate, urlO, titleO ) {
        if( !urlO || urlO === "" ) urlO = window.content.document.URL;
        if( !titleO || titleO === "" ) titleO = document.title;
        if( rememberTemplate && rememberTemplate != "" )
            rememberTemplate = rememberTemplate + "/";
        else
            rememberTemplate = "";
        fireforg_protocol.orgProtocolSendURL("remember://" + rememberTemplate + encodeURIComponent(urlO) + "/" + encodeURIComponent(titleO) + "/" + encodeURIComponent(window.getSelection()));
    },
    orgProtocolStoreLink: function ( link, title) {
        if( !link || link === "" )
            link = window.content.document.URL;
        if( !title || title === "" )
            title = document.title;
	fireforg_protocol.orgProtocolSendURL("store-link://" + encodeURIComponent(link) + "/" + encodeURIComponent(title));
    },
    orgProtocolShowAnnotation: function (file, heading, encoded, headingPoint) {   
        if( !encoded ) {
	    file = encodeURIComponent( file );
            heading = encodeURIComponent( heading ); }
	fireforg_protocol.orgProtocolSendURL("fireforg-show-annotation://" + file + "/" + heading + "/" + headingPoint);
    }
}