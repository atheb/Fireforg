//  Fireforg - a Firefox extension for org mode interaction

//  Copyright 2009 Andreas Burtzlaff

//  Author: Andreas Burtzlaff < andreas at burtz[REMOVE]laff dot de >
//  Version: 0.1a

//  This file is not part of GNU Emacs.

//  This includes a copy of jquery (http://jquery.com/) under its GPL license
//  (http://docs.jquery.com/Licensing)

//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2, or (at
//  your option) any later version.

//  This program is distributed in the hope that it will be useful, but
//  WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.

//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

var fireforg = {
    // define jquery convenience functions
    jq: function (a) { return $mb( a, window.content.document); },

    requestid: 0,
    // fetch links
    lookupAndModifyLinks: function () {

        // remove existing annotations;
        fireforg.jq(".orgNote").remove();

        var prefManager = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);
        var annotationLinkStyle = prefManager.getCharPref("extensions.fireforg.annotationLinkStyle");
        var annotationLinkTooltip = prefManager.getBoolPref("extensions.fireforg.annotationLinkTooltip");

        var linksJQ = fireforg.jq("a");
        
        linksJQ.each( function () {


                var objectJQ = fireforg.jq(this);
                var url = objectJQ.attr("href");

                var registryEntry = fireforg.getRegistryEntryForLink( url );

                if( registryEntry ) {
                    // add class orgNoteLink in order to be able to select the modified elements later (no css info connected with this class)
		    //		    objectJQ.attr("class", objectJQ.attr("class") + " orgNoteLink");
                
		    objectJQ.attr("style", objectJQ.attr("style") + annotationLinkStyle);
		    // in order to be able to remove the inserted style string, it has to be stored separately
		    objectJQ.attr("fireforgStyle", annotationLinkStyle);

                    // generate tooltip text from entry (BUG: Newline doesn't work here. Rewrite with proper menu on mouseover.)
                    if( annotationLinkTooltip ) {
                       var tooltipText = "";
                       fireforg.jq( registryEntry ).children().each( function () { tooltipText = tooltipText + fireforg.jq( this ).attr("text") + "\n"; });
		       objectJQ.attr("title", tooltipText );
		    }

		}

		/*
                var pos = fireforg.jq(this).offset();
		//                var posRoot = $mb(window.content.document, window.content.document).offset();

		//                var annotation = document.createElement('div');
                //annotation.textContent = "OrgTest!";
		
                var annotation = document.createElement('div');
                annotation.class += "orgNote";
		//                var img = document.createElement('img');
                //img.src = "chrome://fireforg/skin/org-mode-unicorn_16.png";
                //annotation.appendChild( img );
		//                fireforg.orgProtocolSendURL("fireforg-get-annotations://" + encodeURIComponent(fireforg.requestid)  + "/" + encodeURIComponent( window.content.document.URL));
                annotation.textContent = "Org!";
                fireforg.jq( annotation ).css({"border-style":"solid", "border-width" : "1px","background-color" : "gray"});

                fireforg.jq("body").append( annotation );
		var annotationWidth = fireforg.jq("body").children().eq(".orgNote").width();                
                var annotationHeight = fireforg.jq(annotation).height();
                //alert(annotationWidth);
		fireforg.jq(annotation).css( { "position" : "absolute", "left" : "" + (pos.left-40) + "px", "top" : "" + pos.top + "px" ,"z-index" : "3"});
                //$mb( annotation, window.content.document ).css( { "position" : "absolute", "left" : "-10px", "top" : "-10px" ,"z-index" : "3"});
		*/
	    });

    },
    onLoadSite: function () {

	if( fireforg.loadRegistryFromFile() ) {
            var prefManager = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);
            if( prefManager.getBoolPref("extensions.fireforg.lookupLinksOnLoad") )
		fireforg.lookupAndModifyLinks();
	} else {
	    fireforg.setStatusBarIconError();
	}

    },
    onUrlSwitch: function() {
	        
        if( fireforg.loadRegistryFromFile() ) {

	    // set waiting state
	    fireforg.links = [];
	    fireforg.setStatusBarWaiting();

	    // get URL
	    var url = window.content.document.URL;
	    fireforg.currentLink = url;

            if( !fireforg.registryDOM ) {
		fireforg.setStatusBarIconNormal(0);
	    } else {
		
		// get all heading for url
		fireforg.currentLinkRegistryEntry = fireforg.registryDOM.evaluate("//link[@url=\"" + url +"\"]", fireforg.registryDOM, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
		if( fireforg.currentLinkRegistryEntry ) {
		    fireforg.setStatusBarIconNormal( fireforg.jq( fireforg.currentLinkRegistryEntry ).children().length );
		} else {
		    fireforg.setStatusBarIconNormal( 0 );
		}
		// get tags for url, remove duplicates
		var tags = "";
		fireforg.jq( fireforg.currentLinkRegistryEntry ).children().each( function () { var t = fireforg.jq( this ).attr("tags"); if( t != "") { tags = tags + t }; });
		if( tags != "" ) {
		    var tagList = tags.replace("::",":").replace(/^:/,"").replace(/:$/,"").split(":").sort();
		    if( tagList.length > 0 ) {
			var tagNoDup = ":" + tagList[0] + ":";
			for( var i = 1 ; i < tagList.length ; i++ ) {
			    if( tagList[i] != "" && tagList[i] != tagList[i-1] )
				tagNoDup = tagNoDup + tagList[i] + ":";
			}
			fireforg.setStatusBarTags( tagNoDup );
		    } else {
			fireforg.setStatusBarTags("");
		    }
		} else {
		    fireforg.setStatusBarTags("");
		}
	    }

	    } else {
		fireforg.setStatusBarIconError();
	    }

       
        
    },
    loadRegistryFromFile: function () {
	var file = Components.classes["@mozilla.org/file/local;1"]
	.createInstance(Components.interfaces.nsILocalFile);
	file.initWithPath( Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch).getCharPref("extensions.fireforg.registryFile") );
	if ( !file.exists() ) {
	    return false;
	} else {
	    var is = Components.classes["@mozilla.org/network/file-input-stream;1"]
	    .createInstance( Components.interfaces.nsIFileInputStream );
	    is.init( file,0x01, 00004, null);
	    var sis = Components.classes["@mozilla.org/scriptableinputstream;1"]
	    .createInstance( Components.interfaces.nsIScriptableInputStream );
	    sis.init( is );
	    var output = sis.read( sis.available() );
            
	    var parser = new DOMParser();
	    fireforg.registryDOM = parser.parseFromString( output , "text/xml");
	    var registry = fireforg.registryDOM.childNodes;


	    // load registry
	    fireforg.domEntry = document.getElementById("fireforg_registry");
	    if( fireforg.domEntry ) {
		document.removeChild( fireforg.domEntry );
	    }
	    fireforg.domEntry = document.createElement("registry");
	    fireforg.domEntry.id = "fireforg_registry";

	    return true;
	}

    },
    getRegistryEntryForLink: function (url) {
	try {
	    // the xpath query may be invalid for certain url's
	    return fireforg.registryDOM.evaluate("//link[@url=\"" + url +"\"]", fireforg.registryDOM, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
	} catch (e) {
	    return null;
	}
    },
    onLoad: function() {
	// init variables
        fireforg.timeout = window.setTimeout("",10);
        fireforg.timeoutCount = 0;

        // add onclick event handler for status bar icon
        document.getElementById("fireforg_spi").onclick = fireforg.onStatusbarIconClicked;

	// add appcontent hook
	var appcontent = window.document.getElementById("appcontent");
	appcontent.addEventListener("DOMContentLoaded", fireforg.onLoadSite, false);
	appcontent.addEventListener("DOMContentLoaded", fireforg.onUrlSwitch, false);

	// add tab change listener
	var container = gBrowser.tabContainer;
	container.addEventListener("TabSelect", fireforg.onUrlSwitch, false);

        // try to listen to url changes. Found this somewhere on the web. Doesn't work.
	/*var wnd = window.QueryInterface(Components.interfaces.nsIInterfaceRequestor)
	.getInterface(Components.interfaces.nsIWebNavigation)
	.QueryInterface(Components.interfaces.nsIDocShell)
	.QueryInterface(Components.interfaces.nsIInterfaceRequestor)
	.getInterface(Components.interfaces.nsIURIContentListener);
	wnd.parentContentListener = fireforg.onUrlSwitch; 
	*/
	// document.getElementById('urlbar').onchange = fireforg.onUrlSwitch;

	this.initialized = true;
	this.strings = document.getElementById("fireforg-strings");
    },
    onStatusbarIconClicked: function (ev) {
	if( ev.button == 0 )
	    fireforg.showLinkListPopup();
        else {
	    document.getElementById('fireforg_popup_menu').openPopup( document.getElementById("fireforg_spi"),"before_end",0,0,false,null);
	}
           
    },
    showLinkListPopup: function () {
   
        if( fireforg.currentLinkRegistryEntry ) {
           var popupMenu = document.getElementById('fireforg_popup_dynamic');
                
           fireforg.populateMenuWithAnnotations( popupMenu, fireforg.currentLinkRegistryEntry );
           
            popupMenu.openPopup( document.getElementById("fireforg_spi"),"before_end",0,0,false,null);
 
	}
    },
    orgProtocolAcknowledgeResponse: function (id) {
	fireforg.orgProtocolSendURL("fireforg-acknowledge://" + encodeURIComponent(id));
    },
    orgProtocolShowAnnotation: function (file, heading, encoded) {
        
        if( !encoded ) {
	    file = encodeURIComponent( file );
            heading = encodeURIComponent( heading ); }
	fireforg.orgProtocolSendURL("fireforg-show-annotation://" + file + "/" + heading);
    },
    setStatusBarIconNormal: function (matches) {
	
        if( matches == 0 ) {
	  document.getElementById('fireforg_spi_image').setAttribute("src","chrome://fireforg/skin/org-mode-unicorn_16.png");
          document.getElementById('fireforg_spi_label').setAttribute("value","");
	} else {
          document.getElementById('fireforg_spi_image').setAttribute("src","chrome://fireforg/skin/org-mode-unicorn_16_highlighted.png");
          document.getElementById('fireforg_spi_label').setAttribute("value","(" + matches + ")");
	}
    },
    setStatusBarIconError: function () {
	document.getElementById('fireforg_spi_label').setAttribute("value","Error");
        document.getElementById('fireforg_spi_label_tags').setAttribute("value","");
    },
    setStatusBarWaiting: function () {
	document.getElementById('fireforg_spi_label').setAttribute("value","...");
        document.getElementById('fireforg_spi_label_tags').setAttribute("value","");
    },
    setStatusBarTags: function (tagString) {
        document.getElementById('fireforg_spi_label_tags').setAttribute("value",tagString);
    },
    orgProtocolStoreLink: function () {
	fireforg.orgProtocolSendURL("store-link://" + encodeURIComponent(window.content.document.URL),true);// + "/" + encodeURIComponent(document.title), true);
    },
    orgProtocolRemember: function () {
        fireforg.orgProtocolSendURL("remember://" + encodeURIComponent(window.content.document.URL) + "/" + encodeURIComponent(document.title) + "/" + encodeURIComponent(window.getSelection()));
    },
    orgProtocolSendURL: function (url) {
        var req = new XMLHttpRequest();
	try {
	    req.open('POST', "org-protocol://" + url,true);
	    req.send(null);
	} catch (ex) { }
    },
    contextMenuItemShowing: function (e) {
	//        alert("ContextMenuItemShowing");

        var contextMenuEntry = document.getElementById('fireforg_ctx_menu_fireforg_popup');                

	// remove all children
	while(contextMenuEntry.hasChildNodes()){
	    contextMenuEntry.removeChild(contextMenuEntry.lastChild);}

        if( gContextMenu.onLink ) {
	    var url = gContextMenu.link;

	    var registryEntry = fireforg.getRegistryEntryForLink( url );

            fireforg.populateMenuWithAnnotations( contextMenuEntry, registryEntry );
             
	} 

	
    },
    populateMenuWithAnnotations: function (menu, registryEntry ) {
	// remove all children
	while(menu.hasChildNodes()){
	    menu.removeChild(menu.lastChild);}

        if( registryEntry ) {
            fireforg.jq( registryEntry ).children()
            .each( 
                  function () {
                      //                  alert( "This: " + this);
                      var headingJQ = fireforg.jq( this );
                      var file = headingJQ.attr("file");
                      var headingText = headingJQ.attr("text");
                      var tags = headingJQ.attr("tags");

                      var tmpItem = document.createElement("menuitem");
                      tmpItem.setAttribute("class","fireforg-popupmenu");
                      tmpItem.setAttribute("label", headingText + "  " + tags );
                      tmpItem.setAttribute("onclick","fireforg.orgProtocolShowAnnotation(\"" + encodeURIComponent(file) + "\",\"" + encodeURIComponent(headingText) + "\",true)");
                      menu.appendChild( tmpItem );
                  });

        }
    }

};
window.addEventListener("load", function(e) { fireforg.onLoad(e); }, false);

