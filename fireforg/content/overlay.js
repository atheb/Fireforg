//  Fireforg - a Firefox extension for org mode interaction

//  Copyright 2009 Andreas Burtzlaff

//  Author: Andreas Burtzlaff < andreas at burtz[REMOVE]laff dot de >
//  Version: 0.1alpha13

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
    jQuery: $mb,
    requestid: 0,
    EMACSCLIENT: "EMACSCLIENT",
    HTTPD: "HTTPD",
    MACWORKAROUND: "MACWORKAROUND",

    // fetch links
    lookupAndModifyLinks: function () {

        // remove existing annotations;
        fireforg.jq(".orgNote").remove();

        var annotationLinkStyle = fireforg_pref.annotationLinkStyle();
        var annotationLinkTooltip = fireforg_pref.annotationLinkTooltip();

        var linksJQ = fireforg.jq("a");


        linksJQ.each( function () {

                var objectJQ = fireforg.jq(this);
                var link = objectJQ.attr("href");
                var url = fireforg.linkToAbsoluteUrl( link, window.content.document.URL );
                var urlMapped = fireforg.linkMapLookup( url );
                //alert("looking up mapped entry " + urlMapped);

                var registryEntry = fireforg.getRegistryEntryFromFileForUrl( url );

                var registryEntryMapped = null;

                // Check whether there is a url mapped to this one
                if( urlMapped && urlMapped != "") {
                    //alert("looking up mapped entry " + urlMapped);
                    registryEntryMapped = fireforg.getRegistryEntryFromFileForUrl( urlMapped );
                }

                //alert( "After urlMapped");
                // if( registryEntryMapped ) {
                //     // Merge
                //     registryEntry = fireforg.jq( registryEntry ).append( fireforg.jq( registryEntryMapped ).children() ).eq(0);
                // }

                if( registryEntry || registryEntryMapped ) {
                    // add class orgNoteLink in order to be able to select the modified elements later (no css info connected with this class)
		    //		    objectJQ.attr("class", objectJQ.attr("class") + " orgNoteLink");
                
		    objectJQ.attr("style", objectJQ.attr("style") + annotationLinkStyle);
		    // in order to be able to remove the inserted style string, it has to be stored separately
		    objectJQ.attr("fireforgStyle", annotationLinkStyle);

                    // generate tooltip text from entry (BUG: Newline doesn't work here. Rewrite with proper menu on mouseover.)
                    if( annotationLinkTooltip ) {
                        var tooltipText = "";
                        if( registryEntry ) 
                            fireforg.jq( registryEntry ).children().each( function () { tooltipText = tooltipText + fireforg.jq( this ).attr("text") + "\n"; });
                        if( registryEntryMapped )
                            fireforg.jq( registryEntryMapped ).children().each( function () { tooltipText = tooltipText + fireforg.jq( this ).attr("text") + "\n"; });
                        objectJQ.attr("title", tooltipText );
		    }
		}

                // If no mapping entry exists and prefetchLinks is on, fetch document, extract doi and add to link map
            if( urlMapped == null && fireforg_pref.prefetchLinks() ) {

                    if( fireforg.prefetchUrlAllowed( url ) ) {
                        //alert("Prefetching link " + url);
                        fireforg.jQuery.get( url, function (htmlText) {
                                // get doi
                                var doi = fireforg_doi.getDOIFromHtml( htmlText );

                                if( doi && doi != "" ) {
                                    var doiURL = fireforg_doi.doiToURL( doi );
                                    // Add resolved URL to link map
                                    fireforg.linkMapAddEntry( url, doiURL );
                                    registryEntry = fireforg.getRegistryEntryFromFileForUrl( doiURL );
                                    if( registryEntry ) {
                                        // Found an entry for the doi retrieved from the linked site.
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
                                } else {
                                    // Add empty entry
                                    fireforg.linkMapAddEntry( url, "");
                                }
                            }, "text");
                    }
                    //     }
                    // };
                    // request.send(null);
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
                  //                fireforg_protocol.orgProtocolSendURL("fireforg-get-annotations://" + encodeURIComponent(fireforg.requestid)  + "/" + encodeURIComponent( window.content.document.URL));
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
        switch( fireforg_pref.orgProtocolSendMethod() ) {
        case fireforg.EMACSCLIENT:
            if( fireforg.loadRegistryFromFile() ) {
                if( fireforg_pref.lookupLinksOnLoad() ) {
                    fireforg.lookupAndModifyLinks();
                }
            } else {
                fireforg.setStatusBarIconError();
            };
            break;
        case fireforg.HTTPD: 
            if( fireforg_pref.lookupLinksOnLoad() ) {
                fireforg.lookupAndModifyLinks();
            }
            break;
        }
    },
    onUrlSwitch: function() {
	        
        if(  !(fireforg_pref.orgProtocolSendMethod() == fireforg.EMACSCLIENT)
           || fireforg.loadRegistryFromFile() ) {

	    // set waiting state
	    fireforg.links = [];
	    fireforg.setStatusBarWaiting();

	    // get URL
	    var url = window.content.document.URL;
	    fireforg.currentLink = url;

            fireforg.currentLinkRegistryEntry = null;
            fireforg.currentHeadingsMatchingDOI = null;

            if( !(fireforg_pref.orgProtocolSendMethod() == fireforg.EMACSCLIENT)
                || fireforg.registryDOM ) {
		
		// get all heading for url
		fireforg.currentLinkRegistryEntry = fireforg.getRegistryEntryFromFileForUrl( url );
                // add all doi matches if enabled
                if( fireforg_pref.matchDOI() ) {
                    var doi = fireforg_doi.getDOIFromHtml( fireforg.jq("html").html() );
                    if( doi ) {
                        fireforg.currentHeadingsMatchingDOI = fireforg.getRegistryEntryFromFileForUrl( fireforg_doi.doiToURL(doi) );
                    } else
                      fireforg.currentHeadingsMatchingDOI = null;
                } else
                    fireforg.currentHeadingsMatchingDOI = null;
                
		// prepare to retrieve tags
		var tags = "";
                var extractTags = function () { 
                   var t = fireforg.jq( this ).attr("tags");
                   if( t && t != "") { tags = tags + t };
                };

                // extract tags from url matches
		fireforg.jq( fireforg.currentLinkRegistryEntry ).children().each( extractTags );
                // extract tags from DOI matches
		fireforg.jq( fireforg.currentHeadingsMatchingDOI ).children().each( extractTags );
                // if( fireforg.currentHeadingsMatchingDOI )
                //     for( i = 0 ; i < fireforg.currentHeadingsMatchingDOI ; i++ )
                //         extractTags( fireforg.currentHeadingsMatchingDOI.snapshotItem(i) );
                //alert( "tags: " + tags );
		if( tags && tags != "" ) {
                    // remove duplicates
                    tags = tags.replace(/:{2,}/g,":").replace(/^:/,"").replace(/:$/,"");
                    //alert( "tags filtered: " + tags );
                    var tagList = tags.split(":").sort();
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
            fireforg.updateStatusBarIcon();

	    } else {
		fireforg.setStatusBarIconError();
	    }

       
        
    },
    loadRegistryFromFile: function () {
	var file = Components.classes["@mozilla.org/file/local;1"]
	.createInstance(Components.interfaces.nsILocalFile);
	file.initWithPath( fireforg_pref.registryFile() );
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

        // add own contentAreaClick to window.
        // This is the function the tabbrowser calls whenever a click in the tab bar occurs.
        
        // if ("contentAreaClick" in window) {
        //     fireforg.__contentAreaClick = window.contentAreaClick;
        //     window.contentAreaClick = fireforg.contentAreaClick;
        // }

	this.initialized = true;
	this.strings = document.getElementById("fireforg-strings");

// If Zotero is to be injected into, then do so after a small delay so
// as to ensure, that the Zotero plugin is actually loaded.
        if( fireforg_pref.injectZotero() )        
            window.setTimeout( fireforg_zotero.injectZoteroAccordingToPref, 5000);

// Define key handler
        // window.onkeypress = function (evt) {
        //     alert("Key pressed: " + evt.charCode);
        // };
        

    },
    onStatusbarIconClicked: function (ev) {
	if( ev.button == 0 )
	    fireforg.showLinkListPopup();
        else {
	    var menu = document.getElementById('fireforg_popup_menu');
            var templateList = fireforg.prefRememberTemplates();
            fireforg.generatePopupMenuStatusBar( menu, templateList );
            menu.openPopup( document.getElementById("fireforg_spi"),"before_end",0,0,false,null);
	}           
    },
    generatePopupMenuStatusBar: function( menu, rememberTemplateList, linkToStoreO, titleToStoreO ) {
        if( !linkToStoreO ) linkToStoreO = "";
        if( !titleToStoreO ) titleToStoreO = linkToStoreO;

   	// remove all children
	while(menu.hasChildNodes()){
	    menu.removeChild(menu.lastChild);}

        // add toggle link prefetching
        var tmpItem = document.createElement("menuitem");
        var linkPrefetchingLabel = "";
        var linkPrefetchNewState = "false";

        if( fireforg_pref.prefetchLinks() ) {
            linkPrefetchingLabel = "Disable link prefetching";
            linkPrefetchNewState = "false";
        } else {
            linkPrefetchingLabel = "Enable link prefetching";
            linkPrefetchNewState = "true";
        }
        tmpItem.setAttribute("class","fireforg-popupmenu");
        tmpItem.setAttribute("label", linkPrefetchingLabel );
        tmpItem.setAttribute("onclick","fireforg_pref.prefetchLinksSet(" + linkPrefetchNewState + ");fireforg.updateStatusBarIcon();");
        menu.appendChild( tmpItem );
 
        tmpItem = document.createElement("menuseparator");
        menu.appendChild( tmpItem );

        // Added "All tabs" entry
        var tmpMenu = document.createElement("menu");
        tmpMenu.setAttribute("class","fireforg-menu");
        tmpMenu.setAttribute("label", "All tabs" );
        menu.appendChild( tmpMenu );        
        var tmpMenuPopup = document.createElement("menupopup");
        tmpMenu.appendChild( tmpMenuPopup );
        { 
            // Fill the "All tabs" menu
            fireforg.appendRememberEntriesToMenu( 
                                             tmpMenuPopup, 
                                             rememberTemplateList, 
                                             function( template ) { return  "remember (" + template + ")" } ,
                                             function( templateString ) { return "fireforg.allTabsRemember(\"" + templateString + "\",\"" + linkToStoreO + "\",\"" + titleToStoreO + "\")"; } );
            tmpMenuPopup.appendChild( tmpItem );
        }
 

        tmpItem = document.createElement("menuseparator");
        menu.appendChild( tmpItem );


        fireforg.generatePopupMenu( menu, rememberTemplateList, linkToStoreO, titleToStoreO );
    },
    generatePopupMenuContext: function ( menu, rememberTemplateList, linkToStoreO, titleToStoreO ) {
        if( !linkToStoreO ) linkToStoreO = "";
        if( !titleToStoreO ) titleToStoreO = linkToStoreO;
        
   	// remove all children
	while(menu.hasChildNodes()){
	    menu.removeChild(menu.lastChild);}

        fireforg.generatePopupMenu( menu, rememberTemplateList, linkToStoreO, titleToStoreO );
    },

    generatePopupMenu: function ( menu, rememberTemplateList, linkToStoreO, titleToStoreO ) {
        
        var tmpItem = document.createElement("menuitem");
        
        // add store link entry
        var tmpItem = document.createElement("menuitem");
        tmpItem.setAttribute("class","fireforg-popupmenu");
        tmpItem.setAttribute("label", "store-link" );
        tmpItem.setAttribute("onclick","fireforg_protocol.orgProtocolStoreLink(\"" + linkToStoreO + "\",\"" + titleToStoreO + "\")");
        menu.appendChild( tmpItem );

        fireforg.appendRememberEntriesToMenu( 
                                             menu, 
                                             rememberTemplateList, 
                                             function( template ) { return  "remember (" + template + ")" } ,
                                             function( templateString ) { return "fireforg_protocol.orgProtocolRemember(\"" + templateString + "\",\"" + linkToStoreO + "\",\"" + titleToStoreO + "\")"; } );
        
        
    },
    // labelFunction: function ( templateString )
    // rteturn the menu entries label
    // jsCodeFunction: function ( templateString )
    // returns the string that is evaluated if the menu entry is clicked
    // 
    appendRememberEntriesToMenu: function( menu, rememberTemplateList, labelFunction, jsCodeFunction ) {
        rememberTemplateList.forEach( function( element ) {
                var tmpItem = document.createElement("menuitem");
                tmpItem.setAttribute("class","fireforg-popupmenu");
                tmpItem.setAttribute("label", labelFunction( element ) );
                tmpItem.setAttribute("onclick", jsCodeFunction( element ) );
                menu.appendChild( tmpItem );
            });
    },
    showLinkListPopup: function () {
   
        if( fireforg.currentLinkRegistryEntry || fireforg.currentHeadingsMatchingDOI ) {
            var popupMenu = document.getElementById('fireforg_popup_dynamic');

            if( fireforg.currentLinkRegistryEntry && !(fireforg.currentHeadingsMatchingDOI) ) 
                fireforg.populateMenuWithAnnotations( popupMenu, fireforg.currentLinkRegistryEntry );
            else if( !(fireforg.currentLinkRegistryEntry && fireforg.jq( fireforg.currentLinkRegistryEntry ).children().length != 0) && fireforg.currentHeadingsMatchingDOI ) {
                fireforg.populateMenuWithAnnotations( popupMenu, fireforg.currentHeadingsMatchingDOI );
            }
            else // both 
                fireforg.populateMenuWithAnnotations( popupMenu, fireforg.removeDuplicateAnnotations( fireforg.jq( fireforg.currentLinkRegistryEntry ).append( fireforg.jq( fireforg.currentHeadingsMatchingDOI ).children() ).eq(0) ) );
            popupMenu.openPopup( document.getElementById("fireforg_spi"),"before_end",0,0,false,null);
        }
    },
    updateStatusBarIcon: function () {
        var linkMatches = 0;
        var doiMatches = 0;
        if( fireforg.currentLinkRegistryEntry )
            linkMatches = fireforg.jq( fireforg.currentLinkRegistryEntry ).children().length;
        if( fireforg.currentHeadingsMatchingDOI ) {
            doiMatches = fireforg.jq( fireforg.currentHeadingsMatchingDOI ).children().length;
            //                    doiMatches = fireforg.currentHeadingsMatchingDOI.snapshotLength;
        }
        fireforg.setStatusBarIconNormal( linkMatches, doiMatches );
    },
    setStatusBarIconNormal: function (matches, matchesDOI) {
        var staticString = "";
        if( fireforg_pref.prefetchLinks() )
            staticString = "PREFETCH";//"<FONT color=\"red\">PREFETCH</FONT>";	

        if( matches == 0 && matchesDOI == 0 ) {
            document.getElementById('fireforg_spi_image').setAttribute("src","chrome://fireforg/skin/org-mode-unicorn_16.png");
            document.getElementById('fireforg_spi_label').setAttribute("value", staticString );
	} else {
            document.getElementById('fireforg_spi_image').setAttribute("src","chrome://fireforg/skin/org-mode-unicorn_16_highlighted.png");
            document.getElementById('fireforg_spi_label').setAttribute("value",staticString + "  (URL:" + matches + ",DOI:" + matchesDOI + ")");
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
    allTabsRemember: function ( template ) {
        for( i = 0 ; i < gBrowser.mTabs.length ; i++ ) {
            var tab = gBrowser.mTabs[i].linkedBrowser;
            var title = tab.contentWindow.document.title;
            var url = tab.contentWindow.location.href;
 //            alert("Tab with title: " + title + " and URL:\n" + url );           
            fireforg_protocol.orgProtocolRemember( 
            template,
            url,
            title);
        }
    },
    contextMenuItemShowing: function (e) {
	//        alert("ContextMenuItemShowing");

        var contextMenuEntry = document.getElementById('fireforg_ctx_menu_fireforg_popup');                

	// remove all children
	while(contextMenuEntry.hasChildNodes()){
	    contextMenuEntry.removeChild(contextMenuEntry.lastChild);}

        if( gContextMenu.onLink ) {
	    var url = gContextMenu.link;

	    var registryEntry = fireforg.getEntriesForUrl( url );

            fireforg.populateMenuWithAnnotations( contextMenuEntry, registryEntry );
             
	} 	
    },
    contextMenuActionsItemShowing: function (e) {
	// alert("ContextMenuActionsItemShowing");

        var contextActionsMenuEntry = document.getElementById('fireforg_ctx_menu_fireforg_popup_actions');                

	// remove all children
	while(contextActionsMenuEntry.hasChildNodes()){
	    contextActionsMenuEntry.removeChild(contextActionsMenuEntry.lastChild);}

        if( gContextMenu.onLink ) {
	    var url = gContextMenu.link;

            fireforg.generatePopupMenuContext( contextActionsMenuEntry, fireforg.prefRememberTemplates(), url);             
	} 	
    },
    // // Context menu for tabs
    // contentAreaClick: function ( evt, noidea ) {
      
    // },

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
                      var headingPoint = headingJQ.attr("point");

                      var tmpItem = document.createElement("menuitem");
                      tmpItem.setAttribute("class","fireforg-popupmenu");
                      tmpItem.setAttribute("label", headingText + "  " + tags );
                      tmpItem.setAttribute("onclick","fireforg_protocol.orgProtocolShowAnnotation(\"" + encodeURIComponent(file) + "\",\"" + encodeURIComponent(headingText) + "\",true,\"" + headingPoint + "\")");
                      menu.appendChild( tmpItem );
                  });

        }
    },


    /* READ PREFERENCES */
    prefRememberTemplates: function () {
        return fireforg_pref.rememberTemplates().split(',');
    },
    /* ACCESS REGISTRY */
    /* Retrieves all entries for url from the registry file.*/
    getRegistryEntryFromFileForUrl: function (url) {
        switch( fireforg_pref.orgProtocolSendMethod() ) {
        case fireforg.EMACSCLIENT:
        try {
	    // the xpath query may be invalid for certain url's
	    return fireforg.registryDOM.evaluate("//link[@url=\"" + url +"\"]", fireforg.registryDOM, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
	} catch (e) {
	    return null;
	}
        break;
        case fireforg.HTTPD:
        //        alert( "Url: " + url);
        var parser = new DOMParser();
        var response = fireforg.jQuery.ajax({ type: "GET",
                                              url: ("http://localhost:15187/org-protocol-http://get-annotations-for-url://" + encodeURIComponent( url ) ),
                                              cache: false,
                                              async: false,
                                              data: "",
                                              timeout: 3000}).responseText;
        //        alert( response );
        var tmpDOM = parser.parseFromString( response, "text/xml");
        var xPathObject = tmpDOM.evaluate("//org-fireforg-get-annotations-for-url", tmpDOM, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
        if( fireforg.jq( xPathObject ).children().size() > 0 ) {
            //            alert("Returning xpath.");          
            return xPathObject;
        } else {
            //            alert("Returning null.");
            return null;
        }
        break;
        }
	
    },
    /* Retrieves all entries for given url from the registry using the
     * cached mappings and the DOI if enabled.  
     *
     * Side effect: fireforg.currentLinkRegistryEntry and
     *              fireforg.currentHeadingsMatchingDOI will be filled.
     *
     * It will _not_ retrieve pages if the given url is not the
     * currently viewed one! */
    getEntriesForUrl: function (url) {
        var urlMapped = fireforg.linkMapLookup( url );

        // get all headings for url
        fireforg.currentLinkRegistryEntry = fireforg.getRegistryEntryFromFileForUrl( url );

        var registryEntryMapped = null;
        // Check whether there is a url mapped to this one
        if( urlMapped && urlMapped != "") {
            registryEntryMapped = fireforg.getRegistryEntryFromFileForUrl( urlMapped );
        } else if( !urlMapped && fireforg_pref.matchDOI() ) {
            var doi = null;
            // See how the site's content can be accessed.
            // If it is the currently viewed url, simply use the content of the <html> tag to search through
            if( window.content.document.URL == url ) {
 doi = fireforg_doi.getDOIFromHtml( fireforg.jq("html").html() );
            } 
            // If it isn't the currently viewed URL the content has to
            // be retrieved. But we won't do that here because of its
            // asynchronous nature. New links in pages are retrieved
            // and handled in fireforg.lookupAndModifyLinks.

            if( doi ) {
                registryEntryMapped = fireforg.getRegistryEntryForLink( fireforg_doi.doiToURL(doi) );

                // add DOI URL to map
                fireforg.linkMapAddEntry( url, fireforg_doi.doiToURL( doi ) );
            } 
        } 

        fireforg.currentHeadingsMatchingDOI = registryEntryMapped;

        if( fireforg.currentLinkRegistryEntry || registryEntryMapped ) {

            if( fireforg.currentLinkRegistryEntry && !(registryEntryMapped) ) 
                return fireforg.currentLinkRegistryEntry;
            else if( !(fireforg.currentLinkRegistryEntry && fireforg.jq( fireforg.currentLinkRegistryEntry ).children().length != 0) && registryEntryMapped ) {
                return registryEntryMapped;
            }
            else // both 
                return fireforg.removeDuplicateAnnotations( fireforg.jq( fireforg.currentLinkRegistryEntry ).append( fireforg.jq( registryEntryMapped ).children() ).eq(0) ) ;
        } else
            return null;

                
    },

    /* LINK MAP */
    linkMap: new Array(),
    // Adds a mapping from url 'src' to url 'dst'.
    // If dst is undefined it is set to "".
    linkMapAddEntry: function ( src, dst ) {
        if( !dst )
            dst = "";
        fireforg.linkMap[src] = dst;
    },
    // Looks up 'url' in the link map
    // returns: If 'url' in link map: the associated url or ""
    //          else: null
    linkMapLookup: function ( url ) {
        return fireforg.linkMap[ url ];
    },
    /* PREFETCHING */
    prefetchUrlAllowed: function (url) {
        if( url ) {
            ;;            return url.match(/^http:\/\//i) && !url.match(/.*\.pdf$/i) && !url.match(/.*\.gif$/i) && !url.match(/.*\.png$/i) && !url.match(/.*\.swf$/i);
            var whitelistRegexp = fireforg_pref.prefetchLinks_whitelistRegexp();
            var blacklistRegexp = fireforg_pref.prefetchLinks.blacklistRegexp();
            return url.match( RegExp(whitelistRegexp) ) && !url.match( RegExp( blacklistRegexp ) );
        }        
        else
            return false;
    },
    linkToAbsoluteUrl: function (link, baseUrl) {

        if( link.match(/^[^:]{1,5}:/i) )
            return link;
        var baseDir = baseUrl.replace(/\/[^\/]+$/, "/");
        var rootDir = baseDir.match(/^[^\/]*\/\/[^\/]*\//i);
        var basePath = baseDir.substring( rootDir[0].length );
        var newPath = "";
        if( link.match(/^\/(.*)/i) ) {
            newPath = RegExp.$1;
        } else {
            newPath = basePath + link;
        }
        // replace /./
        newPath = newPath.replace(/^\.\//, "");
        newPath = newPath.replace(/\/\.\//g, "/");
        // replace ..
        var tmpPath = "";
        while( (newPath = newPath.replace(/[^\/]*\/\.\.\//g, "") ) != tmpPath) {
            tmpPath = newPath;
        }
        //alert("linkToAbsoluteUrl: link = " + link + ", rootDir = " + rootDir + "\n=> " + newLink);
        return rootDir + newPath;
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
}

window.addEventListener("load", function(e) { fireforg.onLoad(e); }, false);