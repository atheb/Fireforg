var fireforg_zotero = {
    injectZoteroAccordingToPref: function () {
        if( fireforg.getPreferenceManager().getBoolPref("extensions.fireforg.injectZotero") ) {
            if( Zotero.Translate ) {
                if( !Zotero.Translate.prototype.fireforg_runHandler ) {
                    Zotero.Translate.prototype.fireforg_runHandler = Zotero.Translate.prototype.runHandler;
                    Zotero.Translate.prototype.runHandler = function(type, argument) {
                        if( type == "itemDone") {
                            fireforg.zoteroItemDoneHandler( argument );
                        }
                        return this.fireforg_runHandler(type, argument);
                    }
                }
            }
        } else { // remove injection
            if( Zotero.Translate() ) {
                if( Zotero.Translate.prototype.fireforg_runHandler ) {
                    Zotero.Translate.prototype.runHandler = Zotero.Translate.prototype.fireforg_runHandler;
                }
            }
        }
    },
    // additional "itemDone" handler for Zoteros translator
    // item : Zotero.item that has been processed
    zoteroItemDoneHandler: function (item) {

        /*alert("itemDoneHandler called with: \n" +
          "type: " + Zotero.ItemTypes.getName(item.getType()) + "\n" + 
          "title: " + item.getDisplayTitle(true) + "\n");*/
        window.setTimeout( function () {
                //alert( "item.attachments : " + item.getAttachments());

                var translatorObj = new Zotero.Translate("export"); // create Translator for export
                translatorObj.setItems( [ item ]);
                translatorObj.setTranslator( "9cb70025-a888-4a29-a210-93ec52da40d4" ); // set Translator to use the BibTex translator
                translatorObj.setHandler("done", fireforg._zoteroTranslationDone);
                // deinject runHandler to avoid recursion
                translatorObj.runHandler = translatorObj.fireforg_runHandler;
                translatorObj.translate(); }
            , 2000);

    
    },
    _zoteroTranslationDone: function (obj, worked) {
        if( !worked ) {
            //            alert("Fireforg: Zotero BibTex export failed!");
        } else {
            var bibtex =  obj.output.replace(/\r\n/g, "\n");
            //alert("BibTex: " + bibtex );
            
            // Send to org
            fireforg_protocol.orgProtocolSendURL("fireforg-bibtex-entry://" + encodeURIComponent( bibtex ) ); 
        }
    }
}

