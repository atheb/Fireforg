var fireforg_pref = {

    getPreferenceManager: function () {
        return Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);
    },
    registryFile: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.registryFile")
    },
    lookupLinksOnLoad: function () {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.lookupLinksOnLoad")
    },
    annotationLinkStyle: function() {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.annotationLinkStyle")
    },
    annotationLinkTooltip: function() {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.annotationLinkTooltip")
    },
    injectZotero: function () {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.injectZotero")
    },
    rememberTemplates: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.rememberTemplates")
    },
    matchDOI: function () {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.matchDOI")
    },
    prefetchLinks: function () {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.prefetchLinks")
    },
    prefetchLinksSet: function (b) {
        fireforg_pref.getPreferenceManager().setBoolPref("extensions.fireforg.prefetchLinks",b);
    },
    prefetchLinks_whitelistRegexp: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.prefetchLinks.whitelistRegexp")
    },
    prefetchLinks_blacklistRegexp: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.prefetchLinks.blacklistRegexp")
    },
    orgProtocolSendMethod: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.orgProtocolSendMethod")
    },
    macWorkaround: function () {
        return fireforg_pref.getPreferenceManager().getBoolPref("extensions.fireforg.macWorkaround")
    },
    macWorkaroundFile: function () {
        return fireforg_pref.getPreferenceManager().getCharPref("extensions.fireforg.macWorkaroundFile")
    },
    httpPort: function () {
        return fireforg_pref.getPreferenceManager().getIntPref("extensions.fireforg.http.port")
    }
}