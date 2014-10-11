var virtualize = require('vdom-virtualize');
var diff = require('virtual-dom/diff');
var patch =require('virtual-dom/patch');
var h =require('virtual-dom/h');

window.onload = function () {
    var evtSource = new EventSource("/events");
    var div = document.getElementById("content");
    var lastVnode = h('div');
    evtSource.onmessage = function(e) {
        var vnode = virtualize.fromHTML("<div>" + e.data.replace(/\\n/g, "\n") + "</div>");

        var patches = diff(lastVnode, vnode);
        patch(div, patches);

        lastVnode = vnode;
        //MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
}
