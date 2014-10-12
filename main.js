var virtualize = require('vdom-virtualize');
var diff = require('virtual-dom/diff');
var patch =require('virtual-dom/patch');
var h = require('virtual-dom/h');
var createElement = require('virtual-dom/create-element');

window.onload = function () {
    var evtSource = new EventSource("/events");
    var div = document.getElementById("content");
    var lastVnode = h('div');
    var root = createElement(lastVnode);
    div.appendChild(root);
    var i = 0;
    evtSource.onmessage = function(e) {
        i ++;
        var vnode = virtualize.fromHTML("<div>" + e.data.replace(/\\n/g, "\n") + "</div>");
        var domNode = createElement(vnode);
        var hidden = createElement(
            h('div', {
                style: {
                    visibility: "hidden",
                    position: "absolute",
                    top: "0"
                }
            })
        );
        document.body.appendChild(hidden);
        hidden.appendChild(domNode);
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, hidden, function() {
            var newTree = virtualize(hidden.firstChild);
            var patches = diff(lastVnode, newTree);
            root = patch(root, patches);
            document.getElementsByTagName('html')[0].scrollTop = root.scrollHeight;
            lastVnode = newTree;
        }]);
    }
}
