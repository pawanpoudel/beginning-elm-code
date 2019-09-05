// Create a simple string
var someString = "some string";

// Update string 10,000 times
function updateString() {
    for (var i = 0; i <= 10000; i++) {
        someString = "updated string"
    };
}

// Find out how long it takes to update a string 10,000 times
var t1 = performance.now();
updateString();
var t2 = performance.now();

console.log("It took " + (t2 - t1) + " milliseconds to update a string.");


// Create new nodes
var newDiv = document.createElement("div");
var newText = document.createTextNode("some text");

// Add new nodes to the DOM tree
newDiv.appendChild(newText);
document.body.appendChild(newDiv);

// Update the text node inside div 10,000 times
function updateDOM() {
    for (var i = 0; i <= 10000; i++) {
        newDiv.innerHTML = "updated text";
    }
}

// Find out how long it takes to update a DOM element
var t3 = performance.now();
updateDOM();
var t4 = performance.now();

console.log("It took " + (t4 - t3) + " milliseconds to update a DOM element.");

var _List_sortWith = F2(function(f, xs) {
    return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
        var ord = A2(f, a, b);
        return ord === __Basics_EQ ? 0 : ord === __Basics_LT ? -1 : 1;
    }));
});
