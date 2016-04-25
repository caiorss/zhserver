if (!String.format) {
  String.format = function(format) {
    var args = Array.prototype.slice.call(arguments, 1);
    return format.replace(/{(\d+)}/g, function(match, number) { 
      return typeof args[number] != 'undefined'
        ? args[number] 
        : match
      ;
    });
  };
}


function Option (t, v) {

    this.t = t; 
    this.v = v;

    this.isSome = function (x) {
        return this.t === "Some"; 
    }

    this.isNone = function (x) {
        return this.t === "None";
    }
  

    this.map = function ( fn) {

        if (this.isSome ()) {
            return Option.unit(fn(this.v))
        }
        else {
            return this 
        }        
    }

    this.bind = function (fn) {
        if (this.isSome ()) {
            return fn(this.v)
        }
        else {
            return this 
        }
    }
    
}

Option.unit = function (x) {
    var opt = new Option ("Some", x);
    return opt 
}

Option.some = function (x) {
    return new Option("Some", x)
}

Option.none = new Option("None", null)




function logger(x){
   console.log(x)
};

var map = function (fn, cont_ar){
    return function (fn_b_to_r){
        cont_ar(function(a){
           fn_b_to_r(fn(a));
        });                   
   };
};

var unit = function (x){
    return function(cc){
        return cc(x)
    }
}



var lift = function (fn){
   return function (cont_ar){
       return map(fn, cont_ar)
   }
}

var bind = function (c, f){
   return function (k){
      c(function (a){
         f(a)(k)
       })
    };   
   };    


var run = function (m, fn){
    m(fn)
}

function parseJson (text){
    return JSON.parse(text.replace(/(\\\d+)+/g, ' '))
}

function insertJS (text){
    return JSON.parse
}

function appendHtml (element, html){
    var node = document.querySelector(element);
    node.innerHTML += html;
}

function setHtml (element, html){
    var node = document.querySelector(element);
    node.innerHTML = html; 
}

/*  Synchronous http Request. 

The synchronous request must be avoided. It is preferable to use the
asynchronous web request instead.

Despite its disadvantages it is useful to during the development.


method, url

*/
function doXHRSync(method, url){
    
    var request = new XMLHttpRequest();  
    request.open(method, url, false);   
    request.send(null);  
      
    if (request.status === 200) {  

        return Option.some(request.responseText)
        
    } else{
        return Option.none 
    }
    
}

    


function doXHR(url, succeed, fail) {
  var xhr = new XMLHttpRequest(); // or ActiveX equivalent
  xhr.open("GET", url, true);
  xhr.send(null);
  xhr.onreadystatechange = function() {
    if (xhr.readyState == 4) {
      if (xhr.status == 200)
        succeed(xhr.responseText);
      else
        fail(xhr);
    }
  };
}

function doXHReq(url){
    return function (succeed){
        return doXHR(url, succeed, logger)
    }
}    



function collHtmlTemplate (obj){
    // var html = '<a href="javascript:getCollection(' + obj.id '")>' +  obj.coll + "</a>"

    var html = String.format(
        "<li><a href='javascript:showCollectionID({0})'>{1}</a></il>",
        obj["id"],
        obj["coll"])

    return html 
}


function insertCollections  (text){
    var json = parseJson(text)

    json.forEach (function (elem){
        appendHtml("#content", collHtmlTemplate(elem));
    });

    console.log("Loaded")
}


function cleanContentArea (){
    setHtml("#content", "");
}

function showCollections(){
    cleanContentArea();
    doXHR("/api/colls",insertCollections , logger);
    console.log("Displayed Collections OK");
}
