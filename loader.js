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




function cleanContentArea (){
    
    setHtml("#content", "");

    // Scroll to top
    scroll(0,0) ;
}


function setPageTile (title){

    document.querySelector("#pageTitle").textContent = title; 
}


/* Forward Composition */
function compose(){

    var functions = Array.from(arguments);

     logger(functions);
    // logger(typeof(arguments));
    
    return function (x){
        return functions.reduce(function (acc, fn){
            return fn(acc)
        },
        x
        )
    }
}




/*
    xmlNode("a", 
            {"href": "http://www.google.com", "id": "linkRef"}, 
            "Google")

    "<a id='linkRef' href='http://www.google.com' >Google</a>"

*/

function xmlNode(tag, attributes, content){
    var tagStart = "<" + tag + " " ;
    var tagEnd =   "</" + tag + ">" ;

    var attrs = reduceKV(attributes, "",
                         function(acc, k, v){
                             return k + '=' + "'" + v + "'" + " " + acc 
                      })
    
    return tagStart + attrs + ">" + content + tagEnd
}



function forEachKV(obj, fn){    
    return Object.keys(obj).forEach( function(key) { 

        fn( key , obj[key] )
        
    })
}


function mapKV(obj, fn){    
    return Object.keys(obj).map( function(key) { 
        fn( key , obj[key] ) 
    })
}

function reduceKV(obj, acc0, fn){    
    return Object.keys(obj).reduce( function(acc, key) { 
       return fn(acc, key , obj[key] ) 

    }, acc0 )
}

function arrayToObj(arr){
    var obj = {};

    arr.forEach(function(x){
        if (x[1] != null){
                       obj[x[0]] = x[1] }
    })
    
    return obj;
}


function htmlTag_li (content){
    return xmlNode("li", {}, content)
}


/*
   > joinStrBy("-", ["23", "safsd", "gssdgsdf", "aaa"])
   "23-safsd-gssdgsdf-aaa"

*/
function joinStrBy(common, strlist){
    return strlist.reduce(function (acc, x){
        return acc + common + x
    })
}

function htmlBulletList(items, attributes){

    var content =  joinStrBy("\n",
                             items.map(function (e){
                                 return xmlNode ("li", {}, e);
                             }))
            
    return xmlNode("lu", attributes, content)
}


function htmlPropertyTable(labels, labelsValues, properties){

    var rows = labels.map(function (key){
        return xmlNode("tr", {}, joinStrBy("", [xmlNode("td", {}, key),
                                                xmlNode("td", {}, labelsValues[key])
                                               ]))
                        
    });

    var rowsHtml = joinStrBy("\n", rows);
    
    return xmlNode("table", properties, rowsHtml) ; 
};

function displayZoteroItem(json){

    console.log(json);
    
    var data = arrayToObj(json["data"]);
    var title = data["title"]
    var url = data["url"]
    var itemID = json["id"] ;
    var file = json["file"];

    var downloadLink = xmlNode("a", {"href": "/attachment/" + file
                                     ,"target": "_blank"
                                    },                               
                               "Download")
    
    var sourceLink = xmlNode("a", {"href": url,
                                   "target": "_blank"},
                             "Url")


    var html = htmlPropertyTable(["Title", "ItemID", "Url", "File"],
                                 {
                                     "Title" : title,
                                     "ItemID": itemID,
                                     "Url"   : sourceLink,
                                     "File"  : downloadLink
                                 },
                                 
                                 {"class": "itemTable"}
                                )
    
    // var bulletList = htmlBulletList([title,
    //                                  itemID,
    //                                  sourceLink,
    //                                  downloadLink ], {})
    
    // var html = xmlNode("div", {"class": "zoteroItem"}, bulletList)

    
    appendHtml("#content", html)


} ; /* End of displayZoteroitem */

function displayZoteroItems(items){
    items.forEach(displayZoteroItem);
}





function collHtmlTemplate (obj){
    // var html = '<a href="javascript:getCollection(' + obj.id '")>' +  obj.coll + "</a>"

    var html = String.format(
        "<li class='filterItem'><a href='javascript:showCollectionID({0})'>{1}</a></il>",
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



function showCollections(){
    setPageTile("All Collections");
    cleanContentArea();
    doXHR("/api/colls",insertCollections , logger);
    console.log("Displayed Collections OK");
}




function showZoteroItemsFromUrl(url){       
    
    cleanContentArea();
      
    doXHR(url,
          function (data){
              var js = parseJson(data);

              console.log(js);
              
              displayZoteroItems(js);
          },
          logger
         );    
}



function showCollectionID(id){
    
    var url = "/api/colls?id=" + id.toString();   
    showZoteroItemsFromUrl(url);
}


function showTagID(id){
    var url = "/api/tags?id=" + id.toString();   
    showZoteroItemsFromUrl(url);
}



function showAuthorID(id){
    var url = "/api/authors?id=" + id.toString();   
    showZoteroItemsFromUrl(url);
}




function liftMap (fn){
    return function (xs) {
        return xs.map(fn)
    }
}

function insertTag (tag){
    var a = xmlNode("a", {"href": String.format("javascript:showTagID({0})", tag.id)}, tag.name)
    var t = xmlNode("li", {"class": "filterItem"}, a)
    appendHtml("#content", t)
}

function showTags(){

    setPageTile("All Tags");
    cleanContentArea();    
    doXHR("/api/tags", compose(parseJson, liftMap(insertTag)) , logger);
}

function insertAuthor (author){
    
    var a = xmlNode("a",
                    {"href": String.format("javascript:showAuthorID({0})", author.id)},
                     author.first + " " + author.last);
    
    var t = xmlNode ("li", {"class": "filterItem"}, a);

    appendHtml("#content", t);        
}


function  showAuthors (){

    setPageTile ("All Authors");
    
    cleanContentArea();    
    doXHR("/api/authors", compose(parseJson, liftMap(insertAuthor)) , logger);
}



function domElements(cssSelector){
    return Array.from(document.querySelectorAll(cssSelector));
}


function domElement(cssSelector){
    return document.querySelector(cssSelector)
}


function filterNodes (pattern){
    domElements(".filterItem a")
        .filter (function (e) { return !e.text.toLowerCase().trim().startsWith(pattern.toLowerCase()) })
        .forEach (function (e) { e.parentElement.style.visibility = "collapse";
                                 e.parentElement.style.display = "none";
                               })
}

function filterDisplay (){
    input = domElement("#filterbox").value;
    showNodes ();
    
    if (input != "") {   filterNodes(input); }       
}

function showNodes (){
    domElements(".filterItem a")
        .forEach(function (e) { e.style.visibility = "visible";
                                e.parentElement.style.display = "block";
                              }) 
}



    
// Run JavaScript Only After Entire Page Has Loaded
//
// window.load = function() {
//     console.log ("Loading");
//     showCollections ();
//     console.log("Loaded");
//     // code here
// };

document.addEventListener('DOMContentLoaded', function() {


    domElement("#filterbox").onkeypress = filterDisplay ;
    domElement("#filterbox").onchange = filterDisplay ;        
    
    console.log ("Loading");
    showCollections ();
    console.log("Loaded");
}, false);

/*
    obj = {"x": "world",
           "y": "hello",
           "z": "some",
          }

    reduceKV(obj, "", function(acc, key, val){
        return ( key + "=" + val + ", " + acc )
    })
    
    "z=some, y=hello, x=world, "
    
*/

