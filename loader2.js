

function liftMap (fn){
    return function (xs) {
        return xs.map(fn)
    }
}


// function parseJSRouter (){
//     var hashPath = location.hash.replace(/^#!?/, "");
//     var path = 
// }


function setPageTitle (title){

    //document.querySelector("#pageTitle").textContent = title;
    $d("#pageTitle").setAttr("textContent", title); 
}


function parseJson (text){
    return JSON.parse(text.replace(/(\\\d+)+/g, ' '))
}


function cleanContentArea (){    
    $d("#content").setHtml("");
    // Scroll to top
    scroll(0,0) ;    
} //---------------------------//



function showCollectionURL(collID, name){
    return "/#!colls?id=" + collID.toString() + "&name=" + name 
}

function showTagURL(tagID, name){
    return "/#!tags?id=" + tagID.toString() + "&name=" + name 
}


function showAuthorURL(authorID, name){
    return "#!author?id=" + authorID.toString();
}



function insertItemTypes (urlFunction, idLabel, valLabel){

    return function (json) {

        var docFragment = document.createDocumentFragment();   


        
        json.forEach (function (e){

            var name = e[valLabel];
            var id   = e[idLabel];
            
            console.log(e)

            var words = name
                .split(/\s,?\.?/)
                .map(function (e){ return e.toLowerCase ()})
            
            var a = $h("a").set(
                {
                    "href": urlFunction(id, name),
                    "child": name 
                });
            
            var li = $h("li").set({"class"      : "filterItem",
                                   "child"      : a.node,
                                   "data-words" : words,
                                   "data-id"    : id.toString(),
                                   
                                  })

            docFragment.appendChild(li.node)
        });

        $d("#content").append(docFragment);

    }
    
} // End of insertItemtypes -------------------------//


//---------------- Display Items ------------------- //

function arrayToObj(arr){
    var d = {};

    arr.forEach(function(k){ d[k[0]] = k[1]});

    return d;
}

function jsonToZoteroItemDOM(json){
    
    var data = arrayToObj(json["data"]);

   // console.log(data)
    
    var title = data["title"]
    var url = data["url"]
    var itemID = json["id"] ;
    var file = json["file"];


    var downloadLink = $h("a").set({
        href:   "/attachment/" + file,
        target: "_blank",
        child:  "Download"
    })

    var urlLink = $h("a").set({
        href   :   url,
        target : "_blank",
        child  : "Link"
    })

    // console.log(title);
    
    var head = $h("h3").set({
        "child":        title,
        "data-words":   "" //title.split()
    })

    
   var table =  htmlTable().setRows(
        [
            ["id",  itemID.toString()],
            ["url", urlLink],          
            ["Download", downloadLink]

        ]
   )

    table.table.set({"class": "itemTable"})

    var datawords = title.toLowerCase().split(" ")
    
    var div = $h("div").set({
        //"child"    : [ head.node, table.node],
        "data-words"  :  datawords,
        "class"       : "filterItem zoteroItem",
    })

    div.append(head.node);
    div.append(table.table.node);

    return div 

}



function showZoteroItemsFromUrl(url){       
          
    doXHR(url,
          
          function (data){

              var anchor = $d("#content");
              var json = parseJson(data);
              var docFragment = document.createDocumentFragment();
              
              
              
              json.forEach(function (itemjson){
                  zotItem = jsonToZoteroItemDOM(itemjson);
                  docFragment.appendChild(zotItem.node);
              });
              
              anchor.append(docFragment)
          },
          
          logger
         );    
}


//---------------- Show Collections -----------------//

insertCollections = insertItemTypes (showCollectionURL, "id", "coll")

function showCollections () {
    setPageTitle("All Collections");
    cleanContentArea();    
    doXHR("/api/colls", compose(parseJson, insertCollections) , logger);
    console.log("Displayed Collections OK");    
}

//------------- Show CollectionID --------------//

function showCollectionID(collUri){

    // setPageTitle ("Collection ID: " + collID.toString())

    name = collUri.split("&")[1].split("=")[1]
    
    setPageTitle("Collection: " + name);
    
    cleanContentArea();
    
    showZoteroItemsFromUrl("/api/colls?id=" + collUri);
}


//-------------- Show Tags ---------------------//

insertTags = insertItemTypes(showTagURL, "id", "name")



function showTags () {

    console.log("Show Tags");
    
    setPageTitle("All Tags");
    
    cleanContentArea();    

    doXHR("/api/tags", compose(parseJson, insertTags) , logger);

    //console.log("Displayed Collections OK");    
}


//---------------- Show Tag ID ------------------//



function showTagID (tagURI) {

    tagname = tagURI.split("&")[1].split("=")[1]
    
    setPageTitle("Tag: " + tagname);
    
    cleanContentArea();    
    
    showZoteroItemsFromUrl("/api/tags?id=" + tagURI);
    
}




//------------- Input box filter ---------------//

function displayAll(){
    $d(".filterItem").show ();
}


function isInArray(elem, array) {
     return array.indexOf(elem) > -1
}

function searChByWords (words, array){
    
    return ! (any.apply(null, words.map(x => isInArray(x, array))))
    // return ! (any (words.map( x => isInArray(x, array))))
}


function filterData () {    
    
    var input = document.querySelector("#filterbox")
        .value
        .trim()
        .toLowerCase()
        // .split(" ")

    displayAll();

    if (input == "") { return }

    $d(".filterItem")
        .filter( e =>
                 !(any.apply(null, e.dataset.words
                           .split(",")
                           .map( w => w.match(new RegExp(input))))))
        .hide()
    
}


function showAuthors (){

    setPageTitle ("All Authors");
    
    cleanContentArea();
    
    doXHR("/api/authors",
          compose(parseJson,
                  liftMap(insertAuthor)) ,
          logger);
    
} //---------------------------//


function searchByTitleLike (){

    var search = document.getElementById("searchbox").value;
       
    var url = "/api/searchByTitle?like=" + "%" + search + "%";

    setPageTitle("Search: " + search);
    
    cleanContentArea();    
    
    showZoteroItemsFromUrl(url);       
}


function parseRoute (route){
    
    var s = route.replace(/^#!?/, "").split("?")

    //console.log(s)

    if (s.length >= 1){

        var path = s[0].split("/")

        if (s.length == 2){

            var params = {}

            var q = s[1].split("&")
                .forEach(function (e) {

                    var m = e.split("=")
                    params[m[0]] = m[[1]];                    
                })

            return [path, params]
            
        }

        return [path, {}]
    }

    return null
    
} // End of function parseRout


function routeMatch (paths, keys, route, callback){
    var s  = parseRoute(route);
    var p  = s[0];
    var q  = s[1];

    console.log(p);

    if (p[0] == paths[0]){

        

        var test = all(keys.map(function (k) { return k in q}))

        console.log(test);
        
        if (test) {
            callback(paths, keys)
            
            return true 
        }

        return false
    }

    return false
}

// routeMatch(["colls"],
//            ["id", "name"],
//            "#!colls?id=10&name=oic",
//            function (a, b){ alert ("Executed")}
//            )



function routeDispatcher (){
    var route = location.hash.replace(/^#!?/, "");

    //alert(route);
    
    if (route == "tags"  ){
        showTags();
        
    } else if (route == "colls") {

        showCollections ();

    } else if (route.match (/colls\?id=(.+)/)){

        var collID = route.match (/colls\?id=(.+)/)[1];
        showCollectionID(collID);

    } else if (route.match (/tags\?id=(.+)/)){

        var tagdata = route.match (/tags\?id=(.+)&name=(.+)/)        
        
//     var tagID   = tagdata[2];
//     var tagName = tagdata[3];

        var tagID = route.match (/tags\?id=(.+)/)[1];

  
        showTagID(tagID);

    }


    else{
        
        showCollections ();
    }

}


document.addEventListener('DOMContentLoaded', function() {

    $d("#filterbox").setAttr("onkeypress", filterData);
    //    $d("#filterbox").setAttr("onchange",   filterData) ;

    $d("#ButtonDoSearch").setAttr("onclick", searchByTitleLike);
    
    routeDispatcher ();
    
}, false);


function testData (){
    cleanContentArea();
    var data = parseJson($d("#testItem").html())
    var x    = displayZoteroItem(data)
    $d("#content").append(x.node)

    
    
}  // ----------------------------------------//


window.addEventListener("hashchange", routeDispatcher)



