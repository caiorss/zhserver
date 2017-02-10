

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


/*---------------  URL Formats ---------------------- */ 

function makeCollectionURL(collID, name){
    return "/#!colls?id=" + collID.toString() + "&name=" + name 
}

function makeTagURL(tagID, name){
    return "/#!tags?id=" + tagID.toString() + "&name=" + name 
}


function makeAuthorURL(authorID, name){
    return "#!author?id=" + authorID.toString();
}



function insertItemTypes (urlFunction, idLabel, valLabel){

    return function (json) {

        var docFragment = document.createDocumentFragment();   


        
        json.forEach (function (e){

            var name = e[valLabel];
            var id   = e[idLabel];
            
  //          console.log(e)

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

function bulletList(alist){

    var liList = alist.map(function (e){
        return $h("li").append(e);
    })
        
    var lu = $h("lu").appendMany(liList);        
    return lu;
}

/// Display a single Zotero Item given its json data
///
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

    var tagLinks = json["tags"].map(function (row) {
        return $h("a").set({
              href:     makeTagURL(row[0], row[1])
            , target:  "_blank"
            , child:   row[1]
        });
    });

    var authorLinks = json["authors"].map(function(author){
        return $h("a").set({
             href:    "/#!author?id=" + author.id
            ,target:  "_blank"
            ,child:   author.first + " " + author.last 

        });

    });

    

    
    var collsLinks = json["colls"].map(function (row) {
        return $h("a").set({
              href:     makeCollectionURL(row[0], row[1])
            , target:  "_blank"
            , child:   row[1]
        });
    })


    
   var table =  htmlTable().setRows(
        [
            ["id",            itemID.toString()],
            ["url",           urlLink],          
            ["Download",      downloadLink],
            ["Authors",       bulletList(authorLinks).set({"class": "itemAttribRow"}) ],            
            ["Collections",   bulletList(collsLinks).set({"class": "itemAttribRow"}) ],            
            ["Tags",          bulletList(tagLinks).set({"class": "itemAttribRow"}) ]
            // ["Abstract",      data["abstractNote"]]

        ]
   )

    table.table.set({"class": "itemTable"});

    var tagswords = json["tags"].map(function (e) {return e[1];});

    var datawords = title.toLowerCase().split(" ");
    
    
    var div = $h("div").set({
        //"child"    : [ head.node, table.node],
        "data-words"  :  datawords,
        "data-tags"   :  tagswords,
        "class"       : "filterItem zoteroItem",
    });

    div.append(head.node);
    div.append(table.table.node);
    div.append($h("h4").set({child: "Abstract"}));
    div.append($h("p").set({child:  data["abstractNote"]}));

    return div ;

} // End of funtion jsonToZoteroItemDOM



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

insertCollections = insertItemTypes (makeCollectionURL, "id", "name")

function showCollections () {
    setPageTitle("Collections");
    cleanContentArea();    
    doXHR("/api/colls", compose(parseJson, insertCollections) , logger);
    console.log("Displayed Collections OK");    
}

//------------ Show Not in Collection ------------- //



//------------- Show CollectionID --------------//

function showCollectionID(collUri){

    // setPageTitle ("Collection ID: " + collID.toString())

    name = collUri.split("&")[1].split("=")[1]
    
    setPageTitle("Collection: " + name);
    
    cleanContentArea();
    
    showZoteroItemsFromUrl("/api/colls?id=" + collUri);
}


//-------------- Show Tags ---------------------//

insertTags = insertItemTypes(makeTagURL, "id", "name")



function showTags () {
    console.log("Tags");
    setPageTitle("Tags");
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



//---------------- Show Authors ---------------------//



function insertAuthors (json){
    
        var docFragment = document.createDocumentFragment();   
        
        json.forEach (function (e){

            var first = e["first"];
            var last  = e["last"];
            var id    = e["id"];
            var name  = first + " " + last
            
//            console.log(e)

            var words = name
                .split(/\s,?\.?/)
                .map(function (e){ return e.toLowerCase ()})
            
            var a = $h("a").set(
                {
                    "href": makeAuthorURL(id, name),
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



function showAuthors () {

    console.log("Show Authors");
    
    setPageTitle("Authors");
    
    cleanContentArea();    

    doXHR("/api/authors", compose(parseJson, insertAuthors) , logger);

    //console.log("Displayed Collections OK");    
}




//---------------- Show Author ID ------------------//

function showAuthorID (id) {    
    
    setPageTitle("Author: ");
    
    cleanContentArea();    
    
    showZoteroItemsFromUrl("/api/authors?id=" + id);
    
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

} // End of function filterData



function searchByTitleLike (search){
    var url = "/api/search?title=" + "%" + search + "%";
    setPageTitle("Search: " + search);
    cleanContentArea();
    showZoteroItemsFromUrl(url);       
}


function searchByContentAndTitleLike (search){
    var url = "/api/search?content=" + search
    setPageTitle("Search: " + search);    
    cleanContentArea();        
    showZoteroItemsFromUrl(url);       
}

  
function searchByItemID (search){
    var url = "/api/item?id=" + search
    setPageTitle("Search ItemId = " + search);
    cleanContentArea();
    showZoteroItemsFromUrl(url);
}


function searchItems () {
    
    var search = document.getElementById("searchbox").value;
    var option = document.getElementById("searchSelection").value;        

    switch (option){

    case "title":
        searchByTitleLike (search);
        break;

    case "content":
        searchByContentAndTitleLike (search); 
        break;
    }    
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

    } else if (route == "authors"){

        showAuthors ();

    } else if (route.match (/colls\?id=(.+)/)){

        var collID = route.match (/colls\?id=(.+)/)[1];
        showCollectionID(collID);

    } else if (route.match (/tags\?id=(.+)/)){

        var tagdata = route.match (/tags\?id=(.+)&name=(.+)/)        
        
//     var tagID   = tagdata[2];
//     var tagName = tagdata[3];

        var tagID = route.match (/tags\?id=(.+)/)[1];

  
        showTagID(tagID);

    } else if (route.match (/author\?id=(.+)/)){

        var id = route.match (/author\?id=(.+)/)[1];
        showAuthorID(id);

    }


    else{
        
        showCollections ();
    }

}


document.addEventListener('DOMContentLoaded', function() {

    $d("#filterbox").setAttr("onkeypress", filterData);
    //    $d("#filterbox").setAttr("onchange",   filterData) ;

    $d("#ButtonDoSearch").setAttr("onclick", searchItems);
    
    routeDispatcher ();
    
}, false);


function testData (){
    cleanContentArea();
    var data = parseJson($d("#testItem").html())
    var x    = displayZoteroItem(data)
    $d("#content").append(x.node)

    
    
}  // ----------------------------------------//


window.addEventListener("hashchange", routeDispatcher)



