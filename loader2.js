

function liftMap (fn){
    return function (xs) {
        return xs.map(fn)
    }
}



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



function showCollectionURL(collID){
    return String.format("javascript:showCollectionID({0})", collID);
}

function showAuthorURL(id){
    return String.format("javascript:showAuthorID({0})", id)
}

function showTagURL(id){
    return String.format("javascript:showTagID({0})", id)
}


function insertItemTypes (urlFunction, idLabel, valLabel){

    return function (json) {

        var docFragment = document.createDocumentFragment();   


        
        json.forEach (function (e){

            var coll = e[valLabel];
            var id   = e[idLabel];
            
            console.log(e)

            var words = coll
                .split(/\s,?\.?/)
                .map(function (e){ return e.toLowerCase ()})
            
            var a = $h("a").set(
                {
                    "href": urlFunction(id),
                    "child": coll 
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

    var datawords = title.toLowerCase().split(" ")
    
    var div = $h("div").set({
        //"child"    : [ head.node, table.node],
        "data-words"  :  datawords,
        "class"       : "filterItem",
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

function showCollectionID(collID){

    setPageTitle ("Collection ID: " + collID.toString())
    cleanContentArea();
    showZoteroItemsFromUrl("/api/colls?id=" + collID.toString());
}


//-------------- Show Tags ---------------------//

insertTags = insertItemTypes(showTagURL, "id", "name")



function showTags () {
    setPageTitle("All Tags");
    
    cleanContentArea();    

    doXHR("/api/tags", compose(parseJson, insertTags) , logger);

    //console.log("Displayed Collections OK");    
}


//---------------- Show Tag ID ------------------//



function showTagID (tagID) {
    
    setPageTitle("tag ID: " + tagID.toString());
    
    cleanContentArea();    
    
    showZoteroItemsFromUrl("/api/tags?id=" + tagID.toString());
    
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




document.addEventListener('DOMContentLoaded', function() {


    $d("#filterbox").setAttr("onkeypress", filterData);
//    $d("#filterbox").setAttr("onchange",   filterData) ;   
    
    
    console.log ("Loading");
    showCollections ();
    console.log("Loaded");
}, false);


function testData (){
    cleanContentArea();
    var data = parseJson($d("#testItem").html())
    var x    = displayZoteroItem(data)
    $d("#content").append(x.node)
}    
