

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


function doXHReq(url){
    return function (succeed){
        return doXHR(url, succeed, logger)
    }

}    


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


