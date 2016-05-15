
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

function isString(value) {
    return typeof value == 'string' ;
}

//------ Boolean Combinators ------- //

function any(){
    return Array.from(arguments)
        .reduce(function (acc, x)
                { return acc || x},
                false
               )
}

function all(){
    return Array.from(arguments)
        .reduce(function (acc, x)
                { return acc &&  x},
                true
               )
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

mapj = function (fn, obj){    
    return Object.keys(obj).map( function(key) { 
        return fn( key , obj[key] ) 
    })
}

forEachj = function (fn, obj){    
    return Object.keys(obj).forEach( function(key) { 

        fn( key , obj[key] )
        
    })
}

reducej =  function (fn, obj, acc0){    
    return Object.keys(obj).reduce( function(acc, key) { 
        return fn(acc, key , obj[key]) 

    }, acc0 )
}

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

//----------- logger ------------
//
// Function wrapper to console.log 
// 
function logger(x){
   console.log(x)
};

function $h (tag){
    return new HtmlDom(tag);
}

function HtmlDom (tag){

    this.node = document.createElement(tag)

    this.append = function (child){

        if (child instanceof HtmlDom){
            return this._appendElem(child.node);
        }
        else {
            return this._appendElem(child)
        }
 
    }

    this._appendElem = function (child){

        if (typeof child == "undefined"){

            return this;

        } else if(isString(child)){
                                       
         this.node.appendChild(document.createTextNode(child))
                    
        }

        else {
                    
            this.node.appendChild(child);
        }

        return this
        
    } // End of append 
     

    this.appendMany = function (childs){
        
        var dom = this
            
        childs.forEach(function (e){
               
            dom.append(e)
        })

        return this ;
    }

    
    
    this.set = function(jsobj){

        console.log("set");

        var node = this.node;

        var obj = this 
        
        Object.keys(jsobj).forEach (function(key){

            if (key != "child"){
                
                node.setAttribute(key, jsobj[key]);

               // console.log(key);                
               // console.log(node[key]);

            }else{

                var child = jsobj["child"]

                //console.log("Add child");
                
                if (Array.isArray (child)){
                    
                    obj.appendMany(child); 
                }
                else {

                    obj.append(child);
                }
                 
            }
        });

        return this;
        
    } // End of set 

    this.appendTo = function (node){

        node.appendChild(this.node);

        return this;
    }
              

};

function $d (selector) {
    return new DomSelector(selector); 
}


function DomSelector (selector){

    this.init = function  () {
        
        if (isString(selector)){
            this.dom =  Array.from(document.querySelectorAll(selector));
        }
        else {
            this.dom = selector 
        }
    }

    this.init (); 

    this.map = function (fn){
        return new DomSelector(this.dom.map(fn));
    }
    
    this.each = function (fn){
        this.dom.forEach(fn);
    }

    this.filter = function (fn){
        return new DomSelector(this.dom.filter(fn));
    }

    // Return a property of each element of Dom 
    this.getAttr = function (attr) {
        return this.map(function (e){ return e[attr]})
    }

    this.setAttr = function (attr, value){
        this.each (function (e){ e[attr] = value });

        return this 
    }

    this.remove = function () {
        this.each (function (e){ e.remove();})
    }


    this.childNodes = function () {
        return new DomSelector (
            this.dom.map(function (e){
                return Array.from(e.childNodes)
            }))
    }


    this.show = function () {
        this.dom.forEach(function (e){
            e.style.visibility = "visible";
            e.style.display = "block";
        })

        return this;
    }


    this.hide = function () {
        this.dom.forEach(function (e){
            e.style.visibility = "collapse";
            e.style.display = "none"
        })

        return this;

    }


    this.append = function (child) {

        if (typeof child == "undefined"){
            return this;
        }
        
        
        this.dom.forEach(function (e){
            
            if (typeof e != "undefined"){
                e.appendChild(child);
            };
        });   

        return this;

    }
                        


    this.setHtml = function (value){
        this.dom.forEach(function(e){ e.innerHTML = value}) ;
        return this; 
    }

    this.getHtml = function (value){
        return new DomSelector(this.dom.map(function (e)
                                            { return e.innerHTML}))
    }

    this.html = function (value){
        return this.dom[0].innerHTML
    }
    
    this.one = function (){
        return this.dom[0]; 
    };

    this.all = function (){
        return this.dom ;
    };

}

function _htmlTable(){

    this.table = $h("table") 

    this.setRows = function (rows){

        var table = this.table ;        
        
        rows.forEach(function(row){

            var tr = $h("tr");

            console.log("row = ")
            console.log(row);

            row.forEach(function (cell){
                
                console.log(cell);
            
                var td = $h("td").append(cell);
                tr.append(td.node);
            });            
   
            table.append(tr.node)
            
        }) // -----        


        return this
        
    } // End of this.setRows 
}



function htmlTable () {
    return new _htmlTable ();
}
