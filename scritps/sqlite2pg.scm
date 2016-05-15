

(define (get-tables database-file)
  (run/strings (sqlite3
                ,database-file
                "SELECT name FROM sqlite_master WHERE type='table';")
               ))

(define (table-to-csv database-file table)
  (run (sqlite3
        ,database-file        
       ;; (> "table.csv")
        )

       (<<
         ,(string-append
          "\n.headers on "
          "\n.mode csv"
          (string-append
           "\n.output "  table ".csv")
          (string-append
           "\nSELECT * FROM " table " ;")                  
          
       ))))

(define (run/psql query)
  (run (psql -h localhost -U postgres zotero -c ,query)))

(define (copy-table sqliteDb table)
  
  (let ((csv-file  (string-append table ".csv")))
    
    (table-to-csv sqliteDb table)

    (run/psql (string-append "\\copy "
                             table
                             " FROM "
                             (string-append " '" csv-file "' ")
                             " DELIMITER ',' CSV HEADER "
                             ))

    (delete-file csv-file)

    ))

(for-each

 (lambda (table)
            (begin
              (display (string-append "\nPrinting table: " table " "))
              (copy-table "zotero.sqlite" table)
              ))
          
 (get-tables "zotero.sqlite")
 )

;;(table-to-csv "zotero.sqlite" "items")

