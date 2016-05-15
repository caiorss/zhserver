CREATE TABLE itemTypes (
    itemTypeID INTEGER PRIMARY KEY,
    typeName TEXT,
    templateItemTypeID INT,
    display INT DEFAULT 1 -- 0 == hide, 1 == display, 2 == primary
);

CREATE TABLE itemTypesCombined (
    itemTypeID INT NOT NULL,
    typeName TEXT NOT NULL,
    display INT DEFAULT 1 NOT NULL,
    custom INT NOT NULL,
    PRIMARY KEY (itemTypeID)
);

CREATE TABLE fieldFormats (
    fieldFormatID INTEGER PRIMARY KEY,
    regex TEXT,
    isInteger INT
);

CREATE TABLE fields (
    fieldID INTEGER PRIMARY KEY,
    fieldName TEXT,
    fieldFormatID INT
    
);

CREATE TABLE fieldsCombined (
    fieldID INT NOT NULL,
    fieldName TEXT NOT NULL,
    label TEXT,
    fieldFormatID INT,
    custom INT NOT NULL,
    PRIMARY KEY (fieldID)
);

CREATE TABLE itemTypeFields (
    itemTypeID INT,
    fieldID INT,
    hide INT,
    orderIndex INT,
    PRIMARY KEY (itemTypeID, orderIndex),
    UNIQUE (itemTypeID, fieldID)    
    
);

CREATE INDEX itemTypeFields_fieldID ON itemTypeFields(fieldID);

CREATE TABLE itemTypeFieldsCombined (
    itemTypeID INT NOT NULL,
    fieldID INT NOT NULL,
    hide INT,
    orderIndex INT NOT NULL,
    PRIMARY KEY (itemTypeID, orderIndex),
    UNIQUE (itemTypeID, fieldID)
);

CREATE INDEX itemTypeFieldsCombined_fieldID ON itemTypeFieldsCombined(fieldID);

CREATE TABLE baseFieldMappings (
    itemTypeID INT,
    baseFieldID INT,
    fieldID INT,
    PRIMARY KEY (itemTypeID, baseFieldID, fieldID)
            
);

CREATE INDEX baseFieldMappings_baseFieldID ON baseFieldMappings(baseFieldID);

CREATE INDEX baseFieldMappings_fieldID ON baseFieldMappings(fieldID);

CREATE TABLE baseFieldMappingsCombined (
    itemTypeID INT,
    baseFieldID INT,
    fieldID INT,
    PRIMARY KEY (itemTypeID, baseFieldID, fieldID)
);


CREATE INDEX baseFieldMappingsCombined_baseFieldID ON baseFieldMappingsCombined(baseFieldID);

CREATE INDEX baseFieldMappingsCombined_fieldID ON baseFieldMappingsCombined(fieldID);

CREATE TABLE charsets (
    charsetID INTEGER PRIMARY KEY,
    charset TEXT UNIQUE
);

CREATE INDEX charsets_charset ON charsets(charset);


CREATE TABLE fileTypes (
    fileTypeID INTEGER PRIMARY KEY,
    fileType TEXT UNIQUE
);

CREATE INDEX fileTypes_fileType ON fileTypes(fileType);

CREATE TABLE fileTypeMimeTypes (
    fileTypeID INT,
    mimeType TEXT,
    PRIMARY KEY (fileTypeID, mimeType)
    
);


CREATE INDEX fileTypeMimeTypes_mimeType ON fileTypeMimeTypes(mimeType);

CREATE TABLE creatorTypes (
    creatorTypeID INTEGER PRIMARY KEY,
    creatorType TEXT
);

CREATE TABLE itemTypeCreatorTypes (
    itemTypeID INT,
    creatorTypeID INT,
    primaryField INT,
    PRIMARY KEY (itemTypeID, creatorTypeID)
    
    
);
CREATE INDEX itemTypeCreatorTypes_creatorTypeID ON itemTypeCreatorTypes(creatorTypeID);
CREATE TABLE syncObjectTypes (
    syncObjectTypeID INTEGER PRIMARY KEY,
    name TEXT
);
CREATE INDEX syncObjectTypes_name ON syncObjectTypes(name);

CREATE TABLE transactionSets (
    transactionSetID INTEGER PRIMARY KEY,
    event TEXT,
    id INT
);

CREATE TABLE transactions (
    transactionID INTEGER PRIMARY KEY,
    transactionSetID INT,
    context TEXT,
    action TEXT
);

CREATE INDEX transactions_transactionSetID ON transactions(transactionSetID);

CREATE TABLE transactionLog (
    transactionID INT,
    field TEXT,
    value NONE,
    PRIMARY KEY (transactionID, field, value)
    
);

CREATE TABLE version (
    schema TEXT PRIMARY KEY,
    version INT NOT NULL
);

CREATE INDEX schema ON version(schema);

CREATE TABLE settings (
    setting TEXT,
    key TEXT,
    value,
    PRIMARY KEY (setting, key)
);
CREATE TABLE syncedSettings (
    setting TEXT NOT NULL,
    libraryID INT NOT NULL,
    value NOT NULL,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    PRIMARY KEY (setting, libraryID)
);
CREATE TABLE items (
    itemID INTEGER PRIMARY KEY,
    itemTypeID INT NOT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT,
    key TEXT NOT NULL,
    UNIQUE (libraryID, key)
    
);

CREATE TABLE itemDataValues (
    valueID INTEGER PRIMARY KEY,
    value TEXT
);

CREATE TABLE itemData (
    itemID INT,
    fieldID INT,
    valueID INT,
    PRIMARY KEY (itemID, fieldID)        
    
);


CREATE INDEX itemData_fieldID ON itemData(fieldID);


CREATE TABLE itemNotes (
    itemID INTEGER PRIMARY KEY,
    sourceItemID INT,
    note TEXT,
    title TEXT      
);

CREATE INDEX itemNotes_sourceItemID ON itemNotes(sourceItemID);

CREATE TABLE itemAttachments (
    itemID INTEGER PRIMARY KEY,
    sourceItemID INT,
    linkMode INT,
    mimeType TEXT,
    charsetID INT,
    path TEXT,
    originalPath TEXT,
    syncState INT DEFAULT 0,
    storageModTime INT,
    storageHash TEXT
     
);


CREATE INDEX itemAttachments_sourceItemID ON itemAttachments(sourceItemID);
CREATE INDEX itemAttachments_mimeType ON itemAttachments(mimeType);
CREATE INDEX itemAttachments_syncState ON itemAttachments(syncState);

CREATE TABLE tags (
    tagID INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    type INT NOT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT,
    key TEXT NOT NULL,
    UNIQUE (libraryID, name, type),
    UNIQUE (libraryID, key)
);

CREATE TABLE itemTags (
    itemID INT,
    tagID INT,
    PRIMARY KEY (itemID, tagID)        
);

CREATE INDEX itemTags_tagID ON itemTags(tagID);

CREATE TABLE itemSeeAlso (
    itemID INT,
    linkedItemID INT,
    PRIMARY KEY (itemID, linkedItemID)        
);


CREATE INDEX itemSeeAlso_linkedItemID ON itemSeeAlso(linkedItemID);

CREATE TABLE creators (
    creatorID INTEGER PRIMARY KEY,
    creatorDataID INT NOT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT,
    key TEXT NOT NULL,
    UNIQUE (libraryID, key)    
);

CREATE INDEX creators_creatorDataID ON creators(creatorDataID);

CREATE TABLE creatorData (
    creatorDataID INTEGER PRIMARY KEY,
    firstName TEXT,
    lastName TEXT,
    shortName TEXT,
    fieldMode INT,
    birthYear INT
);

CREATE INDEX creatorData_name ON creatorData(lastName, firstName);

CREATE TABLE itemCreators (
    itemID INT,
    creatorID INT,
    creatorTypeID INT DEFAULT 1,
    orderIndex INT DEFAULT 0,
    PRIMARY KEY (itemID, creatorID, creatorTypeID, orderIndex)           
);

CREATE TABLE collections (
    collectionID INTEGER PRIMARY KEY,
    collectionName TEXT NOT NULL,
    parentCollectionID INT DEFAULT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT,
    key TEXT NOT NULL,
    UNIQUE (libraryID, key)    
);


CREATE TABLE collectionItems (
    collectionID INT,
    itemID INT,
    orderIndex INT DEFAULT 0,
    PRIMARY KEY (collectionID, itemID)        
);


CREATE INDEX itemID ON collectionItems(itemID);

CREATE TABLE savedSearches (
    savedSearchID INTEGER PRIMARY KEY,
    savedSearchName TEXT NOT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT,
    key TEXT NOT NULL,
    UNIQUE (libraryID, key)
);


-- CREATE TABLE savedSearchConditions (
--     savedSearchID INT,
--     searchConditionID INT,
--     condition TEXT,
--     operator TEXT,
--     value TEXT,
--     required NULL,
--     PRIMARY KEY (savedSearchID, searchConditionID)    
-- );


-- @FIX: This table failed 
--

-- CREATE TABLE deletedItems (
--     itemID INTEGER PRIMARY KEY,
--     dateDeleted DEFAULT CURRENT_TIMESTAMP NOT NULL
-- );


CREATE INDEX deletedItems_dateDeleted ON deletedItems(dateDeleted);


CREATE TABLE relations (
    libraryID INT NOT NULL,
    subject TEXT NOT NULL,
    predicate TEXT NOT NULL,
    object TEXT NOT NULL,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (subject, predicate, object)
);

CREATE INDEX relations_object ON relations(object);

CREATE TABLE libraries (
    libraryID INTEGER PRIMARY KEY,
    libraryType TEXT NOT NULL
);

CREATE TABLE users (
    userID INTEGER PRIMARY KEY,
    username TEXT NOT NULL
);


CREATE TABLE groups (
    groupID INTEGER PRIMARY KEY,
    libraryID INT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    editable INT NOT NULL,
    filesEditable INT NOT NULL
    
);


CREATE TABLE groupItems (
    itemID INTEGER PRIMARY KEY,
    createdByUserID INT NOT NULL,
    lastModifiedByUserID INT NOT NULL        
);


CREATE TABLE fulltextItems (
    itemID INTEGER PRIMARY KEY,
    version INT,
    indexedPages INT,
    totalPages INT,
    indexedChars INT,
    totalChars INT,
    synced INT DEFAULT 0
    
);

CREATE INDEX fulltextItems_version ON fulltextItems(version);

CREATE TABLE fulltextWords (
    wordID INTEGER PRIMARY KEY,
    word TEXT UNIQUE
);

CREATE TABLE fulltextItemWords (
    wordID INT,
    itemID INT,
    PRIMARY KEY (wordID, itemID)        
);

CREATE INDEX fulltextItemWords_itemID ON fulltextItemWords(itemID);

CREATE TABLE syncDeleteLog (
    syncObjectTypeID INT NOT NULL,
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    timestamp INT NOT NULL,
    UNIQUE (syncObjectTypeID, libraryID, key)    
);

CREATE INDEX syncDeleteLog_timestamp ON syncDeleteLog(timestamp);

CREATE TABLE storageDeleteLog (
    libraryID INT,
    key TEXT NOT NULL,
    timestamp INT NOT NULL,
    PRIMARY KEY (libraryID, key)
);

CREATE INDEX storageDeleteLog_timestamp ON storageDeleteLog(timestamp);


-- @FIX: ERROR:  syntax error at or near "offset"
--

-- CREATE TABLE annotations (
--     annotationID INTEGER PRIMARY KEY,
--     itemID INT,
--     parent TEXT,
--     textNode INT,
--     offset INT,
--     x INT,
--     y INT,
--     cols INT,
--     rows INT,
--     text TEXT,
--     collapsed BOOL,
--     dateModified DATE    
-- );

CREATE INDEX annotations_itemID ON annotations(itemID);

CREATE TABLE highlights (
    highlightID INTEGER PRIMARY KEY,
    itemID INTEGER,
    startParent TEXT,
    startTextNode INT,
    startOffset INT,
    endParent TEXT,
    endTextNode INT,
    endOffset INT,
    dateModified DATE
    
);

CREATE INDEX highlights_itemID ON highlights(itemID);

CREATE TABLE proxies (
    proxyID INTEGER PRIMARY KEY,
    multiHost INT,
    autoAssociate INT,
    scheme TEXT
);

CREATE TABLE proxyHosts (
    hostID INTEGER PRIMARY KEY,
    proxyID INTEGER,
    hostname TEXT    
);

CREATE INDEX proxyHosts_proxyID ON proxyHosts(proxyID);

CREATE TABLE customItemTypes (
    customItemTypeID INTEGER PRIMARY KEY,
    typeName TEXT,
    label TEXT,
    display INT DEFAULT 1, -- 0 == hide, 1 == display, 2 == primary
    icon TEXT
);

CREATE TABLE customFields (
    customFieldID INTEGER PRIMARY KEY,
    fieldName TEXT,
    label TEXT
);

CREATE TABLE customItemTypeFields (
    customItemTypeID INT NOT NULL,
    fieldID INT,
    customFieldID INT,
    hide INT NOT NULL,
    orderIndex INT NOT NULL,
    PRIMARY KEY (customItemTypeID, orderIndex)           
);


CREATE INDEX customItemTypeFields_fieldID ON customItemTypeFields(fieldID);

CREATE INDEX customItemTypeFields_customFieldID ON customItemTypeFields(customFieldID);

CREATE TABLE customBaseFieldMappings (
    customItemTypeID INT,
    baseFieldID INT,
    customFieldID INT,
    PRIMARY KEY (customItemTypeID, baseFieldID, customFieldID)          
);

CREATE INDEX customBaseFieldMappings_baseFieldID ON customBaseFieldMappings(baseFieldID);

CREATE INDEX customBaseFieldMappings_customFieldID ON customBaseFieldMappings(customFieldID);

CREATE TABLE translatorCache (
	leafName TEXT PRIMARY KEY,
	translatorJSON TEXT,
	code TEXT,
	lastModifiedTime INT
);

CREATE TABLE zoteroDummyTable (id INTEGER PRIMARY KEY);
