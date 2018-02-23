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
CREATE TABLE itemDataValues (
    valueID INTEGER PRIMARY KEY,
    value UNIQUE
);
CREATE TABLE users (
    userID INTEGER PRIMARY KEY,
    username TEXT NOT NULL
);
CREATE TABLE fulltextWords (
    wordID INTEGER PRIMARY KEY,
    word TEXT UNIQUE
);
CREATE TABLE proxies (
    proxyID INTEGER PRIMARY KEY,
    multiHost INT,
    autoAssociate INT,
    scheme TEXT
);
CREATE TABLE proxyHosts (
    hostID INTEGER PRIMARY KEY,
    proxyID INTEGER,
    hostname TEXT,
    FOREIGN KEY (proxyID) REFERENCES proxies(proxyID)
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
    PRIMARY KEY (customItemTypeID, orderIndex),
    FOREIGN KEY (customItemTypeID) REFERENCES customItemTypes(customItemTypeID),
    FOREIGN KEY (fieldID) REFERENCES fields(fieldID),
    FOREIGN KEY (customFieldID) REFERENCES customFields(customFieldID)
);
CREATE INDEX customItemTypeFields_fieldID ON customItemTypeFields(fieldID);
CREATE INDEX customItemTypeFields_customFieldID ON customItemTypeFields(customFieldID);
CREATE TRIGGER fki_customItemTypeFields_customFieldID_customFields_customFieldID
  BEFORE INSERT ON customItemTypeFields
  FOR EACH ROW BEGIN
    SELECT RAISE(ABORT, 'insert on table "customItemTypeFields" violates foreign key constraint "fki_customItemTypeFields_customFieldID_customFields_customFieldID"')
    WHERE NEW.customFieldID IS NOT NULL AND (SELECT COUNT(*) FROM customFields WHERE customFieldID = NEW.customFieldID) = 0;
  END;
CREATE TABLE zoteroDummyTable (id INTEGER PRIMARY KEY);
CREATE TABLE itemTypes (    itemTypeID INTEGER PRIMARY KEY,    typeName TEXT,    templateItemTypeID INT,    display INT DEFAULT 1 );
CREATE TABLE itemTypesCombined (    itemTypeID INT NOT NULL,    typeName TEXT NOT NULL,    display INT DEFAULT 1 NOT NULL,    custom INT NOT NULL,    PRIMARY KEY (itemTypeID));
CREATE TABLE fieldFormats (    fieldFormatID INTEGER PRIMARY KEY,    regex TEXT,    isInteger INT);
CREATE TABLE fields (    fieldID INTEGER PRIMARY KEY,    fieldName TEXT,    fieldFormatID INT,    FOREIGN KEY (fieldFormatID) REFERENCES fieldFormats(fieldFormatID));
CREATE TABLE fieldsCombined (    fieldID INT NOT NULL,    fieldName TEXT NOT NULL,    label TEXT,    fieldFormatID INT,    custom INT NOT NULL,    PRIMARY KEY (fieldID));
CREATE TABLE itemTypeFields (    itemTypeID INT,    fieldID INT,    hide INT,    orderIndex INT,    PRIMARY KEY (itemTypeID, orderIndex),    UNIQUE (itemTypeID, fieldID),    FOREIGN KEY (itemTypeID) REFERENCES itemTypes(itemTypeID),    FOREIGN KEY (fieldID) REFERENCES fields(fieldID));
CREATE INDEX itemTypeFields_fieldID ON itemTypeFields(fieldID);
CREATE TABLE itemTypeFieldsCombined (    itemTypeID INT NOT NULL,    fieldID INT NOT NULL,    hide INT,    orderIndex INT NOT NULL,    PRIMARY KEY (itemTypeID, orderIndex),    UNIQUE (itemTypeID, fieldID));
CREATE INDEX itemTypeFieldsCombined_fieldID ON itemTypeFieldsCombined(fieldID);
CREATE TABLE baseFieldMappings (    itemTypeID INT,    baseFieldID INT,    fieldID INT,    PRIMARY KEY (itemTypeID, baseFieldID, fieldID),    FOREIGN KEY (itemTypeID) REFERENCES itemTypes(itemTypeID),    FOREIGN KEY (baseFieldID) REFERENCES fields(fieldID),    FOREIGN KEY (fieldID) REFERENCES fields(fieldID));
CREATE INDEX baseFieldMappings_baseFieldID ON baseFieldMappings(baseFieldID);
CREATE INDEX baseFieldMappings_fieldID ON baseFieldMappings(fieldID);
CREATE TABLE baseFieldMappingsCombined (    itemTypeID INT,    baseFieldID INT,    fieldID INT,    PRIMARY KEY (itemTypeID, baseFieldID, fieldID));
CREATE INDEX baseFieldMappingsCombined_baseFieldID ON baseFieldMappingsCombined(baseFieldID);
CREATE INDEX baseFieldMappingsCombined_fieldID ON baseFieldMappingsCombined(fieldID);
CREATE TABLE charsets (    charsetID INTEGER PRIMARY KEY,    charset TEXT UNIQUE);
CREATE INDEX charsets_charset ON charsets(charset);
CREATE TABLE fileTypes (    fileTypeID INTEGER PRIMARY KEY,    fileType TEXT UNIQUE);
CREATE INDEX fileTypes_fileType ON fileTypes(fileType);
CREATE TABLE fileTypeMimeTypes (    fileTypeID INT,    mimeType TEXT,    PRIMARY KEY (fileTypeID, mimeType),    FOREIGN KEY (fileTypeID) REFERENCES fileTypes(fileTypeID));
CREATE INDEX fileTypeMimeTypes_mimeType ON fileTypeMimeTypes(mimeType);
CREATE TABLE creatorTypes (    creatorTypeID INTEGER PRIMARY KEY,    creatorType TEXT);
CREATE TABLE itemTypeCreatorTypes (    itemTypeID INT,    creatorTypeID INT,    primaryField INT,    PRIMARY KEY (itemTypeID, creatorTypeID),    FOREIGN KEY (itemTypeID) REFERENCES itemTypes(itemTypeID),    FOREIGN KEY (creatorTypeID) REFERENCES creatorTypes(creatorTypeID));
CREATE INDEX itemTypeCreatorTypes_creatorTypeID ON itemTypeCreatorTypes(creatorTypeID);
CREATE TABLE syncObjectTypes (    syncObjectTypeID INTEGER PRIMARY KEY,    name TEXT);
CREATE INDEX syncObjectTypes_name ON syncObjectTypes(name);
CREATE TABLE transactionSets (    transactionSetID INTEGER PRIMARY KEY,    event TEXT,    id INT);
CREATE TABLE transactions (    transactionID INTEGER PRIMARY KEY,    transactionSetID INT,    context TEXT,    action TEXT);
CREATE INDEX transactions_transactionSetID ON transactions(transactionSetID);
CREATE TABLE transactionLog (    transactionID INT,    field TEXT,    value NONE,    PRIMARY KEY (transactionID, field, value),    FOREIGN KEY (transactionID) REFERENCES transactions(transactionID));
CREATE TABLE syncCache (
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    syncObjectTypeID INT NOT NULL,
    version INT NOT NULL,
    data TEXT,
    PRIMARY KEY (libraryID, key, syncObjectTypeID, version),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE,
    FOREIGN KEY (syncObjectTypeID) REFERENCES syncObjectTypes(syncObjectTypeID)
);
CREATE TABLE translatorCache (
    fileName TEXT PRIMARY KEY,
    metadataJSON TEXT,
    lastModifiedTime INT
);
CREATE TABLE collections (
    collectionID INTEGER PRIMARY KEY,
    collectionName TEXT NOT NULL,
    parentCollectionID INT DEFAULT NULL,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    UNIQUE (libraryID, key),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE,
    FOREIGN KEY (parentCollectionID) REFERENCES collections(collectionID) ON DELETE CASCADE
);
CREATE INDEX collections_synced ON collections(synced);
CREATE TABLE items (
    itemID INTEGER PRIMARY KEY,
    itemTypeID INT NOT NULL,
    dateAdded TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    UNIQUE (libraryID, key),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE INDEX items_synced ON items(synced);
CREATE TABLE creators (
    creatorID INTEGER PRIMARY KEY,
    firstName TEXT,
    lastName TEXT,
    fieldMode INT,
    UNIQUE (lastName, firstName, fieldMode)
);
CREATE TABLE itemCreators (
    itemID INT NOT NULL,
    creatorID INT NOT NULL,
    creatorTypeID INT NOT NULL DEFAULT 1,
    orderIndex INT NOT NULL DEFAULT 0,
    PRIMARY KEY (itemID, creatorID, creatorTypeID, orderIndex),
    UNIQUE (itemID, orderIndex),
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (creatorID) REFERENCES creators(creatorID) ON DELETE CASCADE,
    FOREIGN KEY (creatorTypeID) REFERENCES creatorTypes(creatorTypeID)
);
CREATE INDEX itemCreators_creatorTypeID ON itemCreators(creatorTypeID);
CREATE TABLE savedSearches (
    savedSearchID INTEGER PRIMARY KEY,
    savedSearchName TEXT NOT NULL,
    clientDateModified TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    UNIQUE (libraryID, key),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE INDEX savedSearches_synced ON savedSearches(synced);
CREATE TABLE tags (
    tagID INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);
CREATE TABLE itemTags (
    itemID INT NOT NULL,
    tagID INT NOT NULL,
    type INT NOT NULL,
    PRIMARY KEY (itemID, tagID),
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (tagID) REFERENCES tags(tagID) ON DELETE CASCADE
);
CREATE INDEX itemTags_tagID ON itemTags(tagID);
CREATE TABLE syncedSettings (
    setting TEXT NOT NULL,
    libraryID INT NOT NULL,
    value NOT NULL,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    PRIMARY KEY (setting, libraryID),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE TABLE itemData (
    itemID INT,
    fieldID INT,
    valueID,
    PRIMARY KEY (itemID, fieldID),
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (fieldID) REFERENCES fieldsCombined(fieldID),
    FOREIGN KEY (valueID) REFERENCES itemDataValues(valueID)
);
CREATE INDEX itemData_fieldID ON itemData(fieldID);
CREATE TABLE itemNotes (
    itemID INTEGER PRIMARY KEY,
    parentItemID INT,
    note TEXT,
    title TEXT,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (parentItemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE INDEX itemNotes_parentItemID ON itemNotes(parentItemID);
CREATE TABLE itemAttachments (
    itemID INTEGER PRIMARY KEY,
    parentItemID INT,
    linkMode INT,
    contentType TEXT,
    charsetID INT,
    path TEXT,
    syncState INT DEFAULT 0,
    storageModTime INT,
    storageHash TEXT,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (parentItemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (charsetID) REFERENCES charsets(charsetID) ON DELETE SET NULL
);
CREATE INDEX itemAttachments_parentItemID ON itemAttachments(parentItemID);
CREATE INDEX itemAttachments_charsetID ON itemAttachments(charsetID);
CREATE INDEX itemAttachments_contentType ON itemAttachments(contentType);
CREATE INDEX itemAttachments_syncState ON itemAttachments(syncState);
CREATE TABLE collectionItems (
    collectionID INT NOT NULL,
    itemID INT NOT NULL,
    orderIndex INT NOT NULL DEFAULT 0,
    PRIMARY KEY (collectionID, itemID),
    FOREIGN KEY (collectionID) REFERENCES collections(collectionID) ON DELETE CASCADE,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE INDEX collectionItems_itemID ON collectionItems(itemID);
CREATE TABLE savedSearchConditions (
    savedSearchID INT NOT NULL,
    searchConditionID INT NOT NULL,
    condition TEXT NOT NULL,
    operator TEXT,
    value TEXT,
    required NONE,
    PRIMARY KEY (savedSearchID, searchConditionID),
    FOREIGN KEY (savedSearchID) REFERENCES savedSearches(savedSearchID) ON DELETE CASCADE
);
CREATE TABLE deletedItems (
    itemID INTEGER PRIMARY KEY,
    dateDeleted DEFAULT CURRENT_TIMESTAMP NOT NULL,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE INDEX deletedItems_dateDeleted ON deletedItems(dateDeleted);
CREATE TABLE relationPredicates (
    predicateID INTEGER PRIMARY KEY,
    predicate TEXT UNIQUE
);
CREATE TABLE collectionRelations (
    collectionID INT NOT NULL,
    predicateID INT NOT NULL,
    object TEXT NOT NULL,
    PRIMARY KEY (collectionID, predicateID, object),
    FOREIGN KEY (collectionID) REFERENCES collections(collectionID) ON DELETE CASCADE,
    FOREIGN KEY (predicateID) REFERENCES relationPredicates(predicateID) ON DELETE CASCADE
);
CREATE INDEX collectionRelations_predicateID ON collectionRelations(predicateID);
CREATE INDEX collectionRelations_object ON collectionRelations(object);
CREATE TABLE itemRelations (
    itemID INT NOT NULL,
    predicateID INT NOT NULL,
    object TEXT NOT NULL,
    PRIMARY KEY (itemID, predicateID, object),
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (predicateID) REFERENCES relationPredicates(predicateID) ON DELETE CASCADE
);
CREATE INDEX itemRelations_predicateID ON itemRelations(predicateID);
CREATE INDEX itemRelations_object ON itemRelations(object);
CREATE TABLE groups (
    groupID INTEGER PRIMARY KEY,
    libraryID INT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    version INT NOT NULL,
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE TABLE groupItems (
    itemID INTEGER PRIMARY KEY,
    createdByUserID INT,
    lastModifiedByUserID INT,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE,
    FOREIGN KEY (createdByUserID) REFERENCES users(userID) ON DELETE SET NULL,
    FOREIGN KEY (lastModifiedByUserID) REFERENCES users(userID) ON DELETE SET NULL
);
CREATE TABLE fulltextItems (
    itemID INTEGER PRIMARY KEY,
    indexedPages INT,
    totalPages INT,
    indexedChars INT,
    totalChars INT,
    version INT NOT NULL DEFAULT 0,
    synced INT NOT NULL DEFAULT 0,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE INDEX fulltextItems_synced ON fulltextItems(synced);
CREATE INDEX fulltextItems_version ON fulltextItems(version);
CREATE TABLE fulltextItemWords (
    wordID INT,
    itemID INT,
    PRIMARY KEY (wordID, itemID),
    FOREIGN KEY (wordID) REFERENCES fulltextWords(wordID),
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE INDEX fulltextItemWords_itemID ON fulltextItemWords(itemID);
CREATE TABLE syncDeleteLog (
    syncObjectTypeID INT NOT NULL,
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    dateDeleted TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (syncObjectTypeID, libraryID, key),
    FOREIGN KEY (syncObjectTypeID) REFERENCES syncObjectTypes(syncObjectTypeID),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE TABLE storageDeleteLog (
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    dateDeleted TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (libraryID, key),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE TABLE annotations (
    annotationID INTEGER PRIMARY KEY,
    itemID INT NOT NULL,
    parent TEXT,
    textNode INT,
    offset INT,
    x INT,
    y INT,
    cols INT,
    rows INT,
    text TEXT,
    collapsed BOOL,
    dateModified DATE,
    FOREIGN KEY (itemID) REFERENCES itemAttachments(itemID) ON DELETE CASCADE
);
CREATE INDEX annotations_itemID ON annotations(itemID);
CREATE TABLE highlights (
    highlightID INTEGER PRIMARY KEY,
    itemID INT NOT NULL,
    startParent TEXT,
    startTextNode INT,
    startOffset INT,
    endParent TEXT,
    endTextNode INT,
    endOffset INT,
    dateModified DATE,
    FOREIGN KEY (itemID) REFERENCES itemAttachments(itemID) ON DELETE CASCADE
);
CREATE INDEX highlights_itemID ON highlights(itemID);
CREATE TABLE customBaseFieldMappings (
    customItemTypeID INT,
    baseFieldID INT,
    customFieldID INT,
    PRIMARY KEY (customItemTypeID, baseFieldID, customFieldID),
    FOREIGN KEY (customItemTypeID) REFERENCES customItemTypes(customItemTypeID),
    FOREIGN KEY (baseFieldID) REFERENCES fields(fieldID),
    FOREIGN KEY (customFieldID) REFERENCES customFields(customFieldID)
);
CREATE INDEX customBaseFieldMappings_baseFieldID ON customBaseFieldMappings(baseFieldID);
CREATE INDEX customBaseFieldMappings_customFieldID ON customBaseFieldMappings(customFieldID);
CREATE TABLE libraries (
    libraryID INTEGER PRIMARY KEY,
    type TEXT NOT NULL,
    editable INT NOT NULL,
    filesEditable INT NOT NULL,
    version INT NOT NULL DEFAULT 0,
    storageVersion INT NOT NULL DEFAULT 0,
    lastSync INT NOT NULL DEFAULT 0
, archived INT NOT NULL DEFAULT 0);
CREATE TABLE feedItems (
    itemID INTEGER PRIMARY KEY,
    guid TEXT NOT NULL UNIQUE,
    readTime TIMESTAMP,
    translatedTime TIMESTAMP,
    FOREIGN KEY (itemID) REFERENCES items(itemID) ON DELETE CASCADE
);
CREATE TABLE syncQueue (
    libraryID INT NOT NULL,
    key TEXT NOT NULL,
    syncObjectTypeID INT NOT NULL,
    lastCheck TIMESTAMP,
    tries INT,
    PRIMARY KEY (libraryID, key, syncObjectTypeID),
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE,
    FOREIGN KEY (syncObjectTypeID) REFERENCES syncObjectTypes(syncObjectTypeID) ON DELETE CASCADE
);
CREATE TABLE feeds (
    libraryID INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    url TEXT NOT NULL UNIQUE,
    lastUpdate TIMESTAMP,
    lastCheck TIMESTAMP,
    lastCheckError TEXT,
    cleanupReadAfter INT,
    cleanupUnreadAfter INT,
    refreshInterval INT,
    FOREIGN KEY (libraryID) REFERENCES libraries(libraryID) ON DELETE CASCADE
);
CREATE TABLE publicationsItems (
    itemID INTEGER PRIMARY KEY
);
CREATE TRIGGER insert_date_field BEFORE INSERT ON itemData  FOR EACH ROW WHEN NEW.fieldID IN (14, 27, 52, 96, 100)  BEGIN    SELECT CASE        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 1, 4) AS INT) BETWEEN 0 AND 9999 AND        SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 5, 1) = '-' AND        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 6, 2) AS INT) BETWEEN 0 AND 12 AND        SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 8, 1) = '-' AND        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 9, 2) AS INT) BETWEEN 0 AND 31      WHEN 0 THEN RAISE (ABORT, 'Date field must begin with SQL date') END;  END;
CREATE TRIGGER update_date_field BEFORE UPDATE ON itemData  FOR EACH ROW WHEN NEW.fieldID IN (14, 27, 52, 96, 100)  BEGIN    SELECT CASE        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 1, 4) AS INT) BETWEEN 0 AND 9999 AND        SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 5, 1) = '-' AND        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 6, 2) AS INT) BETWEEN 0 AND 12 AND        SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 8, 1) = '-' AND        CAST(SUBSTR((SELECT value FROM itemDataValues WHERE valueID=NEW.valueID), 9, 2) AS INT) BETWEEN 0 AND 31      WHEN 0 THEN RAISE (ABORT, 'Date field must begin with SQL date') END;  END;
CREATE TRIGGER insert_creators BEFORE INSERT ON creators  FOR EACH ROW WHEN NEW.firstName='' AND NEW.lastName=''  BEGIN    SELECT RAISE (ABORT, 'Creator names cannot be empty');  END;
CREATE TRIGGER update_creators BEFORE UPDATE ON creators  FOR EACH ROW WHEN NEW.firstName='' AND NEW.lastName=''  BEGIN    SELECT RAISE (ABORT, 'Creator names cannot be empty');  END;
CREATE TRIGGER fki_collections_parentCollectionID_libraryID  BEFORE INSERT ON collections  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'insert on table "collections" violates foreign key constraint "fki_collections_parentCollectionID_libraryID"')    WHERE NEW.parentCollectionID IS NOT NULL AND    NEW.libraryID != (SELECT libraryID FROM collections WHERE collectionID = NEW.parentCollectionID);  END;
CREATE TRIGGER fku_collections_parentCollectionID_libraryID  BEFORE UPDATE ON collections  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'update on table "collections" violates foreign key constraint "fku_collections_parentCollectionID_libraryID"')    WHERE NEW.parentCollectionID IS NOT NULL AND    NEW.libraryID != (SELECT libraryID FROM collections WHERE collectionID = NEW.parentCollectionID);  END;
CREATE TRIGGER fki_collectionItems_libraryID  BEFORE INSERT ON collectionItems  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'insert on table "collectionItems" violates foreign key constraint "fki_collectionItems_libraryID"')    WHERE (SELECT libraryID FROM collections WHERE collectionID = NEW.collectionID) != (SELECT libraryID FROM items WHERE itemID = NEW.itemID);  END;
CREATE TRIGGER fku_collectionItems_libraryID  BEFORE UPDATE ON collectionItems  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'update on table "collectionItems" violates foreign key constraint "fku_collectionItems_libraryID"')    WHERE (SELECT libraryID FROM collections WHERE collectionID = NEW.collectionID) != (SELECT libraryID FROM items WHERE itemID = NEW.itemID);  END;
CREATE TRIGGER fki_collectionItems_itemID_parentItemID  BEFORE INSERT ON collectionItems  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'insert on table "collectionItems" violates foreign key constraint "fki_collectionItems_itemID_parentItemID"')    WHERE NEW.itemID IN (SELECT itemID FROM itemAttachments WHERE parentItemID IS NOT NULL UNION SELECT itemID FROM itemNotes WHERE parentItemID IS NOT NULL);  END;
CREATE TRIGGER fku_collectionItems_itemID_parentItemID  BEFORE UPDATE OF itemID ON collectionItems  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'update on table "collectionItems" violates foreign key constraint "fku_collectionItems_itemID_parentItemID"')    WHERE NEW.itemID IN (SELECT itemID FROM itemAttachments WHERE parentItemID IS NOT NULL UNION SELECT itemID FROM itemNotes WHERE parentItemID IS NOT NULL);  END;
CREATE TRIGGER fku_itemAttachments_parentItemID_collectionItems_itemID  BEFORE UPDATE OF parentItemID ON itemAttachments  FOR EACH ROW WHEN OLD.parentItemID IS NULL AND NEW.parentItemID IS NOT NULL BEGIN    DELETE FROM collectionItems WHERE itemID = NEW.itemID;  END;
CREATE TRIGGER fku_itemNotes_parentItemID_collectionItems_itemID  BEFORE UPDATE OF parentItemID ON itemNotes  FOR EACH ROW WHEN OLD.parentItemID IS NULL AND NEW.parentItemID IS NOT NULL BEGIN    DELETE FROM collectionItems WHERE itemID = NEW.itemID;  END;
CREATE TRIGGER fki_itemAttachments  BEFORE INSERT ON itemAttachments  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'insert on table "itemAttachments" violates foreign key constraint "fki_itemAttachments"')    WHERE NEW.parentItemID IS NOT NULL AND    (SELECT libraryID FROM items WHERE itemID = NEW.itemID) != (SELECT libraryID FROM items WHERE itemID = NEW.parentItemID);            SELECT RAISE(ABORT, 'item is not an attachment')    WHERE (SELECT itemTypeID FROM items WHERE itemID = NEW.itemID) != 14;            SELECT RAISE(ABORT, 'parent is not a regular item')    WHERE NEW.parentItemID IS NOT NULL AND (SELECT itemTypeID FROM items WHERE itemID = NEW.parentItemID) IN (1,14);            SELECT RAISE(ABORT, 'collection item must be top level')    WHERE NEW.parentItemID IS NOT NULL AND (SELECT COUNT(*) FROM collectionItems WHERE itemID=NEW.itemID)>0;  END;
CREATE TRIGGER fku_itemAttachments  BEFORE UPDATE ON itemAttachments  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'update on table "itemAttachments" violates foreign key constraint "fku_itemAttachments"')    WHERE NEW.parentItemID IS NOT NULL AND    (SELECT libraryID FROM items WHERE itemID = NEW.itemID) != (SELECT libraryID FROM items WHERE itemID = NEW.parentItemID);            SELECT RAISE(ABORT, 'parent is not a regular item')    WHERE NEW.parentItemID IS NOT NULL AND (SELECT itemTypeID FROM items WHERE itemID = NEW.parentItemID) IN (1,14);  END;
CREATE TRIGGER fki_itemNotes  BEFORE INSERT ON itemNotes  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'insert on table "itemNotes" violates foreign key constraint "fki_itemNotes_libraryID"')    WHERE NEW.parentItemID IS NOT NULL AND    (SELECT libraryID FROM items WHERE itemID = NEW.itemID) != (SELECT libraryID FROM items WHERE itemID = NEW.parentItemID);            SELECT RAISE(ABORT, 'item is not a note or attachment') WHERE    (SELECT itemTypeID FROM items WHERE itemID = NEW.itemID) NOT IN (1,14);            SELECT RAISE(ABORT, 'parent is not a regular item') WHERE    NEW.parentItemID IS NOT NULL AND (SELECT itemTypeID FROM items WHERE itemID = NEW.parentItemID) IN (1,14);            SELECT RAISE(ABORT, 'collection item must be top level') WHERE    NEW.parentItemID IS NOT NULL AND (SELECT COUNT(*) FROM collectionItems WHERE itemID=NEW.itemID)>0;  END;
CREATE TRIGGER fku_itemNotes  BEFORE UPDATE ON itemNotes  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'update on table "itemNotes" violates foreign key constraint "fku_itemNotes"')    WHERE NEW.parentItemID IS NOT NULL AND    (SELECT libraryID FROM items WHERE itemID = NEW.itemID) != (SELECT libraryID FROM items WHERE itemID = NEW.parentItemID);            SELECT RAISE(ABORT, 'parent is not a regular item') WHERE    NEW.parentItemID IS NOT NULL AND (SELECT itemTypeID FROM items WHERE itemID = NEW.parentItemID) IN (1,14);  END;
CREATE TRIGGER fki_tagsBEFORE INSERT ON tags  FOR EACH ROW BEGIN    SELECT RAISE(ABORT, 'Tag cannot be blank')    WHERE TRIM(NEW.name)='';  END;
CREATE TRIGGER fku_tags  BEFORE UPDATE OF name ON tags  FOR EACH ROW BEGIN      SELECT RAISE(ABORT, 'Tag cannot be blank')      WHERE TRIM(NEW.name)='';  END;
