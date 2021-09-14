// Copyright 2019 JanusGraph Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

CREATE TABLE IF NOT EXISTS chopaan_test.txlog (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.edgestore (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.edgestore_lock_ (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.systemlog (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.janusgraph_ids (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.graphindex (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.graphindex_lock_ (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.system_properties (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};

CREATE TABLE IF NOT EXISTS chopaan_test.system_properties_lock_ (
    key blob,
    column1 blob,
    value blob, PRIMARY KEY (key, column1)    
) WITH CLUSTERING ORDER BY (column1 DESC)
    AND compaction={'class':'LeveledCompactionStrategy'}
    AND compression={'class':'SnappyCompressor'};