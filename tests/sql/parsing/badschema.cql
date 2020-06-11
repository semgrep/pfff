CREATE TABLE database.table (
    key text,
    clustering_key text,
    value text,
    PRIMARY KEY (key, clustering_key)
) WITH CLUSTERING ORDER BY (column1 ASC)
    AND bloom_filter_fp_chance = 0.1
    AND caching = {'keys': 'ALL', 'rows_per_partition': 'NONE'}
    AND comment = 'Owner: josephl@netflix.com | Table for storing stuff'
    AND compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
    AND compression = {'class': 'org.apache.cassandra.db.compaction.LZ4Compressor'}
    AND crc_check_chance = 1.0
    AND dclocal_read_repair_chance = 0.1
    AND default_time_to_live = 0
    AND gc_grace_seconds = 864000
    AND max_index_interval = 2048
    AND memtable_flush_period_in_ms = 0
    AND min_index_interval = 128
    AND read_repair_chance = 0.0
    AND speculative_retry = 'NONE';
