%% @doc This module contains message code mappings generated from
%% src/riak_pb_messages.csv. DO NOT EDIT OR COMMIT THIS FILE!
-module(riak_pb_messages).

-export([msg_type/1, msg_code/1, decoder_for/1]).

-spec msg_type(non_neg_integer()) -> atom().

msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbpingreq;
msg_type(2) -> rpbpingresp;
msg_type(3) -> rpbgetclientidreq;
msg_type(4) -> rpbgetclientidresp;
msg_type(5) -> rpbsetclientidreq;
msg_type(6) -> rpbsetclientidresp;
msg_type(7) -> rpbgetserverinforeq;
msg_type(8) -> rpbgetserverinforesp;
msg_type(9) -> rpbgetreq;
msg_type(10) -> rpbgetresp;
msg_type(11) -> rpbputreq;
msg_type(12) -> rpbputresp;
msg_type(13) -> rpbdelreq;
msg_type(14) -> rpbdelresp;
msg_type(15) -> rpblistbucketsreq;
msg_type(16) -> rpblistbucketsresp;
msg_type(17) -> rpblistkeysreq;
msg_type(18) -> rpblistkeysresp;
msg_type(19) -> rpbgetbucketreq;
msg_type(20) -> rpbgetbucketresp;
msg_type(21) -> rpbsetbucketreq;
msg_type(22) -> rpbsetbucketresp;
msg_type(23) -> rpbmapredreq;
msg_type(24) -> rpbmapredresp;
msg_type(25) -> rpbindexreq;
msg_type(26) -> rpbindexresp;
msg_type(27) -> rpbsearchqueryreq;
msg_type(28) -> rpbsearchqueryresp;
msg_type(29) -> rpbresetbucketreq;
msg_type(30) -> rpbresetbucketresp;
msg_type(31) -> rpbgetbuckettypereq;
msg_type(32) -> rpbsetbuckettypereq;
msg_type(33) -> rpbgetbucketkeypreflistreq;
msg_type(34) -> rpbgetbucketkeypreflistresp;
msg_type(40) -> rpbcsbucketreq;
msg_type(41) -> rpbcsbucketresp;
msg_type(42) -> rpbindexbodyresp;
msg_type(50) -> rpbcounterupdatereq;
msg_type(51) -> rpbcounterupdateresp;
msg_type(52) -> rpbcountergetreq;
msg_type(53) -> rpbcountergetresp;
msg_type(54) -> rpbyokozunaindexgetreq;
msg_type(55) -> rpbyokozunaindexgetresp;
msg_type(56) -> rpbyokozunaindexputreq;
msg_type(57) -> rpbyokozunaindexdeletereq;
msg_type(58) -> rpbyokozunaschemagetreq;
msg_type(59) -> rpbyokozunaschemagetresp;
msg_type(60) -> rpbyokozunaschemaputreq;
msg_type(70) -> rpbcoveragereq;
msg_type(71) -> rpbcoverageresp;
msg_type(80) -> dtfetchreq;
msg_type(81) -> dtfetchresp;
msg_type(82) -> dtupdatereq;
msg_type(83) -> dtupdateresp;
msg_type(90) -> tsqueryreq;
msg_type(91) -> tsqueryresp;
msg_type(92) -> tsputreq;
msg_type(93) -> tsputresp;
msg_type(94) -> tsdelreq;
msg_type(95) -> tsdelresp;
msg_type(96) -> tsgetreq;
msg_type(97) -> tsgetresp;
msg_type(98) -> tslistkeysreq;
msg_type(99) -> tslistkeysresp;
msg_type(100) -> tscoveragereq;
msg_type(101) -> tscoverageresp;
msg_type(102) -> tscoverageentry;
msg_type(103) -> tsrange;
msg_type(104) -> tsttbmsg;
msg_type(253) -> rpbauthreq;
msg_type(254) -> rpbauthresp;
msg_type(255) -> rpbstarttls;
msg_type(_) -> undefined.

-spec msg_code(atom()) -> non_neg_integer().

msg_code(rpberrorresp) -> 0;
msg_code(rpbpingreq) -> 1;
msg_code(rpbpingresp) -> 2;
msg_code(rpbgetclientidreq) -> 3;
msg_code(rpbgetclientidresp) -> 4;
msg_code(rpbsetclientidreq) -> 5;
msg_code(rpbsetclientidresp) -> 6;
msg_code(rpbgetserverinforeq) -> 7;
msg_code(rpbgetserverinforesp) -> 8;
msg_code(rpbgetreq) -> 9;
msg_code(rpbgetresp) -> 10;
msg_code(rpbputreq) -> 11;
msg_code(rpbputresp) -> 12;
msg_code(rpbdelreq) -> 13;
msg_code(rpbdelresp) -> 14;
msg_code(rpblistbucketsreq) -> 15;
msg_code(rpblistbucketsresp) -> 16;
msg_code(rpblistkeysreq) -> 17;
msg_code(rpblistkeysresp) -> 18;
msg_code(rpbgetbucketreq) -> 19;
msg_code(rpbgetbucketresp) -> 20;
msg_code(rpbsetbucketreq) -> 21;
msg_code(rpbsetbucketresp) -> 22;
msg_code(rpbmapredreq) -> 23;
msg_code(rpbmapredresp) -> 24;
msg_code(rpbindexreq) -> 25;
msg_code(rpbindexresp) -> 26;
msg_code(rpbsearchqueryreq) -> 27;
msg_code(rpbsearchqueryresp) -> 28;
msg_code(rpbresetbucketreq) -> 29;
msg_code(rpbresetbucketresp) -> 30;
msg_code(rpbgetbuckettypereq) -> 31;
msg_code(rpbsetbuckettypereq) -> 32;
msg_code(rpbgetbucketkeypreflistreq) -> 33;
msg_code(rpbgetbucketkeypreflistresp) -> 34;
msg_code(rpbcsbucketreq) -> 40;
msg_code(rpbcsbucketresp) -> 41;
msg_code(rpbindexbodyresp) -> 42;
msg_code(rpbcounterupdatereq) -> 50;
msg_code(rpbcounterupdateresp) -> 51;
msg_code(rpbcountergetreq) -> 52;
msg_code(rpbcountergetresp) -> 53;
msg_code(rpbyokozunaindexgetreq) -> 54;
msg_code(rpbyokozunaindexgetresp) -> 55;
msg_code(rpbyokozunaindexputreq) -> 56;
msg_code(rpbyokozunaindexdeletereq) -> 57;
msg_code(rpbyokozunaschemagetreq) -> 58;
msg_code(rpbyokozunaschemagetresp) -> 59;
msg_code(rpbyokozunaschemaputreq) -> 60;
msg_code(rpbcoveragereq) -> 70;
msg_code(rpbcoverageresp) -> 71;
msg_code(dtfetchreq) -> 80;
msg_code(dtfetchresp) -> 81;
msg_code(dtupdatereq) -> 82;
msg_code(dtupdateresp) -> 83;
msg_code(tsqueryreq) -> 90;
msg_code(tsqueryresp) -> 91;
msg_code(tsputreq) -> 92;
msg_code(tsputresp) -> 93;
msg_code(tsdelreq) -> 94;
msg_code(tsdelresp) -> 95;
msg_code(tsgetreq) -> 96;
msg_code(tsgetresp) -> 97;
msg_code(tslistkeysreq) -> 98;
msg_code(tslistkeysresp) -> 99;
msg_code(tscoveragereq) -> 100;
msg_code(tscoverageresp) -> 101;
msg_code(tscoverageentry) -> 102;
msg_code(tsrange) -> 103;
msg_code(tsttbmsg) -> 104;
msg_code(rpbauthreq) -> 253;
msg_code(rpbauthresp) -> 254;
msg_code(rpbstarttls) -> 255.

-spec decoder_for(non_neg_integer()) -> module().


decoder_for(0) -> riak_pb;
decoder_for(1) -> riak_pb;
decoder_for(2) -> riak_pb;
decoder_for(3) -> riak_kv_pb;
decoder_for(4) -> riak_kv_pb;
decoder_for(5) -> riak_kv_pb;
decoder_for(6) -> riak_kv_pb;
decoder_for(7) -> riak_pb;
decoder_for(8) -> riak_pb;
decoder_for(9) -> riak_kv_pb;
decoder_for(10) -> riak_kv_pb;
decoder_for(11) -> riak_kv_pb;
decoder_for(12) -> riak_kv_pb;
decoder_for(13) -> riak_kv_pb;
decoder_for(14) -> riak_kv_pb;
decoder_for(15) -> riak_kv_pb;
decoder_for(16) -> riak_kv_pb;
decoder_for(17) -> riak_kv_pb;
decoder_for(18) -> riak_kv_pb;
decoder_for(19) -> riak_pb;
decoder_for(20) -> riak_pb;
decoder_for(21) -> riak_pb;
decoder_for(22) -> riak_pb;
decoder_for(23) -> riak_kv_pb;
decoder_for(24) -> riak_kv_pb;
decoder_for(25) -> riak_kv_pb;
decoder_for(26) -> riak_kv_pb;
decoder_for(27) -> riak_search_pb;
decoder_for(28) -> riak_search_pb;
decoder_for(29) -> riak_pb;
decoder_for(30) -> riak_pb;
decoder_for(31) -> riak_pb;
decoder_for(32) -> riak_pb;
decoder_for(33) -> riak_kv_pb;
decoder_for(34) -> riak_kv_pb;
decoder_for(40) -> riak_kv_pb;
decoder_for(41) -> riak_kv_pb;
decoder_for(42) -> riak_kv_pb;
decoder_for(50) -> riak_kv_pb;
decoder_for(51) -> riak_kv_pb;
decoder_for(52) -> riak_kv_pb;
decoder_for(53) -> riak_kv_pb;
decoder_for(54) -> riak_yokozuna_pb;
decoder_for(55) -> riak_yokozuna_pb;
decoder_for(56) -> riak_yokozuna_pb;
decoder_for(57) -> riak_yokozuna_pb;
decoder_for(58) -> riak_yokozuna_pb;
decoder_for(59) -> riak_yokozuna_pb;
decoder_for(60) -> riak_yokozuna_pb;
decoder_for(70) -> riak_kv_pb;
decoder_for(71) -> riak_kv_pb;
decoder_for(80) -> riak_dt_pb;
decoder_for(81) -> riak_dt_pb;
decoder_for(82) -> riak_dt_pb;
decoder_for(83) -> riak_dt_pb;
decoder_for(90) -> riak_ts_pb;
decoder_for(91) -> riak_ts_pb;
decoder_for(92) -> riak_ts_pb;
decoder_for(93) -> riak_ts_pb;
decoder_for(94) -> riak_ts_pb;
decoder_for(95) -> riak_ts_pb;
decoder_for(96) -> riak_ts_pb;
decoder_for(97) -> riak_ts_pb;
decoder_for(98) -> riak_ts_pb;
decoder_for(99) -> riak_ts_pb;
decoder_for(100) -> riak_ts_pb;
decoder_for(101) -> riak_ts_pb;
decoder_for(102) -> riak_ts_pb;
decoder_for(103) -> riak_ts_pb;
decoder_for(104) -> riak_ts_pb;
decoder_for(253) -> riak_pb;
decoder_for(254) -> riak_pb;
decoder_for(255) -> riak_pb.