-ifndef(RPBERRORRESP_PB_H).
-define(RPBERRORRESP_PB_H, true).
-record(rpberrorresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).
-endif.

-ifndef(RPBGETSERVERINFORESP_PB_H).
-define(RPBGETSERVERINFORESP_PB_H, true).
-record(rpbgetserverinforesp, {
    node,
    server_version
}).
-endif.

-ifndef(RPBPAIR_PB_H).
-define(RPBPAIR_PB_H, true).
-record(rpbpair, {
    key = erlang:error({required, key}),
    value
}).
-endif.

-ifndef(RPBGETBUCKETREQ_PB_H).
-define(RPBGETBUCKETREQ_PB_H, true).
-record(rpbgetbucketreq, {
    bucket = erlang:error({required, bucket}),
    type
}).
-endif.

-ifndef(RPBGETBUCKETRESP_PB_H).
-define(RPBGETBUCKETRESP_PB_H, true).
-record(rpbgetbucketresp, {
    props = erlang:error({required, props})
}).
-endif.

-ifndef(RPBSETBUCKETREQ_PB_H).
-define(RPBSETBUCKETREQ_PB_H, true).
-record(rpbsetbucketreq, {
    bucket = erlang:error({required, bucket}),
    props = erlang:error({required, props}),
    type
}).
-endif.

-ifndef(RPBRESETBUCKETREQ_PB_H).
-define(RPBRESETBUCKETREQ_PB_H, true).
-record(rpbresetbucketreq, {
    bucket = erlang:error({required, bucket}),
    type
}).
-endif.

-ifndef(RPBGETBUCKETTYPEREQ_PB_H).
-define(RPBGETBUCKETTYPEREQ_PB_H, true).
-record(rpbgetbuckettypereq, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(RPBSETBUCKETTYPEREQ_PB_H).
-define(RPBSETBUCKETTYPEREQ_PB_H, true).
-record(rpbsetbuckettypereq, {
    type = erlang:error({required, type}),
    props = erlang:error({required, props})
}).
-endif.

-ifndef(RPBMODFUN_PB_H).
-define(RPBMODFUN_PB_H, true).
-record(rpbmodfun, {
    module = erlang:error({required, module}),
    function = erlang:error({required, function})
}).
-endif.

-ifndef(RPBCOMMITHOOK_PB_H).
-define(RPBCOMMITHOOK_PB_H, true).
-record(rpbcommithook, {
    modfun,
    name
}).
-endif.

-ifndef(RPBBUCKETPROPS_PB_H).
-define(RPBBUCKETPROPS_PB_H, true).
-record(rpbbucketprops, {
    n_val,
    allow_mult,
    last_write_wins,
    precommit = [],
    has_precommit = false,
    postcommit = [],
    has_postcommit = false,
    chash_keyfun,
    linkfun,
    old_vclock,
    young_vclock,
    big_vclock,
    small_vclock,
    pr,
    r,
    w,
    pw,
    dw,
    rw,
    basic_quorum,
    notfound_ok,
    backend,
    search,
    repl,
    search_index,
    datatype,
    consistent,
    write_once
}).
-endif.

-ifndef(RPBAUTHREQ_PB_H).
-define(RPBAUTHREQ_PB_H, true).
-record(rpbauthreq, {
    user = erlang:error({required, user}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(TSQUERYREQ_PB_H).
-define(TSQUERYREQ_PB_H, true).
-record(tsqueryreq, {
    query,
    stream = false,
    cover_context
}).
-endif.

-ifndef(TSQUERYRESP_PB_H).
-define(TSQUERYRESP_PB_H, true).
-record(tsqueryresp, {
    columns = [],
    rows = [],
    done = true
}).
-endif.

-ifndef(TSGETREQ_PB_H).
-define(TSGETREQ_PB_H, true).
-record(tsgetreq, {
    table = erlang:error({required, table}),
    key = [],
    timeout
}).
-endif.

-ifndef(TSGETRESP_PB_H).
-define(TSGETRESP_PB_H, true).
-record(tsgetresp, {
    columns = [],
    rows = []
}).
-endif.

-ifndef(TSPUTREQ_PB_H).
-define(TSPUTREQ_PB_H, true).
-record(tsputreq, {
    table = erlang:error({required, table}),
    columns = [],
    rows = []
}).
-endif.

-ifndef(TSPUTRESP_PB_H).
-define(TSPUTRESP_PB_H, true).
-record(tsputresp, {
    
}).
-endif.

-ifndef(TSDELREQ_PB_H).
-define(TSDELREQ_PB_H, true).
-record(tsdelreq, {
    table = erlang:error({required, table}),
    key = [],
    vclock,
    timeout
}).
-endif.

-ifndef(TSDELRESP_PB_H).
-define(TSDELRESP_PB_H, true).
-record(tsdelresp, {
    
}).
-endif.

-ifndef(TSINTERPOLATION_PB_H).
-define(TSINTERPOLATION_PB_H, true).
-record(tsinterpolation, {
    base = erlang:error({required, base}),
    interpolations = []
}).
-endif.

-ifndef(TSCOLUMNDESCRIPTION_PB_H).
-define(TSCOLUMNDESCRIPTION_PB_H, true).
-record(tscolumndescription, {
    name = erlang:error({required, name}),
    type = erlang:error({required, type})
}).
-endif.

-ifndef(TSROW_PB_H).
-define(TSROW_PB_H, true).
-record(tsrow, {
    cells = []
}).
-endif.

-ifndef(TSCELL_PB_H).
-define(TSCELL_PB_H, true).
-record(tscell, {
    varchar_value,
    sint64_value,
    timestamp_value,
    boolean_value,
    double_value
}).
-endif.

-ifndef(TSLISTKEYSREQ_PB_H).
-define(TSLISTKEYSREQ_PB_H, true).
-record(tslistkeysreq, {
    table = erlang:error({required, table}),
    timeout
}).
-endif.

-ifndef(TSLISTKEYSRESP_PB_H).
-define(TSLISTKEYSRESP_PB_H, true).
-record(tslistkeysresp, {
    keys = [],
    done
}).
-endif.

-ifndef(TSCOVERAGEREQ_PB_H).
-define(TSCOVERAGEREQ_PB_H, true).
-record(tscoveragereq, {
    query,
    table = erlang:error({required, table}),
    replace_cover,
    unavailable_cover = []
}).
-endif.

-ifndef(TSCOVERAGERESP_PB_H).
-define(TSCOVERAGERESP_PB_H, true).
-record(tscoverageresp, {
    entries = []
}).
-endif.

-ifndef(TSCOVERAGEENTRY_PB_H).
-define(TSCOVERAGEENTRY_PB_H, true).
-record(tscoverageentry, {
    ip = erlang:error({required, ip}),
    port = erlang:error({required, port}),
    cover_context = erlang:error({required, cover_context}),
    range
}).
-endif.

-ifndef(TSRANGE_PB_H).
-define(TSRANGE_PB_H, true).
-record(tsrange, {
    field_name = erlang:error({required, field_name}),
    lower_bound = erlang:error({required, lower_bound}),
    lower_bound_inclusive = erlang:error({required, lower_bound_inclusive}),
    upper_bound = erlang:error({required, upper_bound}),
    upper_bound_inclusive = erlang:error({required, upper_bound_inclusive}),
    desc = erlang:error({required, desc})
}).
-endif.

