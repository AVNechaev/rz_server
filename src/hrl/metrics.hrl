-ifndef(METRICS).
-define(METRICS, true).

-define(METRIC_CREATE_BOOL(Name, Help, Opts), prometheus_boolean:new([{name, Name}, {help, Help} | Opts])).
-define(METRIC_CREATE_BOOL(Name, Help), ?METRIC_CREATE_BOOL(Name, Halp, [])).
-define(METRIC_CREATE_GAUGE(Name, Help, Opts), prometheus_gauge:new([{name, Name}, {help, Help} | Opts])).
-define(METRIC_CREATE_GAUGE(Name, Help), ?METRIC_CREATE_GAUGE(Name, Help, [])).
-define(METRIC_CREATE_COUNTER(Name, Help, Opts), prometheus_counter:new([{name, Name}, {help, Help} | Opts])).
-define(METRIC_CREATE_COUNTER(Name, Help), ?METRIC_CREATE_COUNTER(Name, Help, [])).
-define(METRIC_CREATE_HISTO(Name, Help, Buckets, Opts), prometheus_histogram:new([{name, Name}, {help, Help}, {buckets, Buckets} | Opts])).
-define(METRIC_CREATE_HISTO(Name, Help, Buckets), ?METRIC_CREATE_HISTO(Name, Help, Buckets, [])).

-define(METRIC_SET_BOOL(Name, Val, Labels), prometheus_boolean:set(Name, Labels, Val)).
-define(METRIC_SET_BOOL(Name, Val), ?METRIC_SET_BOOL(Name, Val, [])).
-define(METRIC_SET_GAUGE(Name, Val, Labels), prometheus_gauge:set(Name, Labels, Val)).
-define(METRIC_SET_GAUGE(Name, Val), ?METRIC_SET_GAUGE(Name, Val, [])).
-define(METRIC_ADD_COUNTER(Name, Val, Labels), prometheus_counter:inc(Name, Labels, Val)).
-define(METRIC_ADD_COUNTER(Name, Val), ?METRIC_ADD_COUNTER(Name, Val, [])).
-define(METRIC_SET_HISTO(Name, Val, Labels), prometheus_histogram:observe(Name, Labels, Val)).
-define(METRIC_SET_HISTO(Name, Val), ?METRIC_SET_HISTO(Name, Val, [])).

-endif.
