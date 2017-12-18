-module(shortener_general_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([successful_redirect/1]).
-export([url_expired/1]).
-export([always_unique_url/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [test_case_name()].
all() ->
    [
        successful_redirect,
        url_expired,
        always_unique_url
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {suppress_supervisor_start_stop, true},
            {handlers, [
                {lager_common_test_backend, [debug, {lager_logstash_formatter, []}]}
            ]}
        ]) ++ genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_lager}
        ]) ++ genlib_app:start_application_with(shortener, [
            {space_size             , 64},
            {hash_algorithm         , sha256},
            {api, #{
                ip                 => "::",
                port               => 8080,
                authorizer         => #{
                    keyset         => #{}
                },
                short_url_template => #{
                    scheme         => http,
                    host           => "url-shortener",
                    path           => "/r/e/d/i/r/"
                }
            }},
            {processor, #{
                ip                 => "::",
                port               => 8022
            }},
            {service_clients, #{
                automaton          => #{url => <<"http://machinegun:8022/v1/automaton">>}
            }}
        ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

%%

-spec successful_redirect(config()) -> _.
-spec url_expired(config()) -> _.
-spec always_unique_url(config()) -> _.

successful_redirect(_C) ->
    ok.

url_expired(_C) ->
    ok.

always_unique_url(_C) ->
    ok.
