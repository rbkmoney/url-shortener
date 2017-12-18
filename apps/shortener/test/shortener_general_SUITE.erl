-module(shortener_general_SUITE).

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([successful_redirect/1]).
-export([successful_delete/1]).
-export([fordidden_source_url/1]).
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
        successful_delete,
        fordidden_source_url,
        url_expired,
        always_unique_url
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({shortener_swagger_server, '_', '_'}, x),
    Host = "url-shortener",
    Port = 8080,
    Netloc = Host ++ ":" ++ genlib:to_list(Port),
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
            {space_size             , 8},
            {hash_algorithm         , sha256},
            {api, #{
                ip                 => "::",
                port               => Port,
                authorizer         => #{
                    signee         => local,
                    keyset => #{
                        local      => {pem_file, get_keysource("keys/local/private.pem", C)}
                    }
                },
                source_url_whitelist => [
                    "https://*",
                    "ftp://*",
                    "http://localhost/*"
                ],
                short_url_template => #{
                    scheme         => http,
                    netloc         => Netloc,
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
    [
        {suite_apps, Apps},
        {api_endpoint, "http://" ++ Netloc}
    ] ++ C.

get_keysource(Key, C) ->
    filename:join(?config(data_dir, C), Key).

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) ->
    UserID = genlib:to_binary(Name),
    {ok, T} = shortener_authorizer_jwt:issue({{UserID, []}, #{}}, unlimited),
    [{api_auth_token, T} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, _C) ->
    ok.

%%

-spec successful_redirect(config()) -> _.
-spec successful_delete(config()) -> _.
-spec fordidden_source_url(config()) -> _.
-spec url_expired(config()) -> _.
-spec always_unique_url(config()) -> _.

successful_redirect(C) ->
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C),
    {ok, 200, _, #{<<"sourceUrl">> := SourceUrl, <<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

successful_delete(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C),
    {ok, 204, _, _} = delete_shortened_url(ID, C),
    {ok, 404, _, _} = get_shortened_url(ID, C),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

fordidden_source_url(C) ->
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"http://localhost/hack?id=42">>), C),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"https://localhost/hack?id=42">>), C),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://example.io/">>), C),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://local.domain/phpmyadmin">>), C),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"ftp://ftp.hp.com/pub/hpcp/newsletter_july2003">>), C).

url_expired(C) ->
    Params = construct_params(<<"https://oops.io/">>, 1),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C),
    {ok, 200, _, #{<<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C),
    ok = timer:sleep(2 * 1000),
    {ok, 404, _, _} = get_shortened_url(ID, C),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

always_unique_url(C) ->
    N = 42,
    Params = construct_params(<<"https://oops.io/">>, 3600),
    {IDs, ShortUrls} = lists:unzip([
        {ID, ShortUrl} ||
            _ <- lists:seq(1, N),
            {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} <- [shorten_url(Params, C)]
    ]),
    N = length(lists:usort(IDs)),
    N = length(lists:usort(ShortUrls)).

construct_params(SourceUrl) ->
    construct_params(SourceUrl, 3600).

construct_params(SourceUrl, Lifetime) ->
    #{
        <<"sourceUrl">> => SourceUrl,
        <<"expiresAt">> => format_ts(genlib_time:unow() + Lifetime)
    }.

%%

shorten_url(ShortenedUrlParams, C) ->
    swag_client_shortener_api:shorten_url(
        ?config(api_endpoint, C),
        append_common_params(#{body => ShortenedUrlParams}, C)
    ).

delete_shortened_url(ID, C) ->
    swag_client_shortener_api:delete_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

get_shortened_url(ID, C) ->
    swag_client_shortener_api:get_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

append_common_params(Params, C) ->
    append_media_type(append_auth(append_request_id(
        maps:merge(#{binding => #{}, qs_val => #{}, header => #{}, body => #{}}, Params)
    ), C)).

append_media_type(Params = #{header := Headers}) ->
    Params#{header => Headers#{
        <<"Accept">>       => <<"application/json">>,
        <<"Content-Type">> => <<"application/json; charset=utf-8">>
    }}.

append_auth(Params = #{header := Headers}, C) ->
    Params#{header => Headers#{<<"Authorization">> => <<"Bearer ", (?config(api_auth_token, C))/binary>>}}.

append_request_id(Params = #{header := Headers}) ->
    Params#{header => Headers#{<<"X-Request-ID">> => woody_context:new_req_id()}}.

format_ts(Ts) ->
    {ok, Result} = rfc3339:format(Ts, seconds),
    Result.
