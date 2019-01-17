-module(shortener_general_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([failed_authorization/1]).
-export([insufficient_permissions/1]).
-export([readonly_permissions/1]).
-export([successful_redirect/1]).
-export([successful_delete/1]).
-export([other_subject_delete/1]).
-export([other_subject_read/1]).
-export([fordidden_source_url/1]).
-export([url_expired/1]).
-export([always_unique_url/1]).

-export([woody_timeout_test/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [test_case_name()].
all() ->
    [
        {group, general},

        woody_timeout_test
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].

groups() ->
    [
        {general, [], [
            failed_authorization,
            insufficient_permissions,
            readonly_permissions,

            successful_redirect,
            successful_delete,
            other_subject_delete,
            other_subject_read,
            fordidden_source_url,
            url_expired,
            always_unique_url
        ]}
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
                {lager_common_test_backend, [warning, {lager_logstash_formatter, []}]}
            ]}
        ]) ++ genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_lager}
        ]),
    [
        {suite_apps, Apps},
        {api_endpoint, "http://" ++ Netloc},
        {host, Host},
        {port, Port},
        {netloc, Netloc}
    ] ++ C.

-spec init_per_group(atom(), config()) -> config().

init_per_group(general, C) ->
    ShortenerApp =
        genlib_app:start_application_with(shortener, get_app_config(
            ?config(port, C),
            ?config(netloc, C),
            get_keysource("keys/local/private.pem", C),
            <<"http://machinegun:8022/v1/automaton">>
        )),
    [
        {shortener_app, ShortenerApp}
    ] ++ C.

-spec end_per_group(atom(), config()) -> _.

end_per_group(general, C) ->
    genlib_app:stop_unload_applications(?config(shortener_app, C)).

get_keysource(Key, C) ->
    filename:join(?config(data_dir, C), Key).

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) when Name =/= woody_timeout_test ->
    set_api_auth_token(Name, [
        {['shortened-urls'], read},
        {['shortened-urls'], write}
    ], C);
init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, _C) ->
    ok.

%%

-spec failed_authorization(config()) -> _.
-spec insufficient_permissions(config()) -> _.
-spec readonly_permissions(config()) -> _.

-spec successful_redirect(config()) -> _.
-spec successful_delete(config()) -> _.
-spec other_subject_delete(config()) -> _.
-spec other_subject_read(config()) -> _.
-spec fordidden_source_url(config()) -> _.
-spec url_expired(config()) -> _.
-spec always_unique_url(config()) -> _.

failed_authorization(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    C1 = clean_api_auth_token(C),
    {ok, 401, _, _} = shorten_url(Params, C1),
    {ok, 401, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 401, _, _} = get_shortened_url(<<"42">>, C1).

insufficient_permissions(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    C1 = set_api_auth_token(insufficient_permissions, [], C),
    {ok, 403, _, _} = shorten_url(Params, C1),
    {ok, 403, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 403, _, _} = get_shortened_url(<<"42">>, C1).

readonly_permissions(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C),
    C1 = set_api_auth_token(readonly_permissions, [{['shortened-urls'], read}], C),
    {ok, 200, _, #{<<"id">> := ID}} = get_shortened_url(ID, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C1).

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

other_subject_delete(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    Acl = [{['shortened-urls'], read}, {['shortened-urls'], write}],
    C1 = set_api_auth_token(other_subject_delete_first, Acl, C),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_delete_second, Acl, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C2),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

other_subject_read(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    Acl = [{['shortened-urls'], read}, {['shortened-urls'], write}],
    C1 = set_api_auth_token(other_subject_read_first, Acl, C),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_read_second, Acl, C1),
    {ok, 403, _, _} = get_shortened_url(ID, C2).

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
-spec woody_timeout_test(config()) -> _.

woody_timeout_test(C) ->
    Apps = genlib_app:start_application_with(shortener, get_app_config(
        ?config(port, C),
        ?config(netloc, C),
        get_keysource("keys/local/private.pem", C),
        <<"http://invalid_url:8022/v1/automaton">>
    )),
    C2 = set_api_auth_token(woody_timeout_test, [
        {['shortened-urls'], read},
        {['shortened-urls'], write}
    ], C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {Time, {error, {invalid_response_code, 503}}} =
        timer:tc(fun() ->
            shorten_url(Params, C2)
        end),
    true = (Time >= 3000000),
    genlib_app:stop_unload_applications(Apps).

%%

set_api_auth_token(Name, ACL, C) ->
    UserID = genlib:to_binary(Name),
    {ok, T} = shortener_authorizer_jwt:issue({{UserID, shortener_acl:from_list(ACL)}, #{}}, unlimited),
    lists:keystore(api_auth_token, 1, C, {api_auth_token, T}).

clean_api_auth_token(C) ->
    lists:keydelete(api_auth_token, 1, C).

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
    case lists:keyfind(api_auth_token, 1, C) of
        {api_auth_token, AuthToken} ->
            Params#{header => Headers#{<<"Authorization">> => <<"Bearer ", AuthToken/binary>>}};
        _ ->
            Params
    end.

append_request_id(Params = #{header := Headers}) ->
    Params#{header => Headers#{<<"X-Request-ID">> => woody_context:new_req_id()}}.

format_ts(Ts) ->
    {ok, Result} = rfc3339:format(Ts, seconds),
    Result.

%%

get_app_config(Port, Netloc, PemFile, AutomatonUrl) ->
    [
        {space_size             , 8},
        {hash_algorithm         , sha256},
        {api, #{
            ip                 => "::",
            port               => Port,
            authorizer         => #{
                signee         => local,
                keyset => #{
                    local      => {pem_file, PemFile}
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
            automaton          => #{url => AutomatonUrl}
        }},
        {service_deadlines, #{
            automaton          => 5000 % milliseconds
        }},
        {service_retries, #{
            automaton          => #{
                'Start'   => {linear, 3, 1000},
                '_'       => finish
            }
        }}
    ].
