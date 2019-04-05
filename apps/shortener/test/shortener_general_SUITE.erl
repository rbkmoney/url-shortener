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

-export([unsupported_cors_method/1]).
-export([supported_cors_method/1]).
-export([unsupported_cors_header/1]).
-export([supported_cors_header/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [test_case_name()].
all() ->
    [
        {group, general},
        {group, cors},
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
        ]},
        {cors, [], [
            unsupported_cors_method,
            supported_cors_method,
            unsupported_cors_header,
            supported_cors_header
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
        genlib_app:start_application_with(scoper, [
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

init_per_group(_Group, C) ->
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

end_per_group(_Group, C) ->
    genlib_app:stop_unload_applications(?config(shortener_app, C)).

get_keysource(Key, C) ->
    filename:join(?config(data_dir, C), Key).

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(test_case_name(), config()) ->
    config().
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
    C1 = set_api_auth_token(insufficient_permissions, [], C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 403, _, _} = shorten_url(Params, C1),
    {ok, 403, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 403, _, _} = get_shortened_url(<<"42">>, C1).

readonly_permissions(C) ->
    C1 = set_api_auth_token(readonly_permissions, [read, write], C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(readonly_permissions, [read], C1),
    {ok, 200, _, #{<<"id">> := ID}} = get_shortened_url(ID, C2),
    {ok, 403, _, _} = delete_shortened_url(ID, C2).

successful_redirect(C) ->
    C1 = set_api_auth_token(successful_redirect, [read, write], C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"sourceUrl">> := SourceUrl, <<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

successful_delete(C) ->
    C1 = set_api_auth_token(successful_delete, [read, write], C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 204, _, _} = delete_shortened_url(ID, C1),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

other_subject_delete(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(other_subject_delete_first, [read, write], C),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_delete_second, [read, write], C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C2),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

other_subject_read(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    C1 = set_api_auth_token(other_subject_read_first, [read, write], C),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_read_second, [read, write], C1),
    {ok, 403, _, _} = get_shortened_url(ID, C2).

fordidden_source_url(C) ->
    C1 = set_api_auth_token(fordidden_source_url, [read, write], C),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"http://localhost/hack?id=42">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"https://localhost/hack?id=42">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://example.io/">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://local.domain/phpmyadmin">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"ftp://ftp.hp.com/pub/hpcp/newsletter_july2003">>), C1).

url_expired(C) ->
    C1 = set_api_auth_token(url_expired, [read, write], C),
    Params = construct_params(<<"https://oops.io/">>, 1),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    ok = timer:sleep(2 * 1000),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

always_unique_url(C) ->
    C1 = set_api_auth_token(always_unique_url, [read, write], C),
    N = 42,
    Params = construct_params(<<"https://oops.io/">>, 3600),
    {IDs, ShortUrls} = lists:unzip([
        {ID, ShortUrl} ||
            _ <- lists:seq(1, N),
            {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} <- [shorten_url(Params, C1)]
    ]),
    N = length(lists:usort(IDs)),
    N = length(lists:usort(ShortUrls)).


%% cors
-spec unsupported_cors_method(config()) -> _.
-spec supported_cors_method(config()) -> _.
-spec unsupported_cors_header(config()) -> _.
-spec supported_cors_header(config()) -> _.

unsupported_cors_method(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(unsupported_cors_method, [read, write], C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"PATCH">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    false = lists:member(<<"access-control-allow-methods">>, Headers).

supported_cors_method(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(supported_cors_method, [read, write], C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"GET">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_methods(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    Allowed = binary:split(Returned, <<",">>, [global]).


supported_cors_header(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(supported_cors_header, [read, write], C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"GET">>}, {<<"access-control-request-headers">>, <<"content-type,authorization">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_headers(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    [_ | Allowed] = binary:split(Returned, <<",">>, [global]). % truncate origin

unsupported_cors_header(C) ->
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(unsupported_cors_header, [read, write], C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"GET">>}, {<<"access-control-request-headers">>, <<"content-type,42">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    false = lists:member(<<"access-control-allow-headers">>, Headers),
    false = lists:member(<<"access-control-allow-credentials">>, Headers),
    false = lists:member(<<"access-control-allow-methods">>, Headers),
    false = lists:member(<<"access-control-allow-origin">>, Headers).

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
    C2 = set_api_auth_token(woody_timeout_test, [read, write], C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {Time, {error, {invalid_response_code, 503}}} =
        timer:tc(fun() ->
            shorten_url(Params, C2)
        end),
    true = (Time >= 3000000),
    genlib_app:stop_unload_applications(Apps).

%%
set_api_auth_token(Name, Permissions, C) ->
    UserID = genlib:to_binary(Name),
    ACL = construct_shortener_acl(Permissions),
    {ok, T} = shortener_authorizer_jwt:issue({{UserID, shortener_acl:from_list(ACL)}, #{}}, unlimited),
    lists:keystore(api_auth_token, 1, C, {api_auth_token, T}).

clean_api_auth_token(C) ->
    lists:keydelete(api_auth_token, 1, C).

construct_shortener_acl(Permissions) ->
    lists:map(fun(P) -> {['shortened-urls'], P} end, Permissions).

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
            automaton => #{
                url => AutomatonUrl,
                retries => #{
                    % function => retry strategy
                    % '_' work as "any"
                    % default value is 'finish'
                    % for more info look genlib_retry :: strategy()
                    % https://github.com/rbkmoney/genlib/blob/master/src/genlib_retry.erl#L19
                    'Start'   => {linear, 3, 1000},
                    'GetMachine'   => {linear, 3, 1000},
                    'Remove'   => {linear, 3, 1000},
                    '_'     => finish
                }
            }
        }}
    ].
