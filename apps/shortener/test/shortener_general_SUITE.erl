-module(shortener_general_SUITE).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([successful_redirect/1]).
-export([successful_delete/1]).
-export([fordidden_source_url/1]).
-export([url_expired/1]).
-export([always_unique_url/1]).

-export([health_check_passing/1]).

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
        woody_timeout_test,
        health_check_passing
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {general, [], [
            successful_redirect,
            successful_delete,
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
            {storage, scoper_storage_logger}
        ]) ++
            genlib_app:start_application_with(bouncer_client, shortener_ct_helper:get_bouncer_client_app_config()),
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
        genlib_app:start_application_with(
            shortener,
            shortener_ct_helper:get_app_config(
                ?config(port, C),
                ?config(netloc, C),
                get_keysource("keys/local/private.pem", C)
            )
        ),
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

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    shortener_ct_helper:with_test_sup(C).

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    shortener_ct_helper:stop_test_sup(C),
    ok.

%%

-spec successful_redirect(config()) -> _.
-spec successful_delete(config()) -> _.
-spec fordidden_source_url(config()) -> _.
-spec url_expired(config()) -> _.
-spec always_unique_url(config()) -> _.

successful_redirect(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C1 = set_api_auth_token(successful_redirect, C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"sourceUrl">> := SourceUrl, <<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

successful_delete(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C1 = set_api_auth_token(successful_delete, C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 204, _, _} = delete_shortened_url(ID, C1),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

fordidden_source_url(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C1 = set_api_auth_token(fordidden_source_url, C),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"http://localhost/hack?id=42">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"https://localhost/hack?id=42">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://example.io/">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://local.domain/phpmyadmin">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"ftp://ftp.hp.com/pub/hpcp/newsletter_july2003">>), C1).

url_expired(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C1 = set_api_auth_token(url_expired, C),
    Params = construct_params(<<"https://oops.io/">>, 1),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    ok = timer:sleep(2 * 1000),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

always_unique_url(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C1 = set_api_auth_token(always_unique_url, C),
    N = 42,
    Params = construct_params(<<"https://oops.io/">>, 3600),
    {IDs, ShortUrls} = lists:unzip([
        {ID, ShortUrl}
        || _ <- lists:seq(1, N),
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
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(unsupported_cors_method, C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"PATCH">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    false = lists:member(<<"access-control-allow-methods">>, Headers).

supported_cors_method(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(supported_cors_method, C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"GET">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_methods(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    Allowed = binary:split(Returned, <<",">>, [global]).

supported_cors_header(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(supported_cors_header, C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [
        {<<"origin">>, <<"localhost">>},
        {<<"access-control-request-method">>, <<"GET">>},
        {<<"access-control-request-headers">>, <<"content-type,authorization">>}
    ],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_headers(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    % truncate origin
    [_ | Allowed] = binary:split(Returned, <<",">>, [global]).

unsupported_cors_header(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(unsupported_cors_header, C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [
        {<<"origin">>, <<"localhost">>},
        {<<"access-control-request-method">>, <<"GET">>},
        {<<"access-control-request-headers">>, <<"content-type,42">>}
    ],
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
    Apps = genlib_app:start_application_with(
        shortener,
        shortener_ct_helper:get_app_config(
            ?config(port, C),
            ?config(netloc, C),
            get_keysource("keys/local/private.pem", C),
            <<"http://invalid_url:8022/v1/automaton">>
        )
    ),
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{
                resolution = {allowed, #bdcs_ResolutionAllowed{}},
                resolution_legacy = allowed
            }} end}
        ],
        C
    ),
    C2 = set_api_auth_token(woody_timeout_test, C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {Time, {error, {invalid_response_code, 503}}} =
        timer:tc(fun() ->
            shorten_url(Params, C2)
        end),
    true = (Time >= 3000000),
    genlib_app:stop_unload_applications(Apps).

%%
-spec health_check_passing(config()) -> _.
health_check_passing(C) ->
    Apps = genlib_app:start_application_with(
        shortener,
        shortener_ct_helper:get_app_config(
            ?config(port, C),
            ?config(netloc, C),
            get_keysource("keys/local/private.pem", C)
        )
    ),
    Path = ?config(api_endpoint, C) ++ "/health",
    {ok, 200, _, Payload} = hackney:request(get, Path, [], <<>>, [with_body]),
    #{<<"service">> := <<"shortener">>} = jsx:decode(Payload, [return_maps]),
    genlib_app:stop_unload_applications(Apps).

%%
set_api_auth_token(Name, C) ->
    UserID = genlib:to_binary(Name),
    ACL = construct_shortener_acl([]),
    {ok, T} = shortener_authorizer_jwt:issue({{UserID, shortener_acl:from_list(ACL)}, #{}}, unlimited),
    lists:keystore(api_auth_token, 1, C, {api_auth_token, T}).

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
    append_media_type(
        append_auth(
            append_request_id(
                maps:merge(#{binding => #{}, qs_val => #{}, header => #{}, body => #{}}, Params)
            ),
            C
        )
    ).

append_media_type(Params = #{header := Headers}) ->
    Params#{
        header => Headers#{
            <<"Accept">> => <<"application/json">>,
            <<"Content-Type">> => <<"application/json; charset=utf-8">>
        }
    }.

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
    genlib_rfc3339:format(Ts, second).
