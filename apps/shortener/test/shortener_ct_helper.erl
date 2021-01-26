-module(shortener_ct_helper).

-include_lib("common_test/include/ct.hrl").

-export([with_test_sup/1]).
-export([stop_test_sup/1]).
-export([mock_services/2]).
-export([get_app_config/3]).
-export([get_app_config/4]).
-export([get_bouncer_client_app_config/0]).

-type config() :: [{atom(), any()}].

-define(SHORTENER_IP, "::").
-define(SHORTENER_PORT, 8080).
-define(SHORTENER_HOST_NAME, "localhost").
-define(SHORTENER_URL, ?SHORTENER_HOST_NAME ++ ":" ++ integer_to_list(?SHORTENER_PORT)).

-spec with_test_sup(config()) -> config().
with_test_sup(C) ->
    {ok, SupPid} = genlib_adhoc_supervisor:start_link(#{}, []),
    _ = unlink(SupPid),
    [{test_sup, SupPid} | C].

-spec stop_test_sup(config()) -> _.
stop_test_sup(C) ->
    exit(?config(test_sup, C), shutdown).

-spec mock_services(list(), config()) -> _.
mock_services(Services, SupOrConfig) ->
    maps:map(fun set_cfg/2, mock_services_(Services, SupOrConfig)).

set_cfg(Service, Url) when Service =:= bouncer orelse Service =:= org_management ->
    {ok, Clients} = application:get_env(bouncer_client, service_clients),
    #{Service := Cfg} = Clients,
    ok = application:set_env(
        bouncer_client,
        service_clients,
        Clients#{Service => Cfg#{url => Url}}
    );
set_cfg(Service, Url) ->
    {ok, Clients} = application:get_env(shortener, service_clients),
    #{Service := Cfg} = Clients,
    ok = application:set_env(
        shortener,
        service_clients,
        Clients#{Service => Cfg#{url => Url}}
    ).

mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));
mock_services_(Services, SupPid) when is_pid(SupPid) ->
    ServerID = {dummy, lists:map(fun get_service_name/1, Services)},
    {ok, IP} = inet:parse_address(?SHORTENER_IP),
    ChildSpec = woody_server:child_spec(
        ServerID,
        Options = #{
            ip => IP,
            port => 0,
            event_handler => scoper_woody_event_handler,
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    {IP, Port} = woody_server:get_addr(ServerID, Options),
    lists:foldl(
        fun(Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {shortener_mock_service, #{function => Fun}}}}.

get_service_modname(bouncer) ->
    {bouncer_decisions_thrift, 'Arbiter'}.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?SHORTENER_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

%%

-spec get_app_config(_, _, _) -> _.
get_app_config(Port, Netloc, PemFile) ->
    get_app_config(Port, Netloc, PemFile, <<"http://machinegun:8022/v1/automaton">>).

-spec get_app_config(_, _, _, _) -> _.
get_app_config(Port, Netloc, PemFile, AutomatonUrl) ->
    [
        {bouncer_ruleset_id, <<"service/authz/api">>},
        {space_size, 8},
        {hash_algorithm, sha256},
        {api, #{
            ip => "::",
            port => Port,
            authorizer => #{
                signee => local,
                keyset => #{
                    local => {pem_file, PemFile}
                }
            },
            source_url_whitelist => [
                "https://*",
                "ftp://*",
                "http://localhost/*"
            ],
            short_url_template => #{
                scheme => http,
                netloc => Netloc,
                path => "/r/e/d/i/r/"
            }
        }},
        {processor, #{
            ip => "::",
            port => 8022
        }},
        {health_check, #{
            service => {erl_health, service, [<<"shortener">>]}
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
                    'Start' => {linear, 3, 1000},
                    'GetMachine' => {linear, 3, 1000},
                    'Remove' => {linear, 3, 1000},
                    '_' => finish
                }
            }
        }}
    ].

-spec get_bouncer_client_app_config() -> _.
get_bouncer_client_app_config() ->
    [
        {service_clients, #{
            bouncer => #{
                url => <<"http://bouncer:8022/">>,
                retries => #{
                    'Judge' => {linear, 3, 1000},
                    '_' => finish
                }
            },
            org_management => #{
                url => <<"http://org_management:8022/">>,
                retries => #{
                    % function => retry strategy
                    % '_' work as "any"
                    % default value is 'finish'
                    % for more info look genlib_retry :: strategy()
                    % https://github.com/rbkmoney/genlib/blob/master/src/genlib_retry.erl#L19
                    'GetUserContext' => {linear, 3, 1000},
                    '_' => finish
                }
            }
        }}
    ].
