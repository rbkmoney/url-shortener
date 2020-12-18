-module(shortener_bouncer_client).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

%% API

-export([add_shortener/2]).

%%

-type operation_id() :: binary().
-type id() :: shortener_slug:id().
-type owner() :: shortener_slug:owner().
-type params() :: #{
    operation_id := operation_id(),
    shortened_url_id := id() | undefined,
    shortened_url_owner_id := owner() | undefined
}.

-spec add_shortener(params(), bouncer_client:context_fragment()) ->
    bouncer_client:context_fragment().
add_shortener(Params, ContextFragment) ->
    #{
        operation_id := OperationID,
        shortened_url_id := ID,
        shortened_url_owner_id := OwnerID
    } = Params,
    ContextFragment#bctx_v1_ContextFragment{
        shortener = #bctx_v1_ContextUrlShortener{
            op = #bctx_v1_UrlShortenerOperation{
                id = OperationID,
                shortened_url = #bctx_v1_ShortenedUrl{
                    id = ID,
                    owner = #bctx_v1_Entity{
                        id = OwnerID
                    }
                }
            }
        }
    }.
