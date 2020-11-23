-module(shortener_bouncer_client).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

%% API

-export([make_shortener_context_fragment/3]).

%%

-type operation_id() :: binary().
-type id() :: shortener_slug:id().
-type owner() :: shortener_slug:owner().

-spec make_shortener_context_fragment(operation_id(), id() | undefined, owner() | undefined) ->
    bouncer_client:context_fragment().
make_shortener_context_fragment(OperationID, ID, OwnerID) ->
    {fragment, #bctx_v1_ContextFragment{
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
    }}.
