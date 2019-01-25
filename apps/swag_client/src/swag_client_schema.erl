%% -*- mode: erlang -*-
-module(swag_client_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_client:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        {Parents, _} = maps:fold(fun check_definition/3, {#{}, #{}}, Defs),
        maps:fold(fun correct_schema/3, Schema, Parents)
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

check_definition(Name, Schema, Acc) ->
    Acc1 = check_discriminator(Name, Schema, Acc),
    check_backrefs(Name, Schema, Acc1).

check_discriminator(Name, Schema, {Parents, Candidates}) ->
    case maps:get(<<"discriminator">>, Schema, undefined) of
        undefined ->
            {Parents, Candidates};
        _ ->
            {
                Parents#{Name => maps:get(Name, Candidates, [])},
                maps:without([Name], Candidates)
            }
    end.

check_backrefs(Name, Schema, Acc) ->
    case maps:get(<<"allOf">>, Schema, undefined) of
        undefined ->
            Acc;
        AllOf ->
            lists:foldl(fun(E, A) -> check_allOf(E, Name, A) end, Acc, AllOf)
    end.

check_allOf(#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Child, {Parents, Candidates}) ->
    case maps:get(Parent, Parents, undefined) of
        undefined ->
            {Parents, update_candidates(Parent, Child, Candidates)};
        Children ->
            {Parents#{Parent => [Child | Children]}, Candidates}
    end;
check_allOf(_, _, Acc) ->
    Acc.

update_candidates(Parent, Child, Candidates) ->
    case maps:get(Parent, Candidates, undefined) of
        undefined ->
            Candidates#{Parent => [Child]};
        Children ->
            Candidates#{Parent => [Child | Children]}
    end.

correct_schema(Parent, Children, Schema) ->
    BasePath = [Parent, ?DEFINITIONS],
    Discr    = maps:get(<<"discriminator">>, get_sub_schema(BasePath, Schema)),
    update_schema(Children, [<<"enum">>, Discr, <<"properties">> | BasePath], Schema).

update_schema(Value, [], _Schema) ->
    Value;
update_schema(Value, [Key | Path], Schema) ->
    SubSchema0 = get_sub_schema(Path, Schema),
    SubSchema1 = update_sub_schema(Key, Value, SubSchema0),
    update_schema(SubSchema1, Path, Schema).

get_sub_schema(ReversedPath, Schema) ->
    lists:foldr(fun(K, S) -> maps:get(K, S) end, Schema, ReversedPath).

update_sub_schema(Key, Value, Schema) ->
    Schema#{Key => Value}.

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"URL shortener API\n">>,
    <<"version">> => <<"1.0">>,
    <<"title">> => <<"RBKmoney URL shortener API">>,
    <<"termsOfService">> => <<"http://rbkmoney.com/">>,
    <<"contact">> => #{
      <<"name">> => <<"RBKmoney support team">>,
      <<"url">> => <<"https://api.rbk.money">>,
      <<"email">> => <<"tech-support@rbkmoney.com">>
    }
  },
  <<"host">> => <<"rbk.mn">>,
  <<"basePath">> => <<"/v1">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Shortener">>,
    <<"description">> => <<"Получение и работа с короткими ссылками">>,
    <<"x-displayName">> => <<"Короткие ссылки">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/shortened-urls">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Shortener">> ],
        <<"description">> => <<"Создать новую короткую ссылку">>,
        <<"operationId">> => <<"shortenUrl">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"shortenedUrlParams">>,
          <<"description">> => <<"Параметры для создания короткой ссылки">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/ShortenedUrlParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Короткая ссылка создана">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ShortenedUrl">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Операция требует авторизации">>
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          }
        }
      }
    },
    <<"/shortened-urls/{shortenedUrlID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Shortener">> ],
        <<"description">> => <<"Получить данные созданной короткой ссылки">>,
        <<"operationId">> => <<"getShortenedUrl">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shortenedUrlID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор короткой ссылки">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Данные короткой ссылки">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ShortenedUrl">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Операция требует авторизации">>
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          },
          <<"404">> => #{
            <<"description">> => <<"Объект не найден">>
          }
        }
      },
      <<"delete">> => #{
        <<"tags">> => [ <<"Shortener">> ],
        <<"description">> => <<"Удалить короткую ссылку">>,
        <<"operationId">> => <<"deleteShortenedUrl">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shortenedUrlID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор короткой ссылки">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Короткая ссылка удалена">>
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Операция требует авторизации">>
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          },
          <<"404">> => #{
            <<"description">> => <<"Объект не найден">>
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Для аутентификации вызовов мы используем [JWT](https://jwt.io). Cоответствующий ключ передается в заголовке.\n```shell\n Authorization: Bearer {TOKENIZATION|PRIVATE_JWT}\n```\nПосмотреть ваш API-ключ вы можете в [личном кабинете](https://dashboard.rbk.money/api/key). Ключи не разделяются на тестовые и боевые, ваш API ключ открывает доступ ко всем функциям платформы. Для тестовых транзакций используйте ID тестовых магазинов. Помните, что вы никому не должны передавать ваш API ключ!\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"ShortenedUrl">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"expiresAt">>, <<"id">>, <<"shortenedUrl">>, <<"sourceUrl">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>
        },
        <<"shortenedUrl">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>
        },
        <<"sourceUrl">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>
        },
        <<"expiresAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }
      },
      <<"example">> => #{
        <<"sourceUrl">> => <<"http://example.com/aeiou">>,
        <<"shortenedUrl">> => <<"http://example.com/aeiou">>,
        <<"id">> => <<"id">>,
        <<"expiresAt">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"ShortenedUrlParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"expiresAt">>, <<"sourceUrl">> ],
      <<"properties">> => #{
        <<"sourceUrl">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>
        },
        <<"expiresAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }
      },
      <<"example">> => #{
        <<"sourceUrl">> => <<"http://example.com/aeiou">>,
        <<"expiresAt">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"inline_response_400">> => #{
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>
        }
      }
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"shortenedUrlID">> => #{
      <<"name">> => <<"shortenedUrlID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор короткой ссылки">>,
      <<"required">> => true,
      <<"type">> => <<"string">>
    }
  },
  <<"responses">> => #{
    <<"BadRequest">> => #{
      <<"description">> => <<"Неверные данные запроса">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/inline_response_400">>
      }
    },
    <<"Forbidden">> => #{
      <<"description">> => <<"Операция недоступна">>
    },
    <<"NotFound">> => #{
      <<"description">> => <<"Объект не найден">>
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Операция требует авторизации">>
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(get_sub_schema([<<"enum">>, Discr, <<"properties">>, Parent, ?DEFINITIONS], Schema)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
