-module(ertifactory).
-export([deploy/4, get_deployed_artifact/4]).
-include_lib("eunit/include/eunit.hrl").

% @doc
% Gets a given artifact from an Artifactory repository.
% Returns the local path where the downloaded release has been stored
% @end
-spec get_deployed_artifact(BaseUrl :: string(), Repository :: string(), Package :: string(), Options :: term()) ->
  {ok, FilePath::string()} | {error, Reason::term()}.
get_deployed_artifact(_BaseUrl, _Repository, _Package, _Options) ->
  ok.

% @doc
% Stores an artifact into a given repository.
% @end
-spec deploy(BaseUrl :: string(), Repository :: string(), Package :: string(), Options :: term() ) ->
  {ok, Url::string()} | {error, Reason::term()}.
deploy(BaseUrl, Repository, Package, Options) ->
  case ensure_net_started() of 
    ok ->
      Username = buclists:keyfind(username, 1, Options, undefined) ,
      Password = buclists:keyfind(password, 1, Options, undefined) ,
      ApiKey = buclists:keyfind(api_key, 1, Options, undefined),
      OptPath = buclists:keyfind(path, 1, Options, undefined),
      ApiKeyHeader = if 
        ApiKey =:= undefined -> [];
        true -> [{"X-Api-Key", ApiKey}]
      end, 
      BasicAuthHeader = if 
        Username =:= undefined orelse Password =:= undefined -> 
          [];
        true -> 
          [{"Authorization", lists:flatten("Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password))}]
      end,
      Header = lists:flatten([ApiKeyHeader|BasicAuthHeader]),
      PackageName = filename:basename(Package),
      URL = if 
        OptPath =:= undefined -> 
          BaseUrl ++ "/" ++ Repository ++ "/" ++ PackageName;
        true -> 
          BaseUrl ++ "/" ++ Repository ++ "/" ++ OptPath ++ "/" ++ PackageName
      end,
      _ = case file:read_file(Package) of
        {ok, Body} ->
          case httpc:request(put, {URL, Header, "application/x-gzip", Body}, [{ssl, [{verify, 0}]}], []) of
            {ok, {{_, 201, _}, _, _}} -> 
              ok;
            {ok, {{_, Code, Message}, _, _}} -> 
               {error, {Code, Message}};
            OtherReturn ->  
              OtherReturn
          end;
        Err -> 
          Err
      end;
    Err ->
      Err
end.

ensure_net_started() ->
  case application:ensure_all_started(inets) of
    {ok, _} -> 
      case application:ensure_all_started(ssl) of
        {ok, _} -> 
          ok;
        Err -> 
          Err
      end;
    Err -> 
      Err
  end.

