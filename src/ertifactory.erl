-module(ertifactory).
-export([deploy/4, 
         search_by_properties/2,
         set_artifact_properties/3]).
-include_lib("eunit/include/eunit.hrl").

-define(ERTIFACTORY_OPTIONS,["username", "password", "api_key", "path"]).
-define(SEARCH_BY_PROPERTIES_PATH, "/api/search/prop").

% @doc
% Gets a given artifact from an Artifactory repository.
% Returns the file info
%
% <ul>
% <li><tt>BaseUrl</tt> : URL string of an artifactory server</li>
% <li><tt>Options</tt> A list of {Key,Value} options. Keys may be atoms or strings. Values should be given as strings. See below.</li>
% </ul>
%
% Availables options :
% <ul>
% <li><tt>{username, "my_user_name"} </tt> : username of an artifactory account (Basic Auth.)</li>
% <li><tt>{password, "my_password"} </tt> : associated password</li>
% <li><tt>{api_key, "my_api_key} </tt> : Alternatively, the api_key used to connect the account.</li>
% <li>Other option tuples use to search.</li>
% </ul>
% @end
-spec search_by_properties(BaseUrl :: httpc:url(), Options :: term()) ->
  {ok, [ArtifactInfos :: map()]} | {error, Reason::term()}.
search_by_properties(BaseUrl, Options) ->
  case auth_headers(Options) of
    {ok, Headers} ->
      URL = build_url([BaseUrl, ?SEARCH_BY_PROPERTIES_PATH], Options),
      case httpc:request(get, 
                         {URL, Headers}, 
                         [{ssl, [{verify, 0}]}], []) of
        {ok, {{_, 200, _}, _, Body}} -> 
          case jsx:is_json(bucs:to_binary(Body)) of
            true ->
              case jsx:decode(bucs:to_binary(Body), [return_maps, {labels, atom}]) of
                #{results := Results} ->
                  {ok, [file_info(finalize_url(X, BaseUrl), Options) || #{uri := X} <- Results]};
                _ ->
                  {error, invalid_response}
              end;
            false ->
              {error, invalid_response1, Body}
          end;
        {ok, {{_, Code, Message}, _, _}} -> 
          {error, {Code, Message}};
        OtherReturn ->  
          OtherReturn
      end;
    Error ->
      Error
  end.

% @doc
% Stores an artifact into a given repository.
% <ul>
% <li><tt>BaseUrl</tt> : URL string of an artifactory server</li>
% <li><tt>Repository</tt> : Repository name of your repo in the artifactory server </li>
% <li><tt>Package</tt> : Local path to the artifact file to deploy</li>
% <li><tt>Options</tt> A list of {Key,Value} options. Keys may be atoms or strings. Values should be given as strings. See below.</li>
% </ul>
%
% Availables options :
% <ul>
% <li><tt>{username, "my_user_name"} </tt> : username of an artifactory account (Basic Auth.)</li>
% <li><tt>{password, "my_password"} </tt> : associated password</li>
% <li><tt>{api_key, "my_api_key} </tt> : Alternatively, the api_key used to connect the account.</li>
% <li><tt>{path, "my/path/to/artifact"}</tt> : Path and name of the artifact in the repository; 
% if omitted, the last path component of "Package" will be used</li>
% <li>Other option tuples are set into the url string as a sequence of property-settings, separated by a semi-colon.</li>
% </ul>
% @end
-spec deploy(BaseUrl :: httpc:url(), Repository :: string(), Package :: string(), Options :: term() ) ->
  {ok, Url::string()} | {error, Reason::term()}.
deploy(BaseUrl, Repository, Package, Options) ->
  case auth_headers(Options) of
    {ok, Headers} ->
      OptPath = buclists:keyufind(path, 1, Options, ""),
      PackageName = filename:basename(Package),
      URL = build_url([BaseUrl, Repository, OptPath, PackageName], 
                      Options, 
                      [{param_separator, ";"},
                       {param_delimiter, ";"},
                       {exclude, ?ERTIFACTORY_OPTIONS}]),
      case file:read_file(Package) of
        {ok, Body} ->
          case httpc:request(put, 
                             {URL, Headers, bucs:to_string(bucmime:type(Package)), Body}, 
                             [{ssl, [{verify, 0}]}], []) of
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

% @doc
% Sets properties for a given artifact in an Artifactory repository.
% Returns ok or {error, Reason}.
%
% <ul>
% <li><tt>BaseUrl</tt> : URL string of an artifactory server</li>
% <li><tt>Options</tt> : A list of {Key,Value} options. Keys may be atoms or strings. Values should be given as strings. See below.</li>
% </ul>
%
% Availables options :
% <ul>
% <li><tt>{username, "my_user_name"} </tt> : username of an artifactory account (Basic Auth.)</li>
% <li><tt>{password, "my_password"} </tt> : associated password</li>
% <li><tt>{api_key, "my_api_key} </tt> : Alternatively, the api_key used to connect the account.</li>
% <li><tt>{path, "my/path/to/artifact:1.0.0"}</tt> : Path and name of the artifact in the repository; 
% <li>Other option tuples use to set properties of the artifact.</li>
% </ul>
% @end
-spec set_artifact_properties(BaseUrl :: httpc:url(), Repository :: string(), Options :: term()) ->
    ok | {error, Reason::term()}.
set_artifact_properties(BaseUrl, Repository, Options) ->
  case auth_headers(Options) of
    {ok, Headers} ->
      OptPath = buclists:keyufind(path, 1, Options, ""),
      URL = build_url([BaseUrl, "api", "storage", Repository, OptPath], 
                      Options, 
                      [{param_separator, "?properties="},
                       {param_delimiter, "|"},
                       {exclude, ?ERTIFACTORY_OPTIONS}]),
      case httpc:request(put, {URL, Headers, "application/x-www-form-urlencoded",""},[{ssl, [{verify, 0}]}], []) of
        {ok, _ } -> 
          ok;
        OtherReturn ->  
          OtherReturn
      end;
    Err ->
      Err
  end.



auth_headers(Options) ->
  case ensure_net_started() of 
    ok ->
      Username = buclists:keyufind(username, 1, Options, undefined) ,
      Password = buclists:keyufind(password, 1, Options, undefined) ,
      ApiKey = buclists:keyufind(api_key, 1, Options, undefined),
      {ok, if 
             ApiKey =:= undefined -> [];
             true -> [{"X-JFrog-Art-Api", ApiKey}]
           end ++ 
           if 
             Username =:= undefined orelse Password =:= undefined -> 
               [];
             true -> 
               [{"Authorization", 
                 lists:flatten("Basic " ++ 
                               base64:encode_to_string(
                                 string:join([Username, Password], ":")))}]
           end};
    Error -> Error
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

build_url(URLParts, Params) ->
  build_url(URLParts, Params, []).
build_url([Base|Rest], Params, Options) ->
  Exclude = buclists:keyufind(exclude, 1, Options, ?ERTIFACTORY_OPTIONS),
  lists:flatten(
    string:join(
      [prepare_url(Base, right)|[prepare_url(Part, both) || Part <- Rest]],
      "/") ++ 
    buclists:keyufind(param_separator, 1, Options, "?") ++ 
    string:join(
      lists:filtermap(fun({K, V}) ->
                          K1 = bucs:to_string(K),
                          case lists:member(K1, Exclude) of
                            false ->
                              {true, string:join([K1, bucs:to_string(V)], "=")};
                            true ->
                              false
                          end
                      end, Params),
      buclists:keyufind(param_delimiter, 1, Options, "&"))).

prepare_url(URL, Direction) ->
  string:strip(URL, Direction, $/).

finalize_url(URL, Base) when is_list(URL), is_list(Base) ->
  case URL of
    [$:|_] -> 
      case http_uri:parse(Base, [{scheme_defaults, http_uri:scheme_defaults()}, 
                            {fragment, true}]) of
        {ok, {Scheme, _, _, _, _, _, _}} -> bucs:to_string(Scheme) ++ URL;
        _ -> URL
      end;
    _ -> URL
  end;
finalize_url(URL, Base) ->
  finalize_url(bucs:to_string(URL), bucs:to_string(Base)).

file_info(FileURL, Options) ->
  case auth_headers(Options) of
    {ok, Headers} ->
      URL = build_url([FileURL], Options),
      case httpc:request(get,
                         {URL, Headers},
                         [{ssl, [{verify, 0}]}], []) of
        {ok, {{_, 200, _}, _, Body}} -> 
          case jsx:is_json(bucs:to_binary(Body)) of
            true ->
              jsx:decode(bucs:to_binary(Body), [return_maps, {labels, atom}]);
            false ->
              {error, invalid_response}
          end;
        {ok, {{_, Code, Message}, _, _}} -> 
          {error, {Code, Message}};
        OtherReturn ->  
          OtherReturn
      end;
    Error ->
      Error
  end.

