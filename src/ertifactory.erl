-module(ertifactory).
-export([deploy/4, get_deployed_artifact/4]).
-include_lib("eunit/include/eunit.hrl").

-define(ERTIFACTORY_OPTIONS,["username", "password", "api_key", "path"]).

% @doc
% Gets a given artifact from an Artifactory repository.
% Returns the local path where the downloaded release has been stored
% @end
-spec get_deployed_artifact(BaseUrl :: httpc:url(), Repository :: string(), Package :: string(), Options :: term()) ->
  {ok, FilePath::string()} | {error, Reason::term()}.
get_deployed_artifact(_BaseUrl, _Repository, _Package, _Options) ->
  ok.

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
  case ensure_net_started() of 
    ok ->
      Username = buclists:keyufind(username, 1, Options, undefined) ,
      Password = buclists:keyufind(password, 1, Options, undefined) ,
      ApiKey = buclists:keyufind(api_key, 1, Options, undefined),
      OptPath = buclists:keyufind(path, 1, Options, ""),
      ApiKeyHeader = if 
                       ApiKey =:= undefined -> [];
                       true -> [{"X-JFrog-Art-Api", ApiKey}]
                     end, 
      BasicAuthHeader = if 
                          Username =:= undefined orelse Password =:= undefined -> 
                            [];
                          true -> 
                            [{"Authorization", lists:flatten("Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password))}]
                        end,
      Header = lists:flatten([ApiKeyHeader|BasicAuthHeader]),
      PackageName = filename:basename(Package),
      URL = string:join(buclists:delete_if(fun(E) -> E =:= "" end, [BaseUrl, Repository, OptPath, PackageName]), "/") ++ other_options_to_string(Options),
      case file:read_file(Package) of
        {ok, Body} ->
          case httpc:request(put, {URL, Header, bucs:to_string(bucmime:type(Package)), Body}, [{ssl, [{verify, 0}]}], []) of
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

other_options_to_string(Options) ->
  lists:flatten(
    lists:filtermap(
      fun({K, V}) ->
          KStr = bucs:to_string(K),
          case lists:member(KStr, ?ERTIFACTORY_OPTIONS) of
            false -> 
              {true, io_lib:format(";~s=~s", [KStr,V])};
            true -> 
              false
          end
      end,
      Options)).

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

