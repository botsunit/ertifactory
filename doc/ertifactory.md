

# Module ertifactory #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deploy-4">deploy/4</a></td><td>
Stores an artifact into a given repository.</td></tr><tr><td valign="top"><a href="#search_by_properties-2">search_by_properties/2</a></td><td> 
Gets a given artifact from an Artifactory repository.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deploy-4"></a>

### deploy/4 ###

<pre><code>
deploy(BaseUrl::<a href="httpc.md#type-url">httpc:url()</a>, Repository::string(), Package::string(), Options::term()) -&gt; {ok, Url::string()} | {error, Reason::term()}
</code></pre>
<br />

Stores an artifact into a given repository.

* `BaseUrl` : URL string of an artifactory server

* `Repository` : Repository name of your repo in the artifactory server

* `Package` : Local path to the artifact file to deploy

* `Options` A list of {Key,Value} options. Keys may be atoms or strings. Values should be given as strings. See below.


Availables options :

* `{username, "my_user_name"}` : username of an artifactory account (Basic Auth.)

* `{password, "my_password"}` : associated password

* `{api_key, "my_api_key}` : Alternatively, the api_key used to connect the account.

* `{path, "my/path/to/artifact"}` : Path and name of the artifact in the repository;
if omitted, the last path component of "Package" will be used

* Other option tuples are set into the url string as a sequence of property-settings, separated by a semi-colon.


<a name="search_by_properties-2"></a>

### search_by_properties/2 ###

<pre><code>
search_by_properties(BaseUrl::<a href="httpc.md#type-url">httpc:url()</a>, Options::term()) -&gt; {ok, [ArtifactInfos::#{}]} | {error, Reason::term()}
</code></pre>
<br />


Gets a given artifact from an Artifactory repository. 
Returns the file info

* `BaseUrl` : URL string of an artifactory server

* `Options` A list of {Key,Value} options. Keys may be atoms or strings. Values should be given as strings. See below.


Availables options :

* `{username, "my_user_name"}` : username of an artifactory account (Basic Auth.)

* `{password, "my_password"}` : associated password

* `{api_key, "my_api_key}` : Alternatively, the api_key used to connect the account.

* Other option tuples use to search.


