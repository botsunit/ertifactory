

# Module ertifactory #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deploy-4">deploy/4</a></td><td> 
Stores an artifact into a given repository.</td></tr><tr><td valign="top"><a href="#get_deployed_artifact-4">get_deployed_artifact/4</a></td><td>
Gets a given artifact from an Artifactory repository.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deploy-4"></a>

### deploy/4 ###

<pre><code>
deploy(BaseUrl::string(), Repository::string(), Package::string(), Options::term()) -&gt; {ok, Url::string()} | {error, Reason::term()}
</code></pre>
<br />


Stores an artifact into a given repository.

Availables options :

* `username`

* `password`

* `api_key`

* `path`


<a name="get_deployed_artifact-4"></a>

### get_deployed_artifact/4 ###

<pre><code>
get_deployed_artifact(BaseUrl::string(), Repository::string(), Package::string(), Options::term()) -&gt; {ok, FilePath::string()} | {error, Reason::term()}
</code></pre>
<br />

Gets a given artifact from an Artifactory repository.
Returns the local path where the downloaded release has been stored

