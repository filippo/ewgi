-ifndef(_EWGI_HRL).
-define(_EWGI_HRL, 1).

% ``The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
% License for the specific language governing rights and limitations
% under the License.
% 
% The Original Code is the EWGI reference implementation.
% 
% The Initial Developer of the Original Code is S.G. Consulting
% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
% 2007 S.G. Consulting srl. All Rights Reserved.
% 
% Contributor(s): Filippo Pacini <filippo.pacini@gmail.com>
%                 Hunter Morris <huntermorris@gmail.com>

%%
%% HTTP status codes
%%
%% 1xx: Informational
-define(CONTINUE,     {100, "Continue"}).
-define(SWITCH_PROTO, {101, "Switching Protocols"}).

%% 2xx: Success
-define(OK,                {200, "OK"}).
-define(CREATED,           {201, "Created"}).
-define(ACCEPTED,          {202, "Accepted"}).
-define(NON_AUTHORITATIVE, {203, "Non-Authoritative Information"}).
-define(NO_CONTENT,        {204, "No Content"}).
-define(RESET,             {205, "Reset Content"}).
-define(PARTIAL,           {206, "Partial Content"}).

%% 3xx: Redirection
-define(MULTIPLE_CHOICES, {300, "Multiple Choices"}).
-define(MOVED,            {301, "Moved Permanently"}).
-define(FOUND,            {302, "Found"}).
-define(SEE_OTHER,        {303, "See Other"}).
-define(REDIRECT,         {303, "See Other"}). % alias for see_other
-define(NOT_MODIFIED,     {304, "Not Modified"}).
-define(USE_PROXY,        {305, "Use Proxy"}).
-define(TEMPORARY,        {307, "Temporary Redirect"}).

%% 4xx: Client Error
-define(BAD_REQ,                   {400, "Bad Request"}).
-define(UNAUTH,                    {401, "Unauthorized"}).
-define(PAYMENT_REQUIRED,          {402, "Payment Required"}).
-define(FORBIDDEN,                 {403, "Forbidden"}).
-define(NOT_FOUND,                 {404, "Not Found"}).
-define(METHOD_NOT_ALLOWED,        {405, "Method Not Allowed"}).
-define(NOT_ACCEPTABLE,            {406, "Not Acceptable"}).
-define(PROXY_AUTH_REQUIRED,       {407, "Proxy Authentication Required"}).
-define(REQ_TIMEOUT,               {408, "Request Time-out"}).
-define(CONFLICT,                  {409, "Conflict"}).
-define(GONE,                      {410, "Gone"}).
-define(LENGTH_REQUIRED,           {411, "Length Required"}).
-define(PRECONDITION_FAILED,       {412, "Precondition Failed"}).
-define(REQ_ENTITY_TOO_LARGE,      {413, "Request Entity Too Large"}).
-define(REQ_URI_TOO_LARGE,         {414, "Request-URI Too Large"}).
-define(UNSUPPORTED_MEDIA_TYPE,    {415, "Unsupported Media Type"}).
-define(REQ_RANGE_NOT_SATISFIABLE, {416, "Requested range not satisfiable"}).
-define(EXPECTATION_FAILED,        {417, "Expectation Failed"}).

%% 5xx: Server Error
-define(INTERNAL_SERVER_ERROR,      {500, "Internal Server Error"}).
-define(ERROR,                      {500, "Internal Server Error"}). % alias for internal_server_error
-define(NOT_IMPLEMENTED,            {501, "Not Implemented"}).
-define(BAD_GW,                     {502, "Bad Gateway"}).
-define(SERVICE_UNAVAILABLE,        {503, "Service Unavailable"}).
-define(GW_TIMEOUT,                 {504, "Gateway Time-out"}).
-define(HTTP_VERSION_NOT_SUPPORTED, {505, "HTTP Version not supported"}).

-define(STATUS_MSG(Status),
        integer_to_list(element(1, Status))
        ++ " "
        ++ element(2, Status)).
               
-define(DEFAULT_CHUNKSIZE, 4096).

-type ewgi_propval() :: atom() | integer() | string() | binary().
-type ewgi_prop() :: {ewgi_propval(), ewgi_propval()}.
-type ewgi_proplist() :: [ewgi_prop()].

%% These should go somewhere else:
-type gb_tree_node() :: {any(), any(), any(), any()} | 'nil'.
-type gb_tree() :: {non_neg_integer(), gb_tree_node()}.

%% @type bag() = gb_tree()
-type bag() :: gb_tree().

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | ewgi_ri_callback()).
%% @type ewgi_ri_callback() = function()
-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | function()) | iolist().

%% @type ewgi_read_input() = function()
-type ewgi_read_input() :: fun((ewgi_ri_callback(), integer()) -> ewgi_ri_callback()).

%% @type ewgi_write_error() = function()
-type ewgi_write_error() :: fun((any()) -> 'ok').

%% @type ewgi_version() = {integer(), integer()}
-type ewgi_version() :: {integer(), integer()}.

%% Convenience records representing request/response contexts

%% @type ewgi_spec() = {'ewgi_spec', function(), function(), string(),
%%                      ewgi_version(), bag()}
%% -type(ewgi_spec() :: {'ewgi_spec', ewgi_read_input(), ewgi_write_error(),
%%                       string(), ewgi_version(), bag()}).
-record(ewgi_spec, {
          read_input  :: ewgi_read_input(),
          write_error :: ewgi_write_error(),
          url_scheme  :: string(),
          version     :: ewgi_version(),
          data        :: bag()}).

%% @type ewgi_header_val() = string() | 'undefined'
-type ewgi_header_val() :: string() | 'undefined'.

%% @type ewgi_header_key() = string()
-type ewgi_header_key() :: string().

%% @type ewgi_http_headers() = {'ewgi_http_headers',
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              bag()}
-record(ewgi_http_headers, {
          http_accept                  :: ewgi_header_val(),
          http_cookie                  :: ewgi_header_val(),
          http_host                    :: ewgi_header_val(),
          http_if_modified_since       :: ewgi_header_val(),
          http_user_agent              :: ewgi_header_val(),
          http_x_http_method_override  :: ewgi_header_val(),
          other                        :: bag()}).

%% @type ewgi_request_method() = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%       'DELETE' | 'TRACE' | 'CONNECT' | string()
-type ewgi_request_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
                               'DELETE' | 'TRACE' | 'CONNECT' | string().

%% @type ewgi_val() = string() | 'undefined'
-type ewgi_val() :: string() | 'undefined'.

%% @type ewgi_request() :: {'ewgi_request', ewgi_val(), integer(), ewgi_val(),
%%                          ewgi_spec(), ewgi_val(), ewgi_http_headers(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_request_method(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val()}
-record(ewgi_request, {
          auth_type                           :: ewgi_val(),
          content_length                      :: non_neg_integer(),
          content_type                        :: ewgi_val(),
          ewgi         = #ewgi_spec{}         :: #ewgi_spec{},
          gateway_interface                   :: ewgi_val(),
          http_headers = #ewgi_http_headers{} :: #ewgi_http_headers{},
          path_info                           :: ewgi_val(),
          path_translated                     :: ewgi_val(),
          query_string                        :: ewgi_val(),
          remote_addr                         :: ewgi_val(),
          remote_host                         :: ewgi_val(),
          remote_ident                        :: ewgi_val(),
          remote_user                         :: ewgi_val(),
          remote_user_data                    :: ewgi_val(),
          request_method                      :: ewgi_request_method(),
          script_name                         :: ewgi_val(),
          server_name                         :: ewgi_val(),
          server_port                         :: ewgi_val(),
          server_protocol                     :: ewgi_val(),
          server_software                     :: ewgi_val()}).

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type stream() :: fun(() -> {} | {any(), stream()}).
%% @type stream() = function()
-type stream() :: fun(() -> {} | {any(), function()}).

%% @type ewgi_status() = {integer(), string()}
-type ewgi_status() :: {integer(), string()}.

%% @type ewgi_message_body() = binary() | iolist() | stream()
-type ewgi_message_body() :: binary() | iolist() | stream().

%% @type ewgi_header_list() = [{ewgi_header_key(), ewgi_header_val()}]
-type ewgi_header_list() :: [{ewgi_header_key(), ewgi_header_val()}].

%% @type ewgi_response() = {'ewgi_response', ewgi_status(),
%%                          [{ewgi_header_key(), ewgi_header_val()}],
%%                           ewgi_message_body(), any()}
-record(ewgi_response, {
          status       :: ewgi_status(),
          headers = [] :: ewgi_header_list(),
          message_body :: ewgi_message_body(),
          err          :: any()}).

%% @type ewgi_context() = {'ewgi_context', ewgi_request(), ewgi_response()}
-record(ewgi_context, {
          request  = #ewgi_request{}  :: #ewgi_request{},
          response = #ewgi_response{} :: #ewgi_response{}}).

%% @type ewgi_app() = function()
-type ewgi_app() :: fun((#ewgi_context{}) -> #ewgi_context{}).

-endif.
