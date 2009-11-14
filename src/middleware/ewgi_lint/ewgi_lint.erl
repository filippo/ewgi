-module(ewgi_lint).

-export([run/2]).

%% ewgi records definitions
%% to be removed after end of code refactoring
-record(ewgi_spec, {
          read_input,
          write_error,
          url_scheme,
          version,
          data % gb_tree()
         }).

-record(ewgi_http_headers, {
          http_accept,
          http_cookie,
          http_host,
          http_if_modified_since,
          http_user_agent,
          http_x_http_method_override,
          other % multiset
         }).

-record(ewgi_request, {
          auth_type,
          content_length,
          content_type,
          ewgi=#ewgi_spec{},
          gateway_interface,
          http_headers=#ewgi_http_headers{},
          path_info,
          path_translated,
          query_string,
          remote_addr,
          remote_host,
          remote_ident,
          remote_user,
          remote_user_data,
          request_method,
          script_name,
          server_name,
          server_port,
          server_protocol,
          server_software
         }).

-record(ewgi_response, {
          status={200, "OK"},
          headers=[],
          message_body,
          err
         }).

-record(ewgi_context, {
          request,
          response
         }).
%% end ewgi records definitions

%% add error message to response message body
report_error(Msg, Ctx) ->
    Body = ewgi_api:response_message_body(Ctx),
    ewgi_api:response_message_body([io_lib:format("~s~n", [Msg])|Body], Ctx).

%%
%% Check ewgi spec record
%%
%% read input callback
check_read_input(ReadInput, Ctx) when is_function(ReadInput, 2) ->
    Ctx;
check_read_input(_ReadInput, Ctx) ->
    report_error("read_input is not a fun/2", Ctx).

%% write error callback
check_write_error(WriteError, Ctx) when is_function(WriteError, 1) ->
    Ctx;
check_write_error(_WriteError, Ctx) ->
    report_error("read_input is not a fun/1", Ctx).

%% url scheme
check_url_scheme("http", Ctx) ->
    Ctx;
check_url_scheme("https", Ctx) ->
    Ctx;
check_url_scheme(_, Ctx) ->
    report_error("url_scheme has to be either \"http\" or \"https\"", Ctx).

%% spec version number
check_version({1, 0}, Ctx) ->
    Ctx;
check_version(_, Ctx) ->
    report_error("version number has to be {1, 0}", Ctx).

%% data field
check_data(_Data, Ctx) ->
    %% FIXME: no way to check if it's a gb_tree?
    Ctx.

%% check ewgi spec
check_ewgi_spec(#ewgi_context{request=Req}=Ctx) when is_record(Ctx, ewgi_context), is_record(Req, ewgi_request) ->
    lists:foldl(fun check/2, Ctx, [read_input, write_error, url_scheme, version, data]).

%%
%% Check ewgi request record
%%
%% check auth type
check_auth_type(_, Ctx) ->
    %% FIXME: in mochiweb and inets this is undefined. Check what happens in yaws
    Ctx.

%% check content length
check_content_lenght(ContentLenght, Ctx) when is_integer(ContentLenght) ->
    Ctx;
check_content_lenght(_ContentLenght, Ctx) ->
    report_error("content length has to be a number", Ctx).

%% check content type
check_content_type(ContentType, Ctx) when is_list(ContentType) ->
    Ctx;
check_content_type(_ContentType, Ctx) ->
    report_error("content type has to be a list", Ctx).

%% check gateway interface
check_gateway_interface("EWGI/1.0", Ctx) ->
    Ctx;
check_gateway_interface(_, Ctx) ->
    report_error("gateway interface has to be \"EWGI/1.0\"", Ctx).

%% FIXME: check http headers

%% check path info
check_path_info(PathInfo, Ctx) when is_list(PathInfo) ->
    Ctx;
check_path_info(_, Ctx) ->
    report_error("path_info has to be a list", Ctx).

%% check path translated
check_path_translated(undefined, Ctx) ->
    Ctx;
check_path_translated(PathTranslated, Ctx) when is_list(PathTranslated) ->
    Ctx;
check_path_translated(_, Ctx) ->
    report_error("path_translated has to be a list or the atom undefined", Ctx).

%% check query string
check_query_string(QS, Ctx) when is_list(QS) ->
    Ctx;
check_query_string(_, Ctx) ->
    report_error("query_string has to be a list", Ctx).

%% check remote address
%% FIXME: yaws supports only ipv4?
check_ip_chunk(IPChunk, Ctx) ->
    IPChunk1 = list_to_integer(IPChunk),
    case IPChunk1 of
        X when X < 0   -> report_error("invalid remote_addr", Ctx);
        X when X > 255 -> report_error("invalid remote_addr", Ctx);
        _              -> Ctx
    end.
            
check_remote_addr(IP, Ctx) when is_list(IP) ->
    IPChunks = string:tokens(IP),
    lists:foldl(fun check_ip_chunk/2, Ctx, IPChunks);
check_remote_addr(_IP, Ctx) ->
    report_error("remote address has to be a list", Ctx).

%% check remote ident
check_remote_ident(undefined, Ctx) ->
    Ctx;
check_remote_ident(Id, Ctx) when is_list(Id) ->
    Ctx;
check_remote_ident(_Id, Ctx) ->
    report_error("remote ident has to be either a list or 'undefined'", Ctx).

%% check remote user
check_remote_user(undefined, Ctx) ->
    Ctx;
check_remote_user(User, Ctx) when is_list(User) ->
    Ctx;
check_remote_user(_User, Ctx) ->
    report_error("remote user has to be either a list or 'undefined'", Ctx).

%% check request method
%% FIXME: check specs fo rall available methods
check_request_method("GET", Ctx) ->
    Ctx;
check_request_method("POST", Ctx) ->
    Ctx;
check_request_method("PUT", Ctx) ->
    Ctx;
check_request_method("DELETE", Ctx) ->
    Ctx;
check_request_method("HEAD", Ctx) ->
    Ctx;
check_request_method("INFO", Ctx) ->
    Ctx;
check_request_method("OPTIONS", Ctx) ->
    Ctx;
check_request_method(_Method, Ctx) ->
    report_error("invalid request method", Ctx).

%% check script name
check_script_name(Script, Ctx) when is_list(Script) ->
    Ctx;
check_script_name(_Script, Ctx) ->
    report_error("script_name has to be a list", Ctx).

%% check server name
check_server_name(Server, Ctx) when is_list(Server) ->
    Ctx;
check_server_name(_Server, Ctx) ->
    report_error("server_name has to be a list", Ctx).

%% check server port
check_server_port(Port, Ctx) when is_list(Port) ->
    try list_to_integer(Port) of
        _ -> Ctx
    catch
        _Err ->
            report_error("invalid server_port", Ctx)
    end;
check_server_port(_, Ctx) ->
    report_error("server_port has to be a list", Ctx).
%% check server protocol
check_server_protocol("HTTP/1.1", Ctx) ->
    Ctx;
check_server_protocol("HTTP/1.0", Ctx) ->
    Ctx;
check_server_protocol("HTTP/0.9", Ctx) ->
    Ctx;
check_server_protocol(_Proto, Ctx) ->
    report_error("invalid server protocol", Ctx).

%% check server software
check_server_software(Server, Ctx) when is_list(Server) ->
    Ctx;
check_server_software(_Server, Ctx) ->
    report_error("server_software has to be a list", Ctx).

%%
%% checks
%%
%% ewgi_spec record check
check(ewgi, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    case is_record(EwgiSpec, ewgi_spec) of
        true ->
            check_ewgi_spec(Ctx);
        _ ->
            report_error("invalid ewgi spec record", Ctx)
    end;
check(read_input, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    ReadInput = EwgiSpec#ewgi_spec.read_input,
    check_read_input(ReadInput, Ctx);
check(write_error, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    WriteError = EwgiSpec#ewgi_spec.write_error,
    check_write_error(WriteError, Ctx);
check(url_scheme, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    UrlScheme = EwgiSpec#ewgi_spec.url_scheme,
    check_url_scheme(UrlScheme, Ctx);
check(version, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    Vsn = EwgiSpec#ewgi_spec.version,
    check_version(Vsn, Ctx);
check(data, #ewgi_context{request=Req}=Ctx) ->
    EwgiSpec = Req#ewgi_request.ewgi,
    Data = EwgiSpec#ewgi_spec.data,
    check_data(Data, Ctx);
%% request record check
check(auth_type, #ewgi_context{request=Req}=Ctx) ->
    check_auth_type(Req#ewgi_request.auth_type, Ctx);
check(content_length, #ewgi_context{request=Req}=Ctx) ->
    check_content_lenght(Req#ewgi_request.content_length, Ctx);
check(content_type, #ewgi_context{request=Req}=Ctx) ->
    check_content_type(Req#ewgi_request.content_type, Ctx);
check(gateway_interface, #ewgi_context{request=Req}=Ctx) ->
    check_gateway_interface(Req#ewgi_request.gateway_interface, Ctx);
check(path_info, #ewgi_context{request=Req}=Ctx) ->
    check_path_info(Req#ewgi_request.path_info, Ctx);
check(path_translated, #ewgi_context{request=Req}=Ctx) ->
    check_path_translated(Req#ewgi_request.path_translated, Ctx);
check(query_string, #ewgi_context{request=Req}=Ctx) ->
    check_query_string(Req#ewgi_request.query_string, Ctx);
check(remote_addr, #ewgi_context{request=Req}=Ctx) ->
    check_remote_addr(Req#ewgi_request.remote_addr, Ctx);
check(remote_ident, #ewgi_context{request=Req}=Ctx) ->
    check_remote_ident(Req#ewgi_request.remote_ident, Ctx);
check(remote_user, #ewgi_context{request=Req}=Ctx) ->
    check_remote_user(Req#ewgi_request.remote_user, Ctx);
check(request_method, #ewgi_context{request=Req}=Ctx) ->
    check_request_method(Req#ewgi_request.request_method, Ctx);
check(script_name, #ewgi_context{request=Req}=Ctx) ->
    check_script_name(Req#ewgi_request.script_name, Ctx);
check(server_name, #ewgi_context{request=Req}=Ctx) ->
    check_server_name(Req#ewgi_request.server_name, Ctx);
check(server_port, #ewgi_context{request=Req}=Ctx) ->
    check_server_port(Req#ewgi_request.server_port, Ctx);
check(server_protocol, #ewgi_context{request=Req}=Ctx) ->
    check_server_protocol(Req#ewgi_request.server_protocol, Ctx);
check(server_software, #ewgi_context{request=Req}=Ctx) ->
    check_server_software(Req#ewgi_request.server_software, Ctx).

%%
%% Check request
%%
check_request(Req, Ctx) when is_record(Req, ewgi_request) ->
    lists:foldl(fun check/2, Ctx, [auth_type, content_length, content_type, ewgi, 
                                   gateway_interface, path_info, path_translated,
                                   query_string, remote_addr, request_host,
                                   remote_ident, remote_user, remote_user_data]).

%%
%% Check response
%%
check_response(Resp, Ctx) when is_record(Resp, ewgi_response) ->
    Ctx.

%%
%% Check ewgi context
%%
check_context(Ctx) when is_record(Ctx, ewgi_context) ->
    Req  = Ctx#ewgi_context.request,
    Resp = Ctx#ewgi_context.response,
    Ctx1 = check_request(Req, Ctx),
    check_response(Resp, Ctx1).

run(Ctx, []) ->
    check_context(Ctx).
