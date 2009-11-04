%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Post example

-module(ewgi_post).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([run/2]).
-export([post_app_example/1]).

-define(DEFAULT_MAX_LENGTH, 2097152). %% 2 MB maximum

run(Ctx, [NoPostApp, FailedPostApp, SuccessPostApp]) ->
    run(Ctx, [NoPostApp, FailedPostApp, SuccessPostApp, ?DEFAULT_MAX_LENGTH]);
run(Ctx, [NoPostApp, FailedPostApp, SuccessPostApp, MaxLength]) ->
    Parser = post_parse_middleware(MaxLength, SuccessPostApp, FailedPostApp),
    case ewgi_api:request_method(Ctx) of
	'GET' ->
	    NoPostApp(Ctx);
	'POST' ->
	    Parser(Ctx)
    end.

%% MaxLength is the maximum size (in bytes) that the server will
%% receive from the client.  App should be the application called when
%% the parse is successful (or unnecessary).  ErrApp should be an
%% error application when the content length exceeds the maximum
%% specified limit.
post_parse_middleware(MaxLength, App, ErrApp)
  when is_integer(MaxLength), MaxLength > 0, is_function(App, 1) ->
    fun(Ctx) ->
            case ewgi_api:request_method(Ctx) of
                Method when Method =:= 'POST';
                            Method =:= 'PUT' ->
                    case ewgi_api:remote_user_data(Ctx) of
                        undefined ->
                            %% Check content-type first
                            Ct = ewgi_api:content_type(Ctx),
                            parse_post(Ctx, App, ErrApp, parse_ct(Ct), MaxLength);
                        _ ->
                            App(Ctx)
                    end;
                _ ->
                    App(Ctx)
            end
    end.

%% Parse content-type (ignoring additional vars for now)
%% Should look like "major/minor; var=val"
parse_ct(L) when is_list(L) ->
    case string:tokens(L, ";") of
        [CT|Vars] ->
	    Vars1 = [string:tokens(VarStr, "=") || VarStr <- Vars],
	    Vars2 = [{string:strip(Name), Value} || [Name, Value] <- Vars1],
            {CT, Vars2};
        _ ->
            undefined
    end.

parse_post(Ctx, App, ErrApp, {"application/x-www-form-urlencoded", Vars}, Max) ->
    case ewgi_api:content_length(Ctx) of
        L when is_integer(L), L > Max ->
	    %% shouldn't we set an error message here?
            ErrApp(Ctx);
        L when is_integer(L), L > 0 ->
            Input = read_input_string(Ctx, L),
	    %% http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html
	    %% When no explicit charset parameter is provided by the sender,
	    %% media subtypes of the "text" type are defined to have a default
	    %% charset value of "ISO-8859-1" when received via HTTP.
	    case proplists:get_value("charset", Vars) of
		undefined -> InCharset = "iso-8859-1"
				 ;Charset -> InCharset = string:to_lower(Charset)
	    end,
	    UnicodeInput = to_unicode(Input, InCharset),
            Vals = ewgi_api:parse_post(UnicodeInput),
            Ctx1 = ewgi_api:remote_user_data(Vals, Ctx),
            App(Ctx1);
        _ ->
            ErrApp(Ctx)
    end;
parse_post(Ctx, App, ErrApp, {"application/json", _Vars}, Max) ->
    case ewgi_api:content_length(Ctx) of
        L when is_integer(L), L > Max ->
	    %% shouldn't we set an error message here?
            ErrApp(Ctx);
        L when is_integer(L), L > 0 ->
	    Input = read_input_string(Ctx, L),
	    %% http://www.ietf.org/rfc/rfc4627.txt
	    %% JSON text SHALL be encoded in Unicode.
	    %% The default encoding is UTF-8.
	    case unicode:bom_to_encoding(Input) of
		{latin1,0} -> InEncoding = utf8
				  ;{InEncoding, _Length} -> ok
	    end,
	    UnicodeInput = unicode:characters_to_list(Input, InEncoding),
            {Json, [], _} = ktj_decode:decode(UnicodeInput),
	    Vals = [{"json", Json}],
            Ctx1 = ewgi_api:remote_user_data(Vals, Ctx),
            App(Ctx1);
        _ ->
            ErrApp(Ctx)
    end;
parse_post(Ctx, App, _, _, _) ->
    %% Silently ignore other content-types
    App(Ctx).

read_input_string(Ctx, L) when is_integer(L), L > 0 ->
    R = ewgi_api:read_input(Ctx),
    iolist_to_binary(R(read_input_string_cb([]), L)).

read_input_string_cb(Acc) ->
    fun(eof) ->
            lists:reverse(Acc);
       ({data, B}) ->
            read_input_string_cb([B|Acc])
    end.

%% Transforms the data from the given charset to unicode
%% Todo: add support for other charset as needed.
to_unicode(Data, "iso-8859-1") ->
    unicode:characters_to_list(Data, latin1);
to_unicode(Data, "utf8") ->
    unicode:characters_to_list(Data, utf8);
to_unicode(Data, "utf-8") ->
    unicode:characters_to_list(Data, utf8).

%% 
%% example functions on how to use the post handling middleware
%%
post_app_example(Ctx) ->
    run(Ctx, [fun display_form/1,
	      fun post_app_error/1,
	      fun display_form_data/1]).

post_app_error({ewgi_context, Request, _}) ->
    Response = {ewgi_response, {400, "BAD REQUEST"}, [],
                [<<"Maximum content-length exceeded.">>],
                undefined},
    {ewgi_context, Request, Response}.

display_form_data({ewgi_context, Request, _Response}=Ctx) ->
    Body = 
	case ewgi_api:remote_user_data(Ctx) of
	    undefined ->
		"undefined";
	    Body1 ->
		io_lib:format("~p", [Body1])
	end,
    ResponseHeaders = [{"Content-type", "text/html; charset=utf8"}],
    Response = {ewgi_response, 
                {200, "OK"}, 
                ResponseHeaders,
                [Body], undefined},
    {ewgi_context, Request, Response}.

display_form({ewgi_context, Request, _Response}) ->
    Body = <<"<form action=\"/postex\" method=\"post\">
Un: <input type=\"text\" name=\"un\" value=\"\"/>
<br/>
	    Pw: <input type=\"text\" name=\"pw\" value=\"\"/>
<br/><br/>
	    <input type=\"submit\" name=\"submit\" value=\"Login\"/>
</form>">>,
    ResponseHeaders = [{"Content-type", "text/html"}],
	    Response = {ewgi_response, 
			{200, "OK"}, 
			ResponseHeaders,
			[Body], undefined},
	    {ewgi_context, Request, Response}.
