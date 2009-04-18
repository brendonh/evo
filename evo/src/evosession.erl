%%%-------------------------------------------------------------------
%%% File    : evosession.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Manage sessions, and login / logout
%%%
%%% Created : 18 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosession, []).

-export([respond/4, save_session/1, user_info/1]).

-include("evo.hrl").


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

user_info(Conf) ->
    {_, Session} = ?GVD(session, Conf, []),
    ?GVD(userInfo, Session, []).


save_session(Conf) ->
    {Key, RawSession} = ?GV(session, Conf),
    Session = lists:usort([{K,V} || {K,V} <- RawSession, is_binary(K)]),
    CouchDB = ?GV(couchdb, Conf),
    erlang_couchdb:update_document(CouchDB, "evo", binary_to_list(Key), Session).




%%%-------------------------------------------------------------------
%%% Login / logout Evo pages
%%%-------------------------------------------------------------------

-define(Template(C, T, D, Cf), 
        gen_server:call(?CONFNAME(Conf, "evotemplate"),
                        {C,T,D,Cf,State#state.templateCallback})).

-define(Q(S), lists:concat(['"', S, '"'])).


template(login) -> {file, "templates/auto/login.html"}.


respond(Req, 'GET', ["login"], Conf) ->
    case user_info(Conf) of
        [] ->
            {ok, Content} = gen_server:call(?CONFNAME(Conf, "evotemplate"),
                                            {run_raw, {reload, login}, Conf, [], fun template/1}),
            {wrap, site, [{content, Content}, {title, "Login"}]};
        _ -> redirect(Req, Conf)
    end;

respond(Req, 'POST', ["login"], Conf) ->
    case user_info(Conf) of
        [] ->
            CouchDB = ?GV(couchdb, Conf),
            Creds = mochiweb_multipart:parse_form(Req),
            Username = ?GV("username", Creds),

            {json, Users} = erlang_couchdb:invoke_view(CouchDB, "evo", "users", "byUsername", 
                                                       [{include_docs, true}, {key, ?Q(Username)}]),
            {Valid, User} = case erlang_couchdb:get_value([<<"rows">>, <<"doc">>], Users) of
                                [] -> {false, none};
                                [{struct, U}] -> check_creds(U, Creds)
                            end,
    
            case Valid of
                true ->
                    {Key, OldSession} = ?GV(session, Conf),
                    UserID = ?GV(<<"_id">>, User),
                    NewSession = [{<<"userID">>, UserID},
                                  {userInfo, get_user(UserID, Conf)}
                                  |OldSession],
                    NewConf = [{session, {Key, NewSession}}|Conf],
                    save_session(NewConf),
                    redirect(Req, NewConf);
                false ->
                    respond(Req, 'GET', ["login"], [{error, "Invalid credentials"}|Conf])
            end;
        _ -> redirect(Req, Conf)
    end;


respond(Req, 'GET', ["logout"], Conf) ->
    {Key, Session} = ?GVD(session, Conf, []),

    NewSession = [{K,V} || {K,V} <- Session,
                           K /= <<"userID">>,
                           K /= userInfo],

    NewConf = [{session, {Key, NewSession}}|Conf],

    save_session(NewConf),

    redirect(Req, NewConf);


%% XXX Debug
respond(_Req, 'GET', ["_clear"], Conf) ->
    CouchDB = ?GV(couchdb, Conf),
    {json, RawSessions} = erlang_couchdb:invoke_view(CouchDB, "evo", "users", "sessions", []),
    Sessions = [{?GV(<<"id">>, S), ?GV(<<"value">>, S)} || {struct, S} 
                <- erlang_couchdb:get_value(<<"rows">>, RawSessions)],
    erlang_couchdb:delete_documents(CouchDB, "evo", Sessions),
    {wrap, site, [{content, ["Alright."] }, {title, "Hey"}]};


respond(Req, _Method, [], Conf) ->
    session_from_cookie(Req, Conf, Req:get(path)).

   
check_creds(User, Creds) ->
    {struct, UNPW} = ?GV(<<"unpw">>, User),
    GivenPass = ?GV("password", Creds),
    RealPass = ?GV(<<"password">>, UNPW),
    HashFunc = hashfunc(?GVD(<<"hash">>, UNPW, <<"plaintext">>)),
    case list_to_binary(HashFunc(GivenPass)) == RealPass of
        true -> {true, User};
        false -> {false, none}
    end.


hashfunc(<<"plaintext">>) -> fun(P) -> P end;
hashfunc(<<"md5">>) -> fun evoutil:md5_hex/1;
hashfunc(<<"sha224">>) -> fun sha2:hexdigest224/1;
hashfunc(<<"sha256">>) -> fun sha2:hexdigest224/1;
hashfunc(<<"sha384">>) -> fun sha2:hexdigest224/1;
hashfunc(<<"sha512">>) -> fun sha2:hexdigest224/1.


redirect(_Req, Conf) ->
    {wrap, site, [{content, ["User: ", ?GV(<<"name">>, user_info(Conf))] }, {title, "Hey"}]}.



%%%-------------------------------------------------------------------
%%% Sessions
%%%-------------------------------------------------------------------


session_from_cookie(_Req, Conf, "/static/" ++ _) ->
    %% Don't bother hitting CouchDB for static files
    {update, Conf};
session_from_cookie(Req, Conf, _) ->
    CookieName = lists:concat(["evo_session_", ?SITENAME(Conf)]),
    {Key, DBSession} = case Req:get_cookie_value(CookieName) of
                           undefined -> create_session(Conf);
                           Existing -> retrieve_session(Existing, Conf)
                       end,

    UserID = ?GV(<<"userID">>, DBSession),
    
    Session = case UserID of
                  undefined -> DBSession;
                  _ -> [{userInfo, get_user(UserID, Conf)}|DBSession]
              end,

    OldHeaders = ?GVD(headers, Conf, []),
    NewHeader = mochiweb_cookies:cookie(CookieName, Key, [{path, "/"}]),

    {update, [{session, {Key, Session}},
              {headers, [NewHeader|OldHeaders]}
              |Conf]}.


get_user(UserID, Conf) ->
    CouchDB = ?GV(couchdb, Conf),
    {json, {struct, User}} = erlang_couchdb:retrieve_document(
        CouchDB, "evo", binary_to_list(UserID)),
    User.


create_session(Conf) ->
    CouchDB = ?GV(couchdb, Conf),
    {json, {struct, Response}} = erlang_couchdb:create_document(
        CouchDB, "evo", [{<<"type">>, <<"session">>}]),
    {?GV(<<"id">>, Response), []}.


retrieve_session(Name, Conf) ->
    CouchDB = ?GV(couchdb, Conf),
    {json, {struct, Session}} = erlang_couchdb:retrieve_document(CouchDB, "evo", Name),
    Key = ?GVD(<<"_id">>, Session, undefined),
    case Key of
        undefined -> create_session(Conf);
        _ -> {Key, Session}
    end.
