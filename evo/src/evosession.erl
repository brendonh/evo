%%%-------------------------------------------------------------------
%%% File    : evosession.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Manage sessions, and login / logout
%%%
%%% Created : 18 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosession, []).

-export([respond/4]).

-include("evo.hrl").


%%%-------------------------------------------------------------------
%%% Login / logout Evo pages
%%%-------------------------------------------------------------------

-define(Template(C, T, D, Cf), 
        gen_server:call(?CONFNAME(Conf, "evotemplate"),
                        {C,T,D,Cf,State#state.templateCallback})).

-define(Q(S), lists:concat(['"', S, '"'])).


template(login) -> {file, "templates/auto/login.html"}.


respond(_Req, 'GET', ["login"], Conf) ->
    {ok, Content} = gen_server:call(?CONFNAME(Conf, "evotemplate"),
                                    {run_raw, {reload, login}, [], [], fun template/1}),
    {wrap, site, [{content, Content}, {title, "Login"}]};

respond(Req, 'POST', ["login"], Conf) ->

    CouchDB = ?GV(couchdb, Conf),
    Creds = mochiweb_multipart:parse_form(Req),
    Username = ?GV("username", Creds),

    {json, Users} = erlang_couchdb:invoke_view(CouchDB, "evo", "users", "byUsername", 
                                               [{include_docs, true}, {key, ?Q(Username)}]),
    {Valid, User} = case erlang_couchdb:get_value([<<"rows">>, <<"doc">>], Users) of
                        [] -> {false, none};
                        [{struct, U}] -> check_creds(U, Creds)
                    end,
    
    {Key, OldSession} = ?GV(session, Conf),
    NewSession = [{<<"userID">>, ?GV(<<"_id">>, User)}|OldSession],
    save_session([{session, {Key, NewSession}}|Conf]),
    
    {wrap, site, [{content, ["Valid: ", Valid] }, {title, "Hey"}]};

%% XXX Debug
respond(Req, 'GET', ["_clear"], Conf) ->
    CouchDB = ?GV(couchdb, Conf),
    {json, RawSessions} = erlang_couchdb:invoke_view(CouchDB, "evo", "users", "sessions", []),
    Sessions = [{?GV(<<"id">>, S), ?GV(<<"value">>, S)} || {struct, S} 
                <- erlang_couchdb:get_value(<<"rows">>, RawSessions)],
    Response = erlang_couchdb:delete_documents(CouchDB, "evo", Sessions),
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


%%%-------------------------------------------------------------------
%%% Sessions
%%%-------------------------------------------------------------------


session_from_cookie(Req, Conf, "/static/" ++ _) ->
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


save_session(Conf) ->
    {Key, RawSession} = ?GV(session, Conf),
    Session = lists:usort([{K,V} || {K,V} <- RawSession, is_binary(K)]),
    CouchDB = ?GV(couchdb, Conf),
    erlang_couchdb:update_document(CouchDB, "evo", binary_to_list(Key), Session).
