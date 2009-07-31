%%%-------------------------------------------------------------------
%%% File    : evosession.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Manage sessions, and login / logout
%%%
%%% Created : 18 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosession).

-include("evo.hrl").

%% Evo component callbacks
-export([respond/5, nav/2]).

%% API
-export([save_session/1, user_info/1, get_user/2, login/3]).

-define(Q(S), lists:concat(['"', S, '"'])).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

user_info(Conf) ->
    Session = ?GVD(session, Conf, []),
    RV = ?GVD(userInfo, Session#session.session, []),
    RV.


save_session(Conf) ->
    Session = ?GV(session, Conf),
    SessionPL = lists:usort([{K,V} || {K,V} <- Session#session.session, is_binary(K)]),
    mnesia:transaction(
      fun() -> mnesia:write(Session#session{session=SessionPL}) end).


nav(_Conf, _Args) -> [].



login(Username, GivenPass, Conf) ->
    {CouchDB, DB} = ?GV(couchdb, Conf),
    {json, Users} = erlang_couchdb:invoke_view(CouchDB, DB, "users", "byUsername", 
                                               [{include_docs, true}, {key, ?Q(Username)}]),
    {Valid, User} = case erlang_couchdb:get_value([<<"rows">>, <<"doc">>], Users) of
                        [] -> {false, none};
                        [{struct, U}] -> check_creds(U, GivenPass)
                    end,
    
    case Valid of
        true ->
            OldSession = ?GV(session, Conf),
            UserID = ?GV(<<"_id">>, User),
            NewSessionPL = [{<<"userID">>, UserID},
                          {userInfo, get_user(UserID, Conf)}
                          |OldSession#session.session],
            NewConf = [{session, OldSession#session{session=NewSessionPL}}|Conf],
            save_session(NewConf),
            {ok, NewConf};
        false ->
            {error, invalid}
    end.



%%%-------------------------------------------------------------------
%%% Login / logout Evo pages
%%%-------------------------------------------------------------------

-define(Template(C, T, D, Cf), 
        gen_server:call(?CONFNAME(Conf, "evotemplate"),
                        {C,T,D,Cf,State#state.templateCallback})).


template(login) -> {file, "templates/auto/login.html"};
template(loggedin) -> {file, "templates/auto/loggedin.html"}.


respond(Req, 'GET', [], Conf, _Args) ->
    {Template, Name} = case user_info(Conf) of
                           [] -> {login, ""};
                           User -> {loggedin, ?GV(<<"name">>, User)}
                       end,
    
    Data = [{error, ?GVD(error, Conf, "")},
            {name, Name}],

    {ok, Content} = gen_server:call(?CONFNAME(Conf, "evotemplate"),
                                    {run, {reload, Template}, Data, [], fun template/1}),


    {response, Req:ok({"text/html", Content})};


respond(Req, 'POST', [], Conf, Args) ->
    case user_info(Conf) of
        [] ->
            Creds = mochiweb_multipart:parse_form(Req),
            Username = ?GV("username", Creds),
            GivenPass = ?GV("password", Creds),

            case login(Username, GivenPass, Conf) of
                {ok, NewConf} -> redirect(Req, NewConf);
                {error, _} -> respond(Req, 'GET', [], [{error, "Invalid credentials"}|Conf], Args)
            end;
        _ -> redirect(Req, Conf)
    end;


respond(Req, 'GET', ["logout"], Conf, _Args) ->
    Session = ?GVD(session, Conf, []),

    NewSession = [{K,V} || {K,V} <- Session#session.session,
                           K /= <<"userID">>,
                           K /= userInfo],

    NewConf = [{session, Session#session{session=NewSession}}|Conf],

    save_session(NewConf),

    redirect(Req, NewConf);



respond(Req, _Method, always, Conf, _Args) ->
    {atomic, RV} = mnesia:transaction(
                     fun() -> session_from_cookie(Req, Conf, Req:get(path)) end),
    RV;


respond(Req, _, _, _Conf, _Args) ->
    {response, Req:not_found()}.

   
check_creds(User, GivenPass) ->
    {struct, UNPW} = ?GV(<<"unpw">>, User),
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


% XXX Todo
redirect(Req, Conf) ->
    {response, Req:respond({302, [{"Location", ?GVD(default, Conf, "/default")}], 
                            "Redirecting..."})}.



%%%-------------------------------------------------------------------
%%% Sessions
%%%-------------------------------------------------------------------


session_from_cookie(_Req, Conf, "/static/" ++ _) ->
    %% Don't bother hitting the DB for static files
    {update, Conf};
session_from_cookie(Req, Conf, _) ->
    CookieName = lists:concat(["evo_session_", ?SITENAME(Conf)]),

    Session = case Req:get_cookie_value(CookieName) of
                  undefined -> create_session();
                  Existing -> retrieve_session(Existing)
              end,

    UserID = ?GV(<<"userID">>, Session#session.session),
    
    SessionPL = case UserID of
                    undefined -> Session#session.session;
                    _ -> [{userInfo, get_user(UserID, Conf)}|Session#session.session]
                end,

    OldHeaders = ?GVD(headers, Conf, []),
    NewHeader = mochiweb_cookies:cookie(CookieName, Session#session.id, [{path, "/"}]),

    {update, [{session, Session#session{session=SessionPL}},
              {headers, [NewHeader|OldHeaders]}
              |Conf]}.


get_user(UserID, Conf) ->
    {CouchDB, DB} = ?GV(couchdb, Conf),
    {json, {struct, User}} = erlang_couchdb:retrieve_document(
        CouchDB, DB, binary_to_list(UserID)),
    User.


create_session() ->
    Key = uuid:to_string(uuid:srandom()),
    Session = #session{id=Key, session=[]},
    mnesia:write(Session),
    Session.


retrieve_session(Key) ->
    Sessions = mnesia:read(session, Key),
    case Sessions of
        [Session] -> Session;
        [] -> create_session();
        Other -> throw({'EXIT', multiple_session_matches, Other})
    end.
