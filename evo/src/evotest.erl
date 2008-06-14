-module(evotest).

-export([test/0]).

test_case(Name, Input, Data, Output) ->
    test_case(Name, Input, Data, Output, true).

test_case(Name, Input, Data, Output, Pretty) ->
    StringOutput = atom_to_list(Output),
    case evo:run(atom_to_list(Input), Data, ?MODULE, Pretty) of
        StringOutput ->
            io:format("~.20s [OK]~n", [Name]);
        Other ->
            io:format("~.20s [FAIL]~n", [Name]),
            io:format("::~p:: != ::~p::~n~n", [list_to_atom(Other), Output])
    end.

test() ->
    T = fun test_case/4,
    F = fun(N,I,D,O) -> test_case(N,I,D,O,false) end,


    %% Basics

    T("EmptyTop", '<div />', [], '<div />'),
    T("TextTop", '<div>Hi</div>', [], '<div>Hi</div>'),
    T("PrefixSpace", '<div> Hi</div>', [], '<div> Hi</div>'),
    T("SuffixSpace", '<div>Hi </div>', [], '<div>Hi </div>'),
    T("NoSpaces", '<div>Hi</div>', [], '<div>Hi</div>'),

    
    % Invisible tags

    T("RemoveInvTop", '<e:inv>Hi</e:inv>', [], 'Hi'),
    T("RemoveInv", '<div><e:inv>Hi</e:inv></div>', [], '<div>Hi</div>'),


    %% Data rendering

    T("DataRender", '<div e:render="data" />', "content", '<div>content</div>'),
    T("DataRenderAppend", '<div e:render="data">one</div>', "two", '<div>onetwo</div>'),
    T("DataRenderSpace", '<div e:render="data">one </div>', "two", '<div>one two</div>'),
    

    %% Type rendering

    T("IntRender", '<e:inv e:render="data" />', 123, '123'),
    T("AtomRender", '<e:inv e:render="data" />', atom, 'atom'),
    % XXX - TODO T("FloatRender", '<e:inv e:render="data" />', 1.23, '1.23'),
    T("ListRender", '<e:inv e:render="data" />', ["a", "b", "c"], '["a","b","c"]'),
    T("TupleRender", '<e:inv e:render="data" />', {one, two, three}, '{one,two,three}'),


    %% Data attributes

    T("DataReplace", '<div e:render="data" e:data="new" />', "old", '<div>new</div>'),
    T("DataClear", '<div e:render="data" e:data="" />', "old", '<div />'),
    F("DataInherit", '<div e:data="new"><div e:render="data" /></div>', ["old"], '<div><div>new</div></div>'),
    F("DataInherit2", 
      '<div e:render="data"><e:inv e:data="new"><div e:render="data" /></e:inv></div>', 
      "old", '<div><div>new</div>old</div>'),


    %% Data expressions

    T("DataExpMath", '<div e:render="data" e:dataExp="4 * 5" />', [], '<div>20</div>'),
    T("DataExpStrings", '<div e:render="data" e:dataExp="string:strip(S(\'ohello\'), left, $o)" />', [], '<div>hello</div>'),
    T("DataExpData", '<div e:render="data" e:dataExp="D ++ D" />', "bang", '<div>bangbang</div>'),


    %% Attribute tags

    T("AttrLiteral", '<div><e:attr name="key">Val</e:attr></div>', [], '<div key="Val" />'),
    T("AttrRender", '<div><e:attr name="key" e:render="data" /></div>', "Data", '<div key="Data" />'),
    T("AttrBoth", '<div><e:attr name="key" e:render="data">One</e:attr></div>', "Two", '<div key="OneTwo" />'),
    T("AttrInv", '<div><e:attr name="key"><e:inv e:render="data" /></e:attr></div>', "Val", '<div key="Val" />'),


    %% Foreach

    T("ForeachLit", '<div e:render="foreach">*</div>', [1,2,3], '<div>***</div>'),
    T("ForeachData", '<div e:render="foreach"><e:inv e:render="data" /></div>', [1,2,3], '<div>123</div>'),
    F("ForeachNest", 
      '<div e:render="foreach"><div e:render="foreach"><e:inv e:render="data" /></div></div>', 
      [[1,2,3], [4,5,6]], 
      '<div><div>123</div><div>456</div></div>'),


    %% Shortcuts

    Data = [{one, hello}, {two, world}],
    T("DataKey", '<e:inv e:render="data" e:key="two" />', Data, 'world'),
    T("Slot", '<e:slot name="two" />', Data, 'world'),
    T("DataTag", '<e:data />', "Data", 'Data'),
    T("Items", '<e:inv e:render="items"><e:data /></e:inv>', 
      Data, '[{key,one},{value,hello}][{key,two},{value,world}]'),
    T("KeyTag", '<e:inv e:render="items"><e:key /></e:inv>',
      Data, 'onetwo'),
    T("ValueTag", '<e:inv e:render="items"><e:value /></e:inv>',
      Data, 'helloworld'),

    ok.
