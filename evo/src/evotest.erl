-module(evotest).

-export([test/0]).

test_case(Name, Input, Data, Output) ->
    test_case(Name, Input, Data, Output, true).

test_case(Name, Input, Data, Output, Pretty) ->
    Result = evo:run(atom_to_list(Input), Data, Pretty),
    report_success(Name, Result, Output).
        

report_success(Name, Result, Output) ->
    StringOutput = atom_to_list(Output),
    case Result of
        StringOutput ->
            io:format("~.20s [OK]~n", [Name]);
        Other ->
            io:format("~.20s [FAIL]~n", [Name]),
            io:format("::~p:: != ::~p::~n~n", [list_to_atom(Other), Output])
    end.

test() ->
    T = fun test_case/4,
    F = fun(N,I,D,O) -> test_case(N,I,D,O,false) end,

    StartTime = now(),


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
    T("ForeachData", '<div e:render="foreach"><e:slot /></div>', [1,2,3], '<div>123</div>'),
    F("ForeachNest", 
      '<div e:render="foreach"><div e:render="foreach"><e:slot /></div></div>', 
      [[1,2,3], [4,5,6]], 
      '<div><div>123</div><div>456</div></div>'),
    T("ForeachRowIndex", '<div e:render="foreach"><e:slot e:dataExp="{R,D}" /></div>', [a,b,c,d], '<div>{0,a}{1,b}{2,c}{3,d}</div>'),
    T("ForeachOddEven", '<div e:render="foreach"><e:slot e:dataExp="OddEven" />,</div>', [a,b,c,d], '<div>odd,even,odd,even,</div>'),
    F("ForeachClass", '<ul e:render="foreach"><li><e:attr name="class" e:render="data" e:dataExp="OddEven" /><e:slot /></li></ul>', 
      [a,b,c,d], '<ul><li class="odd">a</li><li class="even">b</li><li class="odd">c</li><li class="even">d</li></ul>'),


    %% Shortcuts

    Data = [{one, hello}, {two, world}],
    CompoundData = [{alpha, Data}],

    T("DataKey", '<e:inv e:render="data" e:key="two" />', Data, 'world'),
    T("CompounKey", '<e:inv e:render="data" e:key="alpha.one" />', CompoundData, 'hello'),
    T("Slot", '<e:slot />', "Data", 'Data'),
    T("KeySlot", '<e:slot key="one" />', Data, 'hello'),
    T("CompoundKeySlot", '<e:slot key="alpha.two" />', CompoundData, 'world'),
    T("Items", '<e:inv e:render="items"><e:slot /></e:inv>', 
      Data, '[{key,one},{value,hello}][{key,two},{value,world}]'),
    T("KeyTag", '<e:inv e:render="items"><e:key /></e:inv>',
      Data, 'onetwo'),
    T("ValueTag", '<e:inv e:render="items"><e:value /></e:inv>',
      Data, 'helloworld'),


    %% Repeated runs
    
    Content = atom_to_list(
                '<ul e:render="foreach"><li e:dataExp="[{D, D*2}]" e:render="items"><e:key />::<e:value /></li></ul>'),
  
    Template = spawn_link(fun() -> evo:prepare(Content) end),

    run_template(Template, [1,2], "RepeatedRunOne", 
                 '<ul><li>1::2</li><li>2::4</li></ul>'),
    run_template(Template, [4,5,6], "RepeatedRunTwo", 
                 '<ul><li>4::8</li><li>5::10</li><li>6::12</li></ul>'),
    run_template(Template, [], "RepeatedRunThree", '<ul />'),

    Template ! finished,

    EndTime = now(),

    io:format("Total time: ~.10Bms~n", [round(timer:now_diff(EndTime, StartTime) / 1000)]),

    ok.


run_template(Template, Data, Name, Output) ->
    Self = self(),
    Template ! {run, Data, false, Self},

    receive
        {Template, result, R} ->
            report_success(Name, R, Output);
        {Template, 'EXIT', _, Error} ->
            io:format("Error: ~p~n", [Error])
    after 1000 ->
            io:format("Template dead~n")
    end.
