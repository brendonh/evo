-module(evotest).

-export([test/0]).

test_case(Name, Input, Data, Output) ->
    test_case(Name, Input, Data, Output, true).

test_case(Name, Input, Data, Output, Pretty) ->
    case evo:run(Input, Data, ?MODULE, Pretty) of
        Output ->
            io:format("~.20s [OK]~n", [Name]);
        Other ->
            io:format("~.20s [FAIL]~n", [Name]),
            io:format("::~p:: != ::~p::~n~n", [Other, Output])
    end.

test() ->
    T = fun test_case/4,
    F = fun(N,I,D,O) -> test_case(N,I,D,O,false) end,
    T("EmptyTop", "<div />", [], "<div />"),
    T("TextTop", "<div>Hi</div>", [], "<div>Hi</div>"),
    T("PrefixSpace", "<div> Hi</div>", [], "<div> Hi</div>"),
    T("SuffixSpace", "<div>Hi </div>", [], "<div>Hi </div>"),
    T("NoSpaces", "<div>Hi</div>", [], "<div>Hi</div>"),

    T("RemoveInvTop", "<e:inv>Hi</e:inv>", [], "Hi"),
    T("RemoveInv", "<div><e:inv>Hi</e:inv></div>", [], "<div>Hi</div>"),

    T("DataRender", "<div e:render=\"data\" />", ["content"], "<div>content</div>"),
    T("DataRenderAppend", "<div e:render=\"data\">one</div>", ["two"], "<div>onetwo</div>"),
    T("DataRenderSpace", "<div e:render=\"data\">one </div>", ["two"], "<div>one two</div>"),

    T("DataReplace", "<div e:render=\"data\" e:data=\"new\" />", ["old"], "<div>new</div>"),
    T("DataClear", "<div e:render=\"data\" e:data=\"\" />", ["old"], "<div />"),
    F("DataInherit", "<div e:data=\"new\"><div e:render=\"data\" /></div>", ["old"], "<div><div>new</div></div>"),
    F("DataInherit2", 
      "<div e:render=\"data\"><e:inv e:data=\"new\"><div e:render=\"data\" /></e:inv></div>", 
      ["old"], "<div><div>new</div>old</div>"),

    T("DataExpMath", "<div e:render=\"data\" e:dataExp=\"4 * 5\" />", [], "<div>20</div>"),
    T("DataExpStrings", "<div e:render=\"data\" e:dataExp=\"string:strip(S('ohello'), left, $o)\" />", [], "<div>hello</div>"),
    T("DataExpData", "<div e:render=\"data\" e:dataExp=\"[D,D]\" />", ["bang"], "<div>bangbang</div>"),

    T("AttrLiteral", "<div><e:attr name=\"key\">Val</e:attr></div>", [], "<div key=\"Val\" />"),
    T("AttrRender", "<div><e:attr name=\"key\" e:render=\"data\" /></div>", ["Data"], "<div key=\"Data\" />"),
    T("AttrBoth", "<div><e:attr name=\"key\" e:render=\"data\">One</e:attr></div>", ["Two"], "<div key=\"OneTwo\" />"),
    T("AttrInv", "<div><e:attr name=\"key\"><e:inv e:render=\"data\" /></e:attr></div>", ["Val"], "<div key=\"Val\" />"),

    T("ForeachLit", "<div e:render=\"foreach\">*</div>", [1,2,3], "<div>***</div>"),
    T("ForeachData", "<div e:render=\"foreach\"><e:inv e:render=\"data\" /></div>", [1,2,3], "<div>123</div>"),
    F("ForeachNest", 
      "<div e:render=\"foreach\"><div e:render=\"foreach\"><e:inv e:render=\"data\" /></div></div>", 
      [[1,2,3], [4,5,6]], 
      "<div><div>123</div><div>456</div></div>"),

    ok.
