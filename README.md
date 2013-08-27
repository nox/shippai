Shippai!
========

Shippai (失敗) is the Japanese word for "failure."

What is it?
-----------

Shippai is a Core Erlang transform which changes any match error to a call to
`erlang:error/2` using the arguments which were given to the current function.

Calls to `erlang:error(badrecord)` added by the record expansion are also
transformed.

Example
-------

``` erlang
-module(t).
-export([t/1]).

t(X) ->
    case X of
        {Y,Z} -> Y + Z
    end.
```

### Without Shippai

```
$ erlc t.erl
$ erl
1> t:t(foo).
** exception error: no case clause matching foo
     in function  t:t/1 (t.erl, line 5)
1> catch t:t(foo).
{'EXIT',{{case_clause,foo},
         [{t,t,1,[{file,"t.erl"},{line,5}]},
          ...]}}
```

### With Shippai

```
$ erlc +'{core_transform,shippai_transform}' t.erl
$ erl
1> t:t(foo).
** exception error: no case clause matching foo
     in function  t:t/1
        called as t:t(foo)
1> catch t:t(foo).
{'EXIT',{{case_clause,foo},
         [{t,t,[foo],[{file,"t.erl"},{line,5}]},
          ...]}}
```

What is there to do?
--------------------

* Write better documentation.
* Write tests.
* Create convenience functions to (de-)instrument a given module.
* Patch OTP to print both line numbers and function arguments in the REPL.

License
-------

Shippai is published under the ISC license.

From [Wikipedia][1]:
> The ISC license is a permissive free software license written by the Internet
> Systems Consortium (ISC). It is functionally equivalent to the Simplified BSD
> License, with language that was made unnecessary by the Berne convention
> removed.

[1]: http://en.wikipedia.org/wiki/ISC_license
