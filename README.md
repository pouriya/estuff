# `estuff`
Erlang rebar3 template for making new Erlang/OTP project.


# Build
```sh
~ $ git clone --depth=1 https://github.com/pouriya-jahanbakhsh/estuff && cd estuff
~/estuff $ make install
```
You can also use `make uninstall`. Note that you should replace your email in `./src/estuff.template` with my email. 


# example
```sh
pouriya@codefather /p/estuff $ rebar3 new estuff foo_repo
===> Writing foo_repo/src/foo_repo.erl
===> Writing foo_repo/src/foo_repo_app.erl
===> Writing foo_repo/src/foo_repo_sup.erl
===> Writing foo_repo/src/foo_repo.app.src
===> Writing foo_repo/src/foo_repo_utils.erl
===> Writing foo_repo/rebar.config
===> Writing foo_repo/config/sys.config
===> Writing foo_repo/config/vm.args
===> Writing foo_repo/LICENSE
===> Writing foo_repo/README.md
===> Writing foo_repo/Makefile
===> Writing foo_repo/tools/rebar3
===> Writing foo_repo/tools/user_default.erl
===> Writing foo_repo/test/foo_repo_SUITE.erl
===> Writing foo_repo/include/foo_repo.hrl

pouriya@codefather /p/estuff $ cd foo_repo/
pouriya@codefather /p/estuff/foo_repo $ make 
all               cover             dialyzer          ping              remote            stop
clean             coverage-summary  distclean         release           shell             test
compile           ct                docs              release-shell     start             

pouriya@codefather /p/estuff/foo_repo $ make compile 
===> Verifying dependencies...
===> Compiling foo_repo

pouriya@codefather /p/estuff/foo_repo $ make shell 
===> Verifying dependencies...
===> Compiling foo_repo
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:32] [hipe] [kernel-poll:true]

Eshell V8.3  (abort with ^G)
(foo_repo@localhost)1> foo_repo:start().
ok
(foo_repo@localhost)2> cr(). % compile & reload all "foo_repo" files
make[1]: Entering directory '/p/estuff/foo_repo'
===> Verifying dependencies...
===> Compiling foo_repo
make[1]: Leaving directory '/p/estuff/foo_repo'
Reloading foo_repo
Reloading foo_repo_app
Reloading foo_repo_sup
Reloading foo_repo_utils
ok
(foo_repo@localhost)3> 
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a

pouriya@codefather /p/estuff/foo_repo $ make test 
===> Verifying dependencies...
===> Compiling foo_repo
===> Dialyzer starting, this may take a while...
===> Updating plt...
===> Resolving files...
===> Updating base plt...
===> Resolving files...
===> Checking 163 files in "/home/pouriya/.cache/rebar3/rebar3_19.3_plt"...
===> Copying "/home/pouriya/.cache/rebar3/rebar3_19.3_plt" to "/p/estuff/foo_repo/_build/test/foo_repo_19.3_plt"...
===> Checking 163 files in "/p/estuff/foo_repo/_build/test/foo_repo_19.3_plt"...
===> Doing success typing analysis...
===> Resolving files...
===> Analyzing 4 files with "/p/estuff/foo_repo/_build/test/foo_repo_19.3_plt"...

===> Verifying dependencies...
===> Compiling foo_repo
===> Running Common Test suites...
%%% foo_repo_SUITE: ..........
All 10 tests passed.

===> Verifying dependencies...
===> Compiling foo_repo
===> Performing cover analysis...
  cover summary written to: /p/estuff/foo_repo/_build/test/cover/index.html
Coverage summary:
foo_repo_utils: 100%
foo_repo_sup: 0%
foo_repo_app: 0%
foo_repo: 0%
Total: 0%

pouriya@codefather /p/estuff/foo_repo $ make release
===> Verifying dependencies...
===> Compiling foo_repo
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /p/estuff/foo_repo/_build/release/lib
          /usr/local/lib/erlang/lib
===> Resolved foo_repo-0.0.0
===> Including Erts from /usr/local/lib/erlang
===> release successfully created!

pouriya@codefather /p/estuff/foo_repo $ make release-shell 
Exec: /p/estuff/foo_repo/_build/release/rel/foo_repo/erts-8.3/bin/erlexec -boot /p/estuff/foo_repo/_build/release/rel/foo_repo/releases/0.0.0/foo_repo -mode embedded -boot_var ERTS_LIB_DIR /p/estuff/foo_repo/_build/release/rel/foo_repo/lib -config /p/estuff/foo_repo/_build/release/rel/foo_repo/releases/0.0.0/sys.config -args_file /p/estuff/foo_repo/_build/release/rel/foo_repo/releases/0.0.0/vm.args -pa -- console
Root: /p/estuff/foo_repo/_build/release/rel/foo_repo
/p/estuff/foo_repo/_build/release/rel/foo_repo
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:32] [hipe] [kernel-poll:true]


=PROGRESS REPORT==== 4-Oct-2018::18:46:42 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.222.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 4-Oct-2018::18:46:42 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.221.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 4-Oct-2018::18:46:42 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.223.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 4-Oct-2018::18:46:42 ===
         application: sasl
          started_at: foo_repo@localhost
Eshell V8.3  (abort with ^G)
(foo_repo@localhost)1> 
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a

pouriya@codefather /p/estuff/foo_repo $ make start 
pouriya@codefather /p/estuff/foo_repo $ make ping
pong

pouriya@codefather /p/estuff/foo_repo $ make remote 
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:32] [hipe] [kernel-poll:true]

Eshell V8.3  (abort with ^G)
(foo_repo@localhost)1> 
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a

pouriya@codefather /p/estuff/foo_repo $ make stop 
ok

pouriya@codefather /p/estuff/foo_repo $ make docs 
===> Verifying dependencies...
===> Compiling foo_repo
===> Verifying dependencies...
===> Fetching edown ({pkg,<<"edown">>,<<"0.8.1">>})
===> Version cached at /home/pouriya/.cache/rebar3/hex/default/packages/edown-0.8.1.tar is up to date, reusing it
===> Compiling edown
===> Compiling foo_repo
===> Running edoc for foo_repo
pouriya@codefather /p/estuff/foo_repo $ ls doc/
edoc-info  erlang.png  foo_repo.md  README.md  stylesheet.css

```

### Todo
* Add `rebar.config.script` file for changing hex repo to github repo.  

All issues are welcomed.
