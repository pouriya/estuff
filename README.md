# `estuff`
Erlang [rebar3](https://rebar3.org) template for making new Erlang/OTP project.


# Build
```sh
~ $ git clone --depth=1 https://github.com/Pouriya-Jahanbakhsh/estuff && cd estuff
...
~/estuff $ make install # or gmake install on FreeBSD
```
You can also use `make uninstall`. Note that you should replace your email in `./src/estuff.template`. 


# Usage

## Creating new project
```sh
~ $ rebar3 new estuff foo && cd foo
===> Writing foo/src/foo.erl
===> Writing foo/src/foo_app.erl
===> Writing foo/src/foo_sup.erl
===> Writing foo/src/foo.app.src
===> Writing foo/src/foo_utils.erl
===> Writing foo/rebar.config
===> Writing foo/rebar.config.script
===> Writing foo/config/sys.config
===> Writing foo/config/vm.args
===> Writing foo/LICENSE
===> Writing foo/README.md
===> Writing foo/Makefile
===> Writing foo/tools/rebar3
===> Writing foo/tools/user_default.erl
===> Writing foo/test/foo_SUITE.erl
===> Writing foo/include/foo.hrl
===> Writing foo/tools/coverage_summary.awk
===> Writing foo/test/shell_quick_test.script
===> Writing foo/Dockerfile
~/foo $
```

## Shell
```sh
~/foo $ make shell # use gmake on FreeBSD
Compiling user_default module
===> Verifying dependencies...
===> Compiling foo
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:256] [hipe] [kernel-poll:true]
Eshell V9.3  (abort with ^G)
(foo@localhost)1>
```
Module `foo` is loaded in shell:
```erlang
(foo@localhost)1> foo: % use Tab key
module_info/0  module_info/1  start/0        stop/0
```

You can edit project files and recompile/reload all files without restarting shell:
```erlang
(foo@localhost)1> cr(). % use c() to just compile them and use r() to just reload them
make[1]: Entering directory '~/foo'
Compiling code
make[1]: Leaving directory '~/foo'
Reloading foo
Reloading foo_app
Reloading foo_sup
Reloading foo_utils
ok
```

You can use `cl().` to clean the screen, I tested it on FreeBSD and Linux:
```erlang








% above commands
(foo@localhost)2> cl().
```
Will be:
```erlang
(foo@localhost)3>







```

Also you can have a quick test in shell using `test().`:
```erlang
(foo@localhost)3> test().
Running ./test/shell_quick_test.script

{ok,ok}
(foo@localhost)4>
```
Just edit `./test/shell_quick_test.script` and run it.

## Test
```sh
~/foo $ make test
Compiling code
Running tests
Coverage summary:
	foo_app                                 0   %
	foo_utils                               100 %
	foo                                     0   %
	foo_sup                                 0   %
	Total                                   0   %
```
In above, Coverage summary is colored. Less than 15% will be Gray, Between 16% and 50% will be Red, Between 51% and 75 will be Orange and +76% will be Green.  
You can edit `./tools/coverage_summary.awk` if you want.

## Release
```sh
~/foo $ make release
Compiling code
Building release foo-0.0.0

~/foo $ ./foo-0.0.0/bin/foo console
Exec: ~/foo/foo-0.0.0/erts-9.3/bin/erlexec -boot ~/foo/foo-0.0.0/releases/0.0.0/foo -mode embedded -boot_var ERTS_LIB_DIR ~/foo/foo-0.0.0/lib -config ~/foo/foo-0.0.0/releases/0.0.0/sys.config -args_file ~/foo/foo-0.0.0/releases/0.0.0/vm.args -pa -- console
Root: ~/foo/foo-0.0.0
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:256] [hipe] [kernel-poll:true]
Eshell V9.3  (abort with ^G)
(foo@localhost)1>
```

## Packaging
```sh
~/foo $ make package v=0 # set verbositiy to 0 to see less information
Compiling code
Packaging source to foo-0.0.0-src.tar.gz
Packaging application to foo-0.0.0-app.tar.gz
Building release foo-0.0.0
Packaging release to foo-0.0.0-release.tar.gz

~/foo $ ls *.tar.gz
foo-0.0.0-app.tar.gz  foo-0.0.0-release.tar.gz  foo-0.0.0-src.tar.gz 
```
`NAME-VERSION-app.tar.gz`: contains application source with compiled erlang files.  
`NAME-VERSION-release.tar.gz`: contains erlang release with all dependencies + sasl + erts.  
`NAME-VERSION-src.tar.gz`: contains application source with build configuration files.  


## Other make targets
`all`: Compiles, Runs tests, Shows coverage summary and builds release package.  
`compile`: Compiles code.  
`dialyzer`: Runs dialyzer for this project.  
`docs`: Makes `edoc` for this project.  
`clean`: Runs `rebar3 clean`.  
`distclean`: Runs `rebar3 clean` and removes `rebar.lock`, created release directory, `./ebin` directory, etc.  
`push`: Runs tests and runs `git push origin master` if test passed.  
`docker`: Builds docker image for project's release. At the time I did not test it.  

# make options
By default its verbosity is `1`. Use  
`v=2` to see commands.  
`v=3` to see what options are passed to `rebar`.  
`v=4` to enable `rebar3` debug too.  
```sh
~/foo $ make compile v=4
Compiling code
\
            export FOO_BUILD=COMPILE && \
            export DEBUG=1 && \
            export FOO_VERSION=0.0.0 && \
            export FOO_BUILD_DEBUG=1 && \
            /opt/estuff/test/foo/tools/rebar3 compile \
        
===> Evaluating config script "rebar.config.script"
REBAR configuration for 'foo' with build type 'compile': [{erl_opts,
                                                           [debug_info]},
                                                          {deps,[]},
                                                          {cover_enabled,true},
                                                          {cover_export_enabled,
                                                           true},
                                                          {dialyzer,
                                                           [{warnings,
                                                             [no_return]},
                                                            {plt_apps,
                                                             top_level_deps},
                                                            {plt_prefix,
                                                             "foo"}]},
                                                          {edoc_opts,[]},
                                                          {relx,
                                                           [{include_erts,
                                                             true},
                                                            {include_src,
                                                             false},
                                                            {dev_mode,false},
                                                            {generate_start_script,
                                                             true},
                                                            {extended_start_script,
                                                             true},
                                                            {sys_config,
                                                             "./config/sys.config"},
                                                            {vm_args,
                                                             "./config/vm.args"}]}]

===> Expanded command sequence to be run: [{default,app_discovery},
                                                  {default,install_deps},
                                                  {default,lock},
                                                  {default,compile}]
### Lots of rebar debug info

===> run_hooks("/opt/estuff/test/foo", post_hooks, compile) -> no hooks defined

===> run_hooks("/opt/estuff/test/foo", post_hooks, compile) -> no hooks defined

cp -r /opt/estuff/test/foo/_build/default/lib/foo/ebin /opt/estuff/test/foo

```
Also you can use `v=0` to see less information.  

If you want to save coverage summary in file, use `coverage` option:
```sh
~/foo $ make test coverage=COVERAGE_SUMMARY.txt
Compiling code
Running tests
Coverage summary:
	foo_app                                 0   %
	foo_utils                               100 %
	foo                                     0   %
	foo_sup                                 0   %
	Total                                   0   %
~/foo $ cat COVERAGE_SUMMARY.txt 
foo_app                                 0   %
foo_utils                               100 %
foo                                     0   %
foo_sup                                 0   %
Total                                   0   %
```

# Contributing
All issues are welcomed. Before making a PR, Please open an issue and let's talk about it.  

### Author
`pouriya.jahanbakhsh@gmail.com`
