# `estuff`
Erlang rebar3 template for creating new Erlang/OTP project.


# Build
```sh
~ $ git clone --depth=1 https://github.com/pouriya-jahanbakhsh/estuff && cd estuff
~/estuff $ make install
```
You can also use `make uninstall`


# example
```sh
~/projects $ rebar3 new estuff my_new_project
===> Writing my_new_project/src/my_new_project.erl
===> Writing my_new_project/src/my_new_project_app.erl
===> Writing my_new_project/src/my_new_project_sup.erl
===> Writing my_new_project/src/my_new_project.app.src
===> Writing my_new_project/src/my_new_project_utils.erl
===> Writing my_new_project/rebar.config
===> Writing my_new_project/config/sys.config
===> Writing my_new_project/config/vm.args
===> Writing my_new_project/LICENSE
===> Writing my_new_project/README.md
===> Writing my_new_project/Makefile
===> Writing my_new_project/tools/rebar3
===> Writing my_new_project/test/my_new_project_SUITE.erl
===> Writing my_new_project/include/my_new_project.hrl
```
In project directory you can use `make [compile|shell|test|docs|clean|distclean]`

### Todo
* Add coverage in test options and new target in Makefile for `make cover`.  
* Add release options in rebar.config and new target in Makefile for `make release`.
* Add `rebar.config.script` file for changing hex repo to github repo.  
All issues are welcomed.
