# whatels

Experimental Erlang language service. Requires Erlang/OTP 17+. Dependencies:

* [rebar3](http://www.rebar3.org)
* [jsx](https://github.com/talentdeficit/jsx)
* [monkey](https://github.com/yuce/monkey)
* [erwatch](https://github.com/yuce/erwatch)

See [Erlang VSCode NEXT](https://github.com/yuce/vscode-erlang-next)
for a [Visual Studio Code](https://code.visualstudio.com/) extension and
[whatels node](https://www.npmjs.com/package/whatels)
for a [NodeJS](https://nodejs.org) client.

## Build

    $ rebar3 compile

## Run

    $ rebar3 as prod release
    $ _build/prod/rel/whatels/bin/whatels foreground

## Messages

### General message format

    <MESSAGE NAME> <PAYLOAD SIZE>\r\n
    <PAYLOAD>\r\n

## License

```
Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* The names of its contributors may not be used to endorse or promote
  products derived from this software without specific prior written
  permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```