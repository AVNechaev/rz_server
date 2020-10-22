#/bin/bash

set -e

escript ./rebar3 as prod release
scp ./_build/prod/rel/rz_server/lib/rz_server-1/ebin/*.beam RZH:~/rz_forex/server/lib/rz_server-1/ebin/
#scp ./_build/prod/rel/rz_server/lib/rz_util-1/ebin/*.beam RZ:~/rz_forex/dist/lib/rz_util-1/ebin/
#scp ./_build/prod/rel/rz_server/lib/iqfeed_client-1/ebin/*.beam RZ:~/rz_forex/dist/lib/iqfeed_client-1/ebin/
#scp ./_build/prod/rel/rz_server/lib/erlang_localtime-1.0.1/ebin/*.beam RZ:~/rz_forex/dist/lib/erlang_localtime-1.0.1/ebin/