#!/bin/bash

# Add local user

USER_ID =${LOCAL_USER_ID:-9001}
APP_DIR=${APP_DIR:-/opt/app}

echo "starting with UID : $USER_ID"
useradd --shell /bin/bash -u $USER_ID -o -c "" -m user
export HOME=/home/user

chown -R user. $APP_DIR

exec /sbin/pid1 -u user -g user "$@"
