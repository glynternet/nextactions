#!/usr/bin/env bash
sed "s/{{DOMAIN}}/$DOMAIN/g" < /etc/nginx/conf.d/nginx.conf.template > /etc/nginx/conf.d/default.conf
nginx -g "daemon off;"