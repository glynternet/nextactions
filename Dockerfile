FROM nginx:stable
ARG BUILD_DIR=UNDEFINED

COPY ${BUILD_DIR} /var/www
COPY ./nginx.conf.template /etc/nginx/conf.d/nginx.conf.template
COPY ./run.sh /run.sh

CMD ["/run.sh"]