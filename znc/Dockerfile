FROM debian:jessie-backports
MAINTAINER Josh Elsasser "josh@elsasser.ca"

RUN apt-get update && apt-get -y install znc/jessie-backports && \
    useradd -d /var/lib/znc --no-create-home --shell /usr/sbin/nologin znc && \
    mkdir -p /var/lib/znc /var/run/znc && \
    chown -R znc /var/lib/znc /var/run/znc && \
    apt-get clean && rm -rf /var/lib/apt/lists* /tmp/* /var/tmp/*

COPY docker-entrypoint.sh /entrypoint.sh

EXPOSE 6667
VOLUME /var/lib/znc

USER znc
ENTRYPOINT ["/entrypoint.sh"]
CMD [""]
