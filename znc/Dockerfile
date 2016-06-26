FROM debian:jessie
MAINTAINER Josh Elsasser "josh@elsasser.ca"

ENV ZNC_VERSION 1.6.3

RUN apt-get update && apt-get install -y \
        build-essential \
        ca-certificates \
        libssl-dev \
        libperl-dev \
        pkg-config \
        libicu-dev  && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists* /tmp/* /var/tmp/*

WORKDIR /usr/src
RUN curl -K "http://znc.in/releases/archive/znc-${ZNC_VERSION}.tar.gz" && \
    apt-get source znc && \
    cd znc-${ZNC_VERSION} && \
    uupdate "../znc-${ZNC_VERSION}" && \
    cd "../znc-${ZNC_VERSION}" && \
    dpkg-buildpackage -us -uc -nc && \
    dpkg -i "~/znc_${ZNC_VERSION}_*.deb" && \
    rm -rf /var/lib/apt/lists* /tmp/* /var/tmp/*

RUN useradd -d /var/lib/znc --no-create-home --shell /usr/sbin/nologin znc
RUN mkdir -p /var/lib/znc /var/run/znc
RUN chown -R znc /var/lib/znc /var/run/znc

COPY znc.conf /var/lib/znc/configs/
COPY docker-entrypoint.sh /entrypoint.sh

EXPOSE 6667
VOLUME /var/lib/znc

WORKDIR /
USER znc
ENTRYPOINT ["/entrypoint.sh"]
CMD [""]
