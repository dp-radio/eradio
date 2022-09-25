FROM erlang:latest

RUN    groupadd -o -g 1000 'erlang' \
    && useradd -m -o -u 1000 -g 1000 -s '/bin/bash' -d '/home/erlang' 'erlang'
USER erlang
ENV HOME /home/erlang
ENV USER erlang
ENV LC_ALL C.UTF-8
WORKDIR /home/erlang

COPY --chown=erlang:erlang _build/default/lib/ /home/erlang/_build/default/lib/
COPY --chown=erlang:erlang apps/ /home/erlang/apps/
COPY --chown=erlang:erlang config/sys.config /home/erlang/config/

ENV ERL_LIBS=/home/erlang/_build/default/lib/

EXPOSE 8080/tcp

ENTRYPOINT [ "/usr/local/bin/erl", "-config", "config/sys.config", "-s", "eradio_app" ]

CMD [ "-noinput", "-sname", "eradio@localhost" ]
