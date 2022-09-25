FROM erlang:latest

ARG UID
ARG GID

# Create a user to map the host user to.
RUN    groupadd -o -g ${GID} 'build' \
    && useradd -m -o -u ${UID} -g ${GID} -s '/bin/bash' -d '/home/build' 'build'
USER build
ENV HOME /home/build
ENV USER build
ENV LC_ALL C.UTF-8
WORKDIR /home/build/src

CMD [ "/bin/bash" ]
