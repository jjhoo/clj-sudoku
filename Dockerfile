FROM debian:bullseye-slim

RUN apt-get update
RUN apt-get install -y \
    clojure \
    curl \
    git \
    leiningen

RUN useradd -u 1001 jenkins -m
RUN mkdir /home/jenkins/.m2

USER jenkins
