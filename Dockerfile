FROM ubuntu:18.04

WORKDIR /sistemas-distribuidos

USER root 

RUN apt-get update && apt-get install erlang -y && apt-get install wget -y
RUN wget "https://s3.amazonaws.com/rebar3/rebar3"
RUN chmod +x rebar3
RUN mkdir ~/bin && mv rebar3 ~/bin

ENV PATH=~/bin:$PATH

CMD ["tail","-f","/dev/null"]
