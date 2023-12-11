FROM madnificent/lisp-webservice:feature-modernize

RUN apt-get update; apt-get -y upgrade; apt-get install -y libev-dev gcc;

ENV SYSTEMS="SPARQL-PARSER"
RUN ["/usr/src/load.sh"]

COPY . /app

EXPOSE 8890
