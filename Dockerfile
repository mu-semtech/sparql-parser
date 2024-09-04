FROM madnificent/lisp-webservice:feature-modernize

RUN apt-get update; apt-get -y upgrade; apt-get install -y libev-dev gcc;
RUN mkdir -p /data
RUN touch /data/.unmounted-data-folder

ENV SYSTEMS="SPARQL-PARSER"

COPY ./launch-sparql-parser.sh /

RUN ["/usr/src/load.sh"]

CMD ["/launch-sparql-parser.sh"]

COPY . /app

EXPOSE 8890
