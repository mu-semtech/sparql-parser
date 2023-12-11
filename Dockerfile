FROM madnificent/lisp-webservice:0.6.0

RUN apt-get install -y libev-dev gcc;

ENV SYSTEMS="SPARQL-PARSER"
RUN ["/usr/src/load.sh"]

COPY . /app

EXPOSE 8890
