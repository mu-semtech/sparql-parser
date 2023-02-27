FROM madnificent/lisp-webservice:0.5.0

COPY . /app

ENV SYSTEMS="sparql-parser"
ENV BOOT="sparql-parser"
