FROM ocaml/opam:alpine

WORKDIR /home/opam/src

COPY TelegraML/telegraml.opam TelegraML/

RUN opam pin add -yn telegraml TelegraML/ && \
    opam depext -u telegraml

COPY price-tracker-exe.opam .

RUN sudo apk add unzip && \
    opam pin add -yn price-tracker-exe . && \
    opam depext price-tracker-exe && \
    opam install --deps-only price-tracker-exe

COPY . .

RUN sudo chown -R opam /home/opam/src && \
    opam config exec make deps && \
    opam config exec make

ENTRYPOINT [ "bin/price-tracker-exe" ]