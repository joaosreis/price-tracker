FROM ocaml/opam:alpine_ocaml-4.06.0

WORKDIR /home/opam/src

RUN opam remote add dev git://github.com/joaosreis/opam-repository

COPY price-tracker-exe.opam .

RUN opam pin add -yn price-tracker-exe . && \
    opam depext price-tracker-exe && \
    opam install --deps-only price-tracker-exe

COPY . .

RUN sudo chown -R opam /home/opam/src && \
    opam config exec make deps && \
    opam config exec make && \
    opam depext -ln price-tracker-exe | egrep -o "\-\s.*" | sed "s/- //" > depexts

FROM alpine

WORKDIR /app

COPY --from=0 /home/opam/src/bin/price-tracker-exe price-tracker.exe

COPY --from=0 /home/opam/src/depexts depexts

RUN cat depexts | xargs apk --update add && rm -rf /var/cache/apk/*

CMD ./price-tracker.exe
