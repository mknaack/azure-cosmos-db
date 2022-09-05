FROM ocaml/opam:alpine-ocaml-4.08

# conf-libev not needed??
RUN sudo apk add --update gmp-dev openssl-dev zlib-dev libev-dev openssl-dev

COPY azure-cosmos-db.opam .

# Install dev tools
RUN opam install ocaml-lsp-server ocamlformat
RUN opam install . --deps-only --with-test


# RUN echo "$(ls)" > temp.txt
# RUN echo "$(pwd)" > temp_pwd.txt
# RUN echo "$(ls)"
# RUN echo "$(pwd)"
# ADD azure-cosmos-db.opam azure-cosmos-db.opam
#ADD azure-cosmos-db.opam ../azure-cosmos-db.opam
# COPY azure-cosmos-db.opam .

# Install dependencies
# RUN opam install . --deps-only --with-test
