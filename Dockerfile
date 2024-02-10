# Use an OCaml base image
FROM ocaml/opam:debian

# Set the working directory in the container
WORKDIR /app

RUN sudo apt-get update && sudo apt-get install -y \
  m4 \
  make \
  && sudo rm -rf /var/lib/apt/lists/*

COPY . .

RUN eval $(opam env) && make all

CMD ["./ps1"]
