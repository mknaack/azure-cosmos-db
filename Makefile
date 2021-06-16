.PHONY: all build unit_test integration_test clean format promote doc_to_web

build:
	dune build

all: build

unit_test:
	dune runtest

integration_test:
	dune exec integration_test/test.exe

clean:
	rm -rf _build *.install

format:
	dune build @fmt

promote:
	dune promote

doc:
	dune build @doc

doc_to_web: doc
	cp -r _build/default/_doc/_html/* ../knaack.bitbucket.io/

docker_base:
	docker build -t azure-cosmos-db-base -f dockerfile.base .

docker_build:
	docker build -t azure-cosmos-db-build -f dockerfile.build .
	docker run azure-cosmos-db-build

docker_base_tag:
	docker tag azure-cosmos-db-base:latest mknaack/azure-cosmos-db-base:0.6
	docker push mknaack/azure-cosmos-db-base:0.6
