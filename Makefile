.PHONY: build gen_schema mqtt_cert dev hoogle setup_hoogle

#DEFAULT_GOAL: help

PROJECT_NAME ?= $(shell grep "^name" chopaan.cabal | cut -d " " -f17)
VERSION ?= $(shell grep "^version:" chopaan.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)
ARCH=$(shell uname -m)

export LOCAL_USER_ID ?= $(shell id -u $$USER)
export BINARY_ROOT = $(shell stack path --local-install-root)
export BINARY_PATH = $(shell echo ${BINARY_ROOT}/bin/${PROJECT_NAME})
export BINARY_PATH_RELATIVE = $(shell BINARY_PATH=${BINARY_PATH} python -c "import os; p = os.environ['BINARY_PATH']; print(os.path.relpath(p).strip())")


IMAGE_NAME=dosti/chopaan


build-d:
	@BINARY_PATH=${BINARY_PATH_RELATIVE} docker-compose build

run-d:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose-up

gen_schema:
	git submodule update --remote --merge && \
	protoc --plugin=protoc-gen-haskell=`stack exec -- which proto-lens-protoc` \
	--haskell_out=./src node_message_schema/NodeMessages.proto

fetch_proto:
	git submodule update --remote --merge

gen_proto:
	protoc --plugin=protoc-gen-haskell=`stack exec -- which proto-lens-protoc` \
	--haskell_out=./src node_message_schema/NodeMessages.proto

mqtt_cert:
	aws iot create-keys-and-certificate --set-as-active \
	--certificate-pem-outfile certs/chopaan.cert.pem \
  --public-key-outfile certs/chopaan.pub.key.pem \
  --private-key-outfile certs/chopaan.private.key.pem \
	| jq .certificateId | tee -a certs/cert.id && \
	python certs/update_params.py && \
  aws iot register-thing --template-body file://certs/chopaan_template.json --parameters file://certs/params.json


db_image:
	docker build -f Dockerfile.db -t chopaan/timescale .

runDB: db_image
	docker-compose up

dev:
	stack test --fast --haddock-deps --file-watch

setup_hoogle:
	stack hoogle -- generate --local

hoogle:
	stack hoogle -- server --local --port=8080

build:
	stack build

s2nix:
	stack-to-nix -o ./nix --stack-yaml=stack.yaml

image:
	docker load < $$(nix-build ./nix/docker.nix)
