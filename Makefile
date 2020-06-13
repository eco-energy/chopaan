gen_schema:
	git submodule update --remote --merge 

mqtt_cert:
	aws iot create-keys-and-certificate --set-as-active \
	--certificate-pem-outfile certs/chopaan.cert.pem \
  --public-key-outfile certs/chopaan.pub.key.pem \
  --private-key-outfile certs/chopaan.private.key.pem \
	| jq .certificateId | tee -a certs/cert.id && \
	python certs/update_params.py && \
  aws iot register-thing --template-body file://certs/chopaan_template.json --parameters file://certs/params.json


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
