import os
import json

fp = os.path.dirname(os.path.realpath(__file__))

def replace_params():
    with open(os.path.join(fp, 'params.json'), 'r') as f:
        params = json.load(f)

    with open(os.path.join(fp, 'cert.id'), 'r') as f:
        cert_id = f.read().strip().strip("\"")

    params["CertificateId"] = cert_id

    with open(os.path.join(fp, 'params.json'), 'w') as f:
        json.dump(params, f)


if __name__ == "__main__":
    replace_params()
    os.remove(os.path.join(fp, 'cert.id'))
