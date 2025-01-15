source .env
gpgtar --encrypt --symmetric --output data.gpg  --gpg-args="--passphrase=${DATA_PASSWORD} --batch" data
docker compose build --push
docker compose up