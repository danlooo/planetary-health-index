source .env
rm data.gpg || :

tar -czf - data | \
gpg --symmetric --cipher-algo AES256 \
    --batch --yes --pinentry-mode loopback \
    --passphrase "$DATA_PASSWORD" \
    -o data.gpg

docker build -t danlooo/planetary-health-index2 .
docker run -p 80:80 -e DATA_PASSWORD=$DATA_PASSWORD danlooo/planetary-health-index2


54.36.111.103
8483