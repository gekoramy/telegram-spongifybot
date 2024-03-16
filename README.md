# telegram-spongifybot

```sh
docker image pull ghcr.io/gekoramy/telegram-spongifybot:latest
```

```sh
docker container run \
  --detach \
  --env-file .env \
  --restart always \
  --log-driver json-file --log-opt max-size=10m --log-opt max-file=1000 --log-opt compress=true \
  ghcr.io/gekoramy/telegram-spongifybot:latest
```
