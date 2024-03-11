# telegram-spongifybot

```sh
nix build .#image
```

```sh
docker load < result
```

```sh
docker run \
  --detach \
  --tty \
  --env-file .env \
  --restart always \
  telegram-spongifybot
```
