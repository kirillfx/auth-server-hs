# auth-server

to run

```sh 
stack run -- ./exmaples/config.yaml
```

to generate JWK

```sh
stack run -- generateJWK ./examples/jwk.json
```

to generate JWT

```sh
stack run -- generateToken ./examples/jwk.json "some-user-id"
```
