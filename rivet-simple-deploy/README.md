## About

This is a few scripts that make deploying and running small
applications easy. The scripts for provisioning are not flexible or
well tested, but they are simple and should serve as a decent starting
point.

## Provisioning a Server

The `provisioning` directory has `ansible` scripts to set up a
server. It assumes a single server, with domains pointing for both a
staging and a production application (the servers can be separate, but
they need to share the same root login password.

First edit `inventory` and point it at the proper machines. Similarly
edit `group_vars/all`. Finally, you must create a
`provisioning/secrets.yml` file with `ansible-vault edit
provisioning/secrets.yml --vault-password-file=password.txt` (where
somepassfile has some decent password in it). It should look like the
following (with actual values):

```
web_root_password: ...
production_db_password: ...
docker_email: ...
docker_username: ...
docker_password: ...
deploy_key: |
  -----BEGIN RSA PRIVATE KEY-----
  ..
  -----END RSA PRIVATE KEY-----
prod_ssl_key: |
  -----BEGIN RSA PRIVATE KEY-----
  ...
  -----END RSA PRIVATE KEY-----
tarsnap_key: |
  # START OF TARSNAP KEY FILE
  ...
  # END OF TARSNAP KEY FILE
```

`deploy_key` should be a private SSH key that has access to the
`source_repo` (it should _not_ be your private key - create one
specifically for the purpose!). `prod_ssl_key` is the private SSL key
for the site. `web_root_password` is the login password for the
server. `production_db_password` is the password that should be given
to the database user that will be used by the application. The
`docker` credentials are for `Docker Hub` - this is used as part of
the deployment process (and so they get written into the deploy
script). `tarsnap_key` is a key for the backup service `Tarsnap`
(which is used for database backups).

You should put the SSL cert for the server at:
`provisioning/roles/ssl/files/prod.ssl.crt`.

Once that's all set, you should be able to provision the server by
running:

`ansible-playbook -i inventory --vault-password-file=password.txt site.yml`

From within the `provisioning` directory.
