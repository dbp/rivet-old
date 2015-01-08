## About

Rivet is an opinionated tool for managing Snap web applications. It
grew out of the desire to stop coping every increasingly complicated
Makefiles into new projects, and the desire to add more sophisticated
tasks.

It is still very new and unstable. Use at your own risk.

## Use

`rivet task` is the general syntax. Some commands read required
settings from a `Rivetfile` in the current directory, which is written
in [configurator](http://hackage.haskell.org/package/configurator) syntax.

## Assumptions

It is assumed that the directory that you are running the commands in
is the top level of your project, and that further, the name of the
directory is the lowercase name of the project. This means that if you
are in the `foo` directory, we assume that we'll find `foo.cabal`, and
that the executable that we'll get by running `cabal install` is at
`.cabal-sandbox/bin/foo`.

Further, we assume that the user to connect to the database is
`foo_user`, and that the three databases (the first two on the local
machine) are `foo_devel`, `foo_test`, and `foo_prod`.

We assume (and require) that you want to use cabal sandboxes.

We assume that your tests can all be run by running `spec/Main.hs`.

## Rivetfile

These are the current settings you can put in the file. None are
required to run `rivet`, but **you must have a Rivetfile** in the
project root. This is primarily a safety feature, as right now there
is essentially no checking, and if you weren't at the project root,
some commands might do very unexpected things (see assumptions about
the project name above).

`database-password = "password"`

`dependencies = ["list", "of", "dependencies"`]

Where the strings in the dependencies are
`user/reponame(:branch)(+dir1(,dir2)*)`. The repos are assumed to be
git repositories hosted at github, and to be public (this will change
eventually). The parts in parenthesis are optional, and the colon,
plus sign, and comma are literal. The meaning is that the repository
will be checked out at the following branch, and if the dirs are
present, they will what will be added as sources to the cabal
sandbox. This is to support the pattern where one repository will
contain multiple projects within it.

If you want the dependency to be updated (to get upstream changes),
you can remove "deps/user/reponame" or `touch
deps/user/reponame/.rivetclone`.

```
commands {
  foo = "bar"
  foo1 = "bar2"
}
```

Defines new commands `foo` and `foo1`, where `bar` and `bar2` are
expected to be shell commands (possibly the path to a shell script,
etc). Currently, due to configurator, you can't use colons in the
command names (see pull request
[here](https://github.com/bos/configurator/pull/18)). This is particularly
useful in moving legacy applications over to using `rivet` (where they may
have various ad-hoc Makefile commands).

## Deployment Note

Rivet expects that your deployment setup is via Docker. Currently it
exects there is a single staging host and a single production host
(which could be the same host). The deployment is configured via the `Rivetfile` as:

```
stage-host = "host@staging.server"
prod-host = "host@prod.server"
production-image = "dbp1/project_production"
production-instances = 3
```

Where `production-instances` is the number of docker containers
running in production, not the number of servers (right now, only 1
server is supported).

It expects that the staging containers have names that match
`projname_stage_` and production containers have names that match
`projname_prod_` (they can have arbitrary prefixes and suffixes, and
will need to, or else naming conflicts will happen).

Rivet also expects that you have CI set up so that staging is
automatically built and deployed. The `deploy` actions that we have
are migrating the database, rolling what is on staging out to
production, and rolling back to specific revisions on either. The two
are assumed to be based on the same docker image (though will often be
running different versions of it, of course). Finally, we expect that
staging and production are connected to the same database, as our
`migrate` happens within the context of the staging host.

The rollout and rollback expect that the following command will cause
a single instance of a specific revision (for which there is a docker image
`production_image:tag`) to be run in staging:

`ssh stage-host /srv/deploy.sh projname stage production-image tag 1`

And similarly, for production:

`ssh stage-host /srv/deploy.sh projname stage production-image tag production-instances`

You can see a sample deploy script in the `provisioning` directory of
`rivet-simple-deploy` (the particular path is
`provisioning/roles/common/templates/deploy.sh`).

## Tasks

The current list of supported tasks are:
`tasks` - prints out a list of tasks.

`init` - Creates a project template in the current directory. The project
    name is assumed to be the name of the current directory. Note that
    while this step is optional, many tasks depend on conventions that are
    expressed in the project template. This cannot be re-run, and should be
    run instead of a `snap init` call.

`setup` - sets up the project, pull in (and building) all the
    dependencies. Note that you can re-run this if you change
    dependencies, etc.

`deps` - just does the dependency part of `setup`.

`run` - build and run the development version of the application. Only
    rebuilds if `Main.hs` file in `src/` or the `.cabal` file has changed.

`test` - run tests.

`test pattern` - run tests with name matching pattern.

`repl` - start a repl for project.

`db` - connect to development database.

`db:create` - Creates projname_devel and projname_test databases, and
    projname_user user (the latter is only created if needed). If the user
    does not already exist, or isn't a superuser, commands are run via sudo
    to the postgres user, so you may be prompted for your password (depending
    on how you have sudo set up).

`db:new name_of_migration` - create a new migration in the `migrations` directory, using
    the `rivet-migration` library (see: Migrations).

`db:status` - prints out the status of all migrations (whether they've been applied) locally.

`db:migrate` - Runs all pending migrations against devel and test databases. The migrations
    are run with `rivet-migration` (see: Migrations).

`db:migrate:down` - Reverts the last migration (in development and test).

`deploy:status` - Queries what is currently running on the staging and production hosts.

`deploy:migrate` - Runs migration in staging environment and
    revision. Since the staging server is assumed to be connected to the
    production database, this will be the only production migration
    needed.

`deploy:rollout` - Pushes what is currently running on staging to production.

`deploy:rollback SHA` - Rolls production back to the specified git
revision (short version), which must correspond to a built image,
which anything that was ever running does.

`deploy:stage SHA` - Rolls staging back to the specified git
revision (short version), which must correspond to a built image,
which anything that was ever running does.
