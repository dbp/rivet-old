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

## Tasks

The current list of supported tasks are:

`setup` - sets up the project, pull in (and building) all the
    dependencies. Note that you can re-run this if you change
    dependencies, etc.

`run` - build and run the development version of the application. Only
    rebuilds if `.hs` files in `src/` have changed.

`run:docker` - builds and runs the application using Docker. You need
    to rerun this when you make changes, it does not do automatic
    reloading (yet). In order to do this, you need Docker installed,
    but do not need ghc (or postgresql). Also, currently the
    boot2docker setup for macosx can't support this, as we need to
    mount volumes into the container (to persist data in the database).

`update` - builds and installs all needed dependencies (including
           those needed for tests).

`test` - run tests.

`repl` - start a repl for project.

`db` - connect to development database.

`db:create` - Creates needed databases and database user (NOT FINISHED).

`db:new` - create a new migration in the `migrations` directory, using
           the `migrate` utility (unreleased, on github at dbp/migrate)

`db:migrate` - Runs migration against development database (NOT FINISHED).

`db:migrate:docker` - Runs migrations against development setup in docker (NOT FINISHED).
