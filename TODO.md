# TODO

## For v0.5

- [ ] Implement base templates. Not hard.

## For v0.6

- [ ] Implement YAML config.
  - Maybe start with just a YAML version of
    what is already possible on the command line, and then amend it to
    allow for more complex configuration.

## Eventually, but essential

- [ ] Test out the error reporting a bit

- Parsing of:

  - [ ] Array indexing

- [ ] Be able to compare syntax trees for equality ignoring
      SourcePos.

- [ ] Framework for negative golden tests.

## Done

- [x] Record literals
- [x] Getting environment from a yaml file.
- [x] Implement "load-template".
- [x] Implement "load-markdown".
- [x] Implement the Shake backing monad.
