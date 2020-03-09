# TODO

## Stuff to do before this is usable

- Test out the error reporting a bit

- Parsing of:

  - Record literals
  - Array indexing
  - Function application (but what shape is the data
    structure? and how do we do dynamic typing?)
  - Be able to compare syntax trees for equality ignoring
    SourcePos.

- Getting environment from a yaml file.

  - ~~Extend golden tests suitably.~~

- Framework for negative golden tests.

- Implement "load-template".

  - Just in IO for now.
  - Requires figuring out what the internal function API
    looks like.

- Implement "load-markdown".

- Implement the Shake backing monad.

  - This will be interesting and fun. May involve writing
    BuiltinRules.
