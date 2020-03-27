# Thales: a static site generator and templating system

Thales is a static site generator based on a generic textual templating
system. It is very much a **work in progress** and **highly
experimental** — it is barely documented, and under-tested, and I am
aware of some bugs in the parser and pretty-printer. That said, it is
capable of building simple projects quite reliably. 

## What does it do?

Thales takes template files, containing text of whatever shape you like
interspersed with specially delimited template directives, and
interprets the directives, substituting their results back into the
template to produce the target file. The language of template directives
you to refer to values defined in other files — currently, [YAML] files,
[Markdown] files, and other templates. Thales tracks the dependencies of
each template (using the [Shake] build system), so that if a target is
requested which has been built before and whose dependencies have not
since changed, it will not be rebuilt.

The syntax of Thales templates is reminiscent of [Jinja2], but the language of
template directives is much more restricted; in that respect
is it like [Mustache templates], but more strongly typed.

[YAML]: https://yaml.org "The Official YAML Web Site"
[Markdown]: https://www.markdownguide.org "Markdown Guide"
[Shake]: https://shakebuild.com "Shake Build System"
[Jinja2]: https://palletsprojects.com/p/jinja/ "Jinja | The Pallets Projects"
[Mustache templates]: https://mustache.github.io "{{ mustache }}"

An example template:

```html
<html>
  <head><title>A cool webpage</title></head>
  <body>
    {{ let config = load-yaml "config.yaml" in }}
    <p>Good evening! My name is {{ config.my-name }}.</p>
    {{ end }}
    <h1>Some vegetables I like:</h1>
    <ul>
      {{ for vegetable in ["potato", "purple carrot", "elder bean"] }}
      <li>{{ vegetable }}</li>
      {{ end }}
    </ul>
  </body>
</html>
```

Here, `{{` and `}}` delimit the template directives. (The CLI for Thales
allows you to select whatever delimiters you like, but these are the
default.) What do those directives mean? If you have any programming
experience, or have used comparable templating languages before, I do
not think it will be surprising.

- `{{ let config = … in }} … {{ end }}` evaluates the expression to the
  right of the `=`, and gives it the name `config` within the block
  ended by `{{ end }}`.

- `load-yaml` takes a string argument, and, interpreting it as a file
  path, loads it as a YAML file which is assumed to contain key-value
  pairs. The result is a *record*, such that you can write `(load-yaml
  "config.yaml").foo` to ask for the value of the key `foo` in
  `config.yaml`. Note that the path is interpreted as relative to the
  directory that the template file is in.

- The `{{ for … }}` bit is a ‘for’ loop similar to what many
  programming languages provide; in the section between `{{ for vegetable
  in … }}` and `{{ end }}`, the name `vegetable` is assigned to each of of
  `"potato"`, `"purple carrot"`, and `"elder bean"` in succession.
  (Square brackets (`[ … ]`) delimit an *array*, as in many programming
  languages. A name or any other expression that evaluates to an array
  could be used there too.)

Now, suppose that `config.yaml` consists of the following:

```yaml
my-name: Parson Yorick
```

Then, if the above template is stored in `hello.html.template`, we can
run:

```sh
$ thales hello.html
```

—and the result will be written to `hello.html`:

```html
<html>
  <head><title>A cool webpage</title></head>
  <body>
    
    <p>Good evening! My name is Parson Yorick.</p>
    
    <h1>Some vegetables I like:</h1>
    <ul>
      
      <li>potato</li>
      
      <li>purple carrot</li>
      
      <li>elder bean</li>
      
    </ul>
  </body>
</html>
```

(I know, there's a bunch of annoying extra whitespace in there – I want
to fix that soonish.)

## How do you build it?

Thales is written in [Haskell]; I use [cabal-install] to build it;
[stack] also works. You will need GHC 8.6.5 or 8.8.3 (stack will install
the correct GHC for you, cabal-install will not). Simply clone this
repository and run:

```sh
cabal v2-update # Update the Hackage index.
cabal v2-build # Build the project.
```

or

```sh
stack build
```

If you want to install Thales so that you can use it, instead run `cabal
v2-update; cabal v2-install` or `stack install`. Then you can run:

```sh
thales --help
```

[cabal-install]:
  https://cabal.readthedocs.io/en/latest/nix-local-build.html "Quickstart — Cabal 3.3.0.0 User's Guide"

[stack]: https://docs.haskellstack.org/en/stable/README/ "The Haskell Tool Stack"

[Haskell]: https://www.haskell.org "Haskell Language"

## Why?

I created this because I wanted a templating system to generate
my static website with, but I wanted to write my own---why? I
don't know, I guess it seemed more appealing to me.

-------

This project may be found on GitHub at
<https://github.com/parson-yorick-thales-templating>.
