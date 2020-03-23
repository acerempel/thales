# Thales: a simple textual templating system

Thales takes template files, containing text of whatever shape you like
interspersed with specially delimited template directives, and
interprets the directives, substituting their results back into the
template to produce the target file. Template directives allow for
inclusion of values from [YAML] files, [Markdown] files, and from other
templates. Thales tracks the dependencies of each template, so that if a
target is requested which has been built before and whose dependencies
have not since changed, it will not be rebuilt.

The syntax of Thales templates is reminiscent of [Jinja2], but the language of
template directives is much more restricted; in that respect
is it like [Mustache templates], but more strongly typed.

[YAML]: https://yaml.org "The Official YAML Web Site"
[Markdown]: https://www.markdownguide.org "Markdown Guide"
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
allows you to select whatever delimiters you like, but this is the
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

-------

I created this because I wanted a templating system to generate
my static website with, but I wanted to write my own---why? I
don't know, I guess it seemed more appealing to me.

This project may be found on GitHub at
<https://github.com/parson-yorick-thales-templating>.
