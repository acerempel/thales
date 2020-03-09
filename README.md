# Thales: a simple textual templating system

## What is it

It's like [Jinja2](https://palletsprojects.com/p/jinja/), and
a bit less like [mustache
templates](https://mustache.github.io). You create a
_template_, which has a special syntax for delimiting
templating directives, and everything else in the template is
left verbatim. When the template is executed, the results of
the directives are spliced into their location in the
template file. Observe:

```html
<html>
  <head><title>A cool webpage</title></head>
  <body>
    <p>Good evening! My name is {{ my-name }}.</p>
    <h1>Some vegetables I like</h1>
    <ul>
      {{ for vegetable in ["potato", "purple carrot", "elder bean"] }}
      <li>{{ vegetable }}</li>
      {{ end }}
    </ul>
  </body>
</html>
```

In order to be executed, this template needs to know what
`my-name` is! If we provide the following YAML file---

```yaml
my-name: Parson Yorick
```

---then we can execute the template with the result that
“Parson Yorick” is substituted in for `{{ my-name }}`---

```html
  ...
    <p>Good evening! My name is Parson Yorick.<p>
    ...
```

---and the part between `{{ for ... }}` and `{{ end }}` is
executed once for each element in `["potato", "purple
carrot", "elder bean"]`:

```html
    ...
    <ul>
      <li>potato</li>
      <li>purple carrot</li>
      <li>elder bean</li>
    </ul>
  ...
```

Make sense?

I created it because I wanted a templating system to generate
my static website with, but I wanted to write my own---why? I
don't know, I guess it seemed more appealing to me.

This project may be found on GitHub at
<https://github.com/parson-yorick-thales-templating>.
