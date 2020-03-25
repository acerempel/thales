{let me = load-yaml "optionally-present.yaml" in}Hello, my name is {me.name}.

{optionally me.email}
You may contact me at {me.email}.
{end}{end}
