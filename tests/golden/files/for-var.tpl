<ul>
  {let foods = load-yaml "for-var.yaml" in}{for vegetable in foods.vegetables}
  <li>I like {vegetable}</li>
  {end}{end}
</ul>
