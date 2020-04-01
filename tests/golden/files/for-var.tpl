<ul>
  {let foods =
        {vegetables =
          ["potato",
           "leek",
           "onion",
           "turnip",]} in}{for vegetable in foods.vegetables}
  <li>I like {vegetable}</li>
  {end}{end}
</ul>
