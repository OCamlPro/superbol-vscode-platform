# État de la pr
- les types ne sont pas toujours bien calculés. Typeck ne fonctionne malheureusement pas du tout dans le cas où les variables proviennent d’un copy ou d’un include. La fonction get_type n’est pas complète. Les types sont vraiment bizarre, je trouve.

- Certains cas ne sont pas gérés. On peut les trouver facilement puisqu’elles renvoient un objet Generated_type.Todo { prefix }

Good luck!
