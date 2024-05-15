# params

> Association-list utilities for web services

## Warning

Association list item keys are assumed to be strings, and optimization declarations
prioritize speed over safety. Take care to validate the structure of the association
list prior to using this library to access specific values.

## Usage

See the tests for complete examples.

Example:

```lisp
(let ((params '(("qux" . 11)
                ("bar" . "baz")
                ("foo" . (("baaz" . "quux")
                          ("quux" . (("foo" . "bar")))
                          ("bar" . 33)
                          ("foobar" . (1 2 3)))))))
  (let ((qux (get-param params "qux"))
        (multiple-toplevel (collect-params params '("qux" "bar")))
        (baaz (get-nested-param params '("foo" "baaz")))
        (multiple-nested (collect-nested-params params '(("qux")
                                                         ("bar")
                                                         ("foo" "baaz")
                                                         ("foo" "bar")
                                                         ("foo" "quux" "foo")))))
     (format t "qux: ~A~%" qux)
     (format t "multiple-toplevel: ~{~A~^, ~}~%" multiple-toplevel)
     (format t "baaz: ~A~%" baaz)
     (format t "multiple-nested: ~{~A~^, ~}~%" multiple-nested)))

;; prints:
;;
;; qux: 11
;; multiple-toplevel: 11, baz
;; baaz: quux
;; multiple-nested: 11, baz, quux, 33, bar
```

## Installation

Not in Quicklisp, so clone to "local-projects/".

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.params)
```

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
