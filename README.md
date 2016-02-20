# cats extensions

Extensions for `funcool/cats` clojure library.

[![Clojars Project](https://img.shields.io/clojars/v/ru.sunsite/cats.svg)](https://clojars.org/ru.sunsite/cats)

## Usage

### Box monad
```clojure
(require '[sunsite.cats.monad.box :as box])

(box/full "OK" :some :payload :info)
=> #<Full "OK" :some :payload :info>

(box/failure :some :error :info)
=> #<Failure :some :error :info>

(box/try-on
  (throw (Exception. "Oops"))
  :some :payload :info)
=> #<Failure :some :payload :info <- Oops>
```

See also `Exception` documentation on  [http://funcool.github.io/cats/latest/#exception](http://funcool.github.io/cats/latest/#exception).

## Todo

* Pass tests for version 1.0

## License

Copyright © 2016 sunsite.ru

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
