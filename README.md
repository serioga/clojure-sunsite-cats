# cats extensions

Extensions for `funcool/cats` clojure library.

[![Clojars Project](https://img.shields.io/clojars/v/ru.sunsite/cats.svg)](https://clojars.org/ru.sunsite/cats)

## Usage

### Box monad

`Box` behaves like `Exception` [http://funcool.github.io/cats/latest/#exception](http://funcool.github.io/cats/latest/#exception) with addons:
- Payload information in full box and failure.
- Carry failure chain information in exception cause and payload.

```clojure
(require '[sunsite.cats.monad.box :as box])

(box/full "OK" :some :payload :info)
=> #<Full "OK" [:some :payload :info]>

(box/failure "Exception message" :some :error :info)
=> #<Failure Exception message [:some :error :info]>

(box/try-on
  (throw (Exception. "Exception message"))
  :some :payload :info)
=> #<Failure Exception message [:some :payload :info]>

(box/failure (box/failure "Oh" :first) :second)
=> #<Failure Oh [:second <- :first]>

```

See also `Exception` documentation on  [http://funcool.github.io/cats/latest/#exception](http://funcool.github.io/cats/latest/#exception).

## License

Copyright © 2016 sunsite.ru

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
