# ring-tutorial

Investigate clojure-ring web framework features.

https://mmcgrana.github.io/2010/03/clojure-web-development-ring.html

## Usage

Then at the REPL, run the Jetty adapter with your handler.

=> (use 'ring.adapter.jetty)
=> (use 'ring-tutorial.core)
=> (run-jetty hello-world-handler {:port 3000})

i.e. (run-jetty [HandlerFn] {:port 3000})

or

=> (use 'ring-tutorial.core)
=> (boot)


A web server will now be running at: http://localhost:3000/

## License

Copyright © 2015

Distributed with no liscence.
