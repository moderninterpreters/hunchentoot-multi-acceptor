# hunchentoot-multi-acceptor
### Arnold Noronha <arnold@jipr.io>

Typically each hunchentoot acceptor listens on a single port, and
typically serves a single domain. A typical configuration is to have a
frontend webserver like Nginx to route requests to the appropriate
port.

This is hard to manage when the number of domains keep increasing
(which is typical for small companies trying to iterate on different
products). hunchentoot-multi-acceptor simplifies the configurations of
such systems.

## License

Apache License, Version 2.0
