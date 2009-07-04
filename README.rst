EWGI, an Erlang webserver interface specification
=================================================

EWGI (pronounced `you-ghee`) is a specification designed to allow web
applications written in Erlang to run on any supported server.  It
also makes developing web applications simpler and more flexible by
providing a common mechanism for reusing components.  It was inspired
by Python's `PEP 333`_ and provides similar functionality to other
projects such as Ruby's `Rack`_ .

"Hello world!" example
----------------------

This sample application simply responds with 200 OK and a
``text/plain`` entity body of ``Hello world!`` for all requests.

::

 simple_app({ewgi_context, Request, _Response}) ->
     ResponseHeaders = [{"Content-type", "text/plain"}],
     Response = {ewgi_response, {200, "OK"}, ResponseHeaders,
                 [<<"Hello world!">>], undefined},
     {ewgi_context, Request, Response}.

Middleware components
---------------------

The real power of the EWGI interface specification is the ability to
compose applications so that requests and responses can be modified by
reusable components.  For example, the specification includes an
example middleware component which converts all text responses to
upper-case.

Advantages
----------

* Applications are `server independent.`
* Middleware components can be reused.
* Applications have a clean, functional interface.

Reference implementations
-------------------------

  The current server reference implementations include:

* `Mochiweb`_
* `Yaws`_
* `inets`_

.. _PEP 333:
    http://www.python.org/dev/peps/pep-0333/
.. _Rack:
    http://rack.rubyforge.org/
.. _Mochiweb:
    http://code.google.com/p/mochiweb/
.. _Yaws:
    http://yaws.hyber.org/
.. _inets:
    http://erlang.org/doc/apps/inets/http_server.html
