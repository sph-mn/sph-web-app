this (sph web app) application can be started by executing:
  ./exe/start

exit with ctrl+c

it is configured to listen for scgi requests on a socket whose path will be displayed.
a http server that supports scgi can be used to transfer requests to the application.

tests can also be executed with:
  ./exe/test

the directory "root" is for compiled and other static files, for example served by the http server.