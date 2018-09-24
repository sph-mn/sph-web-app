# nginx example configuration files for use with sph-web-app and scgi

minimal full configuration. for more information see the official nginx documentation

``/etc/nginx.conf``
```
worker_processes 1;

events {
  worker_connections 1024;
}

http {
  include mime.types;
  server {
    server_name project-name.localhost;
    listen [::1]:80;
    root /srv/http/project-name/root;
    location /assets/ {
    }
    location / {
      default_type text/html;
      include params/scgi;
      scgi_pass unix:/tmp/1000/project-name;
    }
  }
}
```

``/etc/nginx/params/scgi``
```
scgi_param SCGI 1;
scgi_pass_request_headers off;
scgi_param request_method $request_method;
scgi_param remote_addr $remote_addr;
scgi_param http_cookie $http_cookie;
scgi_param user_agent $http_user_agent;
scgi_param if_modified_since $http_if_modified_since;
scgi_param content_type $http_content_type;
scgi_param request_uri $request_uri;
scgi_param https $https;
```

* events, worker_processes: nginx fails to start if these options are not set
* mime.types: for serving assets with the right content-type depending on filename extension. you can create and include a custom file with fewer mime types for optimisation
* listen: depends on what the hostname resolves to: if it is 127.0.0.1 instead of the ipv6 address ::1, then use 127.0.0.1:80 instead. *:80 did not work for me
* root: the path to the root directory in the web-app project. the root directory is preferably symlinked to ``/srv/http/project-name``
* location /assets/: this configures nginx to not pass requests with paths that begin with /assets/ to the scgi application
* scgi_pass: the 1000 in ``unix:/tmp/1000/project-name;`` contains the user id of the user that will be running the web-app application, adjust it accordingly. ``echo $UID``