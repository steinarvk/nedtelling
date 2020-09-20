FROM nginx
COPY dist /usr/share/nginx/html
COPY static /usr/share/nginx/html
