# Use the existing Haskell image as our base
FROM haskell:8.6.5
# Checkout our code onto the Docker container
WORKDIR /app
ADD . /app
# Install system dependencies
RUN apt-get update && \
  apt-get install -y \
  default-libmysqlclient-dev \
  libpcre3 \
  libpcre3-dev
# Build our code
RUN stack setup
RUN stack build --copy-bins
# RUN stack build --test --copy-bins
# Expose a port to run our application
EXPOSE 80
ENTRYPOINT sleep infinity