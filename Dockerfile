# Use the existing Haskell image as our base
FROM haskell:8.6.5
# Checkout our code onto the Docker container
WORKDIR /app
ADD . /app
# Build our code
RUN stack setup
RUN stack build --copy-bins
# RUN stack build --test --copy-bins
# Expose a port to run our application
EXPOSE 80
ENTRYPOINT sleep infinity