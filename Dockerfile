FROM ubuntu:24.04

WORKDIR /app/

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    tzdata \
    libssl-dev \
    libffi8 \
    ca-certificates

ADD _build/install/default/bin/price_proxy /app/
ADD Dockerfile /app/

ENTRYPOINT [ "./price_proxy" , "server" ]
