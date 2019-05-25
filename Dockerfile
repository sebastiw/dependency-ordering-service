FROM erlang:22.0 as builder

COPY . /artifact
WORKDIR /artifact

RUN make release

FROM ubuntu:18.04

# Make ^G work
ENV TERM=xterm

RUN apt-get update && \
# Erlang dependencies
        apt-get install -y openssl && \
# Debug tools
        apt-get install -y curl htop && \
# Minimizing
        apt-get clean && \
        rm -r /var/cache/apt/archives && \
        rm -r /var/lib/apt/lists

WORKDIR /opt/dos

COPY --from=builder /artifact/_build/default/rel/dos /opt/dos

EXPOSE 8000

CMD ["/opt/dos/bin/dos", "foreground"]
