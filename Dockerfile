FROM nixos/nix:latest AS build

WORKDIR /fvckbot
COPY . .
RUN echo -e "experimental-features = nix-command flakes\nfilter-syscalls = false" >> /etc/nix/nix.conf
RUN nix build
RUN echo $(readlink -f result) > nix-path && mkdir nix-store && mv $(nix-store -qR ./result) nix-store/

FROM alpine
COPY --from=build /fvckbot/nix-path ./
COPY --from=build /fvckbot/nix-store /nix/store
ENV TG_BOT_TOKEN=""
CMD ["-c", "$(cat nix-path)/bin/fvckbot"]
ENTRYPOINT [ "/bin/sh" ]
