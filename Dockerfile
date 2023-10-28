FROM ubuntu

WORKDIR /app

COPY --chown=1001 . .

RUN apt update \
    && apt install -y \
    make \
    python3.10 \
    python3-pip \
    ghc

RUN pip install -r ./requirements.txt

USER 1001

ENTRYPOINT [ "make", "run" ]
