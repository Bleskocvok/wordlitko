FROM ubuntu

WORKDIR /app

COPY --chown=1001 . .

RUN apt update \
    && apt install -y \
    wget \
    unzip \
    make \
    python3.10 \
    python3-pip \
    ghc

RUN pip install -r ./requirements.txt

RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN apt install -y ./google-chrome-stable_current_amd64.deb

USER 1001

RUN ls -R .

ENTRYPOINT [ "make", "run" ]
