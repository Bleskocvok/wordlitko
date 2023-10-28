FROM ubuntu

WORKDIR /app

COPY --chown=1001 . .

RUN apt update \
    && apt install -y \
    make \
    python3.10 \
    python3-pip \
    ghc

RUN snap install firefox

RUN pip install -r ./requirements.txt

USER 1001

RUN wget https://github.com/mozilla/geckodriver/releases/download/v0.33.0/geckodriver-v0.33.0-linux64.tar.gz \
    && tar -xf geckodriver-v0.33.0-linux64.tar.gz -C driver/

ENTRYPOINT [ "make", "run" ]
