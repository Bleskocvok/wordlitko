FROM ubuntu

WORKDIR /app

RUN apt update \
    && apt install -y \
    git \
    wget \
    unzip \
    make \
    python3.10 \
    python3-pip \
    ghc

COPY --chown=1001 . .

RUN pip install -r ./requirements.txt

RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN apt install -y ./google-chrome-stable_current_amd64.deb

RUN wget -q https://edgedl.me.gvt1.com/edgedl/chrome/chrome-for-testing/119.0.6045.105/linux64/chromedriver-linux64.zip
RUN unzip chromedriver-linux64.zip -d driver \
    && mv driver/chromedriver-linux64/chromedriver driver/

USER 1001

RUN make

ENTRYPOINT [ "make" ]
CMD [ "run" ]
