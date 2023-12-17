FROM ubuntu

WORKDIR /app

RUN apt update \
    && apt install -y \
    build-essential \
    make \
    python3.10 \
    python3-pip

COPY --chown=1001 . .

RUN pip install -r ./requirements.txt

USER 1001

RUN make csolve

ENTRYPOINT [ "bash", "run_fake" ]
CMD [ "" ]
