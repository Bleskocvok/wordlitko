#!/usr/bin/env python3

import discord
from dotenv import load_dotenv
import sys
import os
from signal import alarm


def send_to(dc_id, msg, token, fetcher):
    intents = discord.Intents.default()
    intents.messages = True

    client = discord.Client(intents=intents)

    @client.event
    async def on_ready():
        user = await fetcher(client, dc_id)
        if user:
            await user.send(msg)
        else:
            print(f"user {dc_id} not found", file=sys.stderr)
        await client.close()

    @client.event
    async def on_error(ev, *args, **kwargs):
        ex = sys.exc_info()
        print(f"error: {ev}: {ex}", file=sys.stderr)
        await client.close()

    client.run(token)


def main(argv) -> int:
    if len(argv) < 2:
        raise RuntimeError(f"usage: {argv[0]} <user | chan> id [file]")

    where = argv[1]
    dc_id = argv[2]

    load_dotenv()
    token = os.getenv('DISCORD_TOKEN')

    if not token:
        raise RuntimeError("environment variable DISCORD_TOKEN not set")

    input_data = ""

    if len(argv) >= 4:
        with open(argv[3], 'r') as f:
            input_data = f.read()
    else:
        input_data = sys.stdin.read()

    # kill the process if it gets stuck for 5s
    alarm(5)

    if where == "user":
        send_to(dc_id, input_data, token, lambda cl, i: cl.fetch_user(i))
    elif where == "chan":
        send_to(dc_id, input_data, token, lambda cl, i: cl.fetch_channel(i))
    else:
        raise RuntimeError(f"invalid where '{where}', must be user/chan")

    return 0


if __name__ == '__main__':
    try:
        sys.exit(main(sys.argv))
    except Exception as ex:
        print(f"error: {ex}", file=sys.stderr)
        sys.exit(1)

