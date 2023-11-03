import discord
from dotenv import load_dotenv
import sys
import os


def send_to(dc_id, msg, token, fetcher):
    intents = discord.Intents.default()
    intents.messages = True

    client = discord.Client(intents=intents)

    @client.event
    async def on_ready():
#         user = await client.fetch_user(dc_id)
        user = await fetcher(client, dc_id)
        if user:
            await user.send(msg)
        else:
            print(f"user {dc_id} not found", file=sys.stderr)
        await client.close()

    client.run(token)


def main(argv) -> int:
    if len(argv) < 2:
        print(f"usage: {argv[0]} file [user/chan]", file=sys.stderr)
        return 1

    filename = argv[1]
    where = "user" if len(argv) < 3 else argv[2]

    load_dotenv()
    token = os.getenv('DISCORD_TOKEN')
    dc_id = os.getenv('DISCORD_ID')

    if not token:
        print("env. var DISCORD_TOKEN not set", file=sys.stderr)
        return 1

    if not dc_id:
        print("env. var DISCORD_ID not set", file=sys.stderr)
        return 1

    with open(filename, 'r') as f:
        if where == "user":
            send_to(dc_id, f.read(), token, lambda cl, i: cl.fetch_user(i))
        elif where == "chan":
            send_to(dc_id, f.read(), token, lambda cl, i: cl.fetch_channel(i))
        else:
            raise RuntimeError(f"invalid where '{where}', must be user/chan")

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
