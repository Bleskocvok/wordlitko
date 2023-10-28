import discord
from dotenv import load_dotenv
import sys
import os


def send_to_user(user_id, msg, token):
    intents = discord.Intents.default()
    intents.messages = True

    client = discord.Client(intents=intents)

    @client.event
    async def on_ready():
        user = await client.fetch_user(user_id)
        if user:
            await user.send(msg)
        else:
            print(f"user {user_id} not found", file=sys.stderr)
        await client.close()

    client.run(token)


def main(argv) -> int:
    if len(argv) < 2:
        print(f"usage: {argv[0]} file", file=sys.stderr)
        return 1

    filename = argv[1]

    load_dotenv()
    token = os.getenv('DISCORD_TOKEN')
    user_id = os.getenv('DISCORD_USER')

    if not token:
        print("env. var DISCORD_TOKEN not set", file=sys.stderr)
        return 1

    if not user_id:
        print("env. var DISCORD_USER not set", file=sys.stderr)
        return 1

    with open(filename, 'r') as f:
        send_to_user(user_id, f.read(), token)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
