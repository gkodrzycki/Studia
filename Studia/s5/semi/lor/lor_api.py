from urllib.request import Request, urlopen
import json
from time import sleep
import os

prev_data = ""
with open("klucz.txt") as f:
    my_key = f.read().rstrip("\n")

    base_url = "https://api.clashroyale.com/v1"

    endpoint = "/players/%23208UUL8QG/battlelog"
    request = Request(
        base_url+endpoint,
        None,
        {
            "Authorization": "Bearer % s" % my_key
        }
    )

    response = urlopen(request).read().decode("utf-8")

    data = json.loads(response)


    # for log in data[0]:
    #     if log == 'team' or log == 'opponent':
    #         print(f"{data[0][log][0]['name']}'s cards:")
    #         for card in data[0][log][0]['cards']:
    #             print(f"* {card['name']}")
    #     else:
    #         print(log, data[0][log])
    
    # sleep(1)
    # os.system('clear')