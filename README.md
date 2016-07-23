# MHUB

Kafka flavored message hub. Main goals:

      1. Durable queues
      2. Push new messages to subscribers through TCP
      3. Get messages with Offset (TCP and UDP)
      4. Get messages with Marker (TCP and UDP)

Build and run
-------------

	$ git clone https://github.com/dvarkin/mhub

	$ cd mhub

	$ make

	$ make tests

	$ make run

By default Mhub aquire 2 ports:

   - 5555 for TCP connections

   - 4444 for UDP connections

TODO: Move port configuration to .config file

## Protocol Overview

Mhub binary protocol is JSON based. 

### Actions

#### Publishing messages to queue

Queues creates autmaticly, after first "pub", "sub" or "get" call came.

       {
       "pub": "queue1","message": "Test Messge1"
       }

       Response:

       "ok"

#### Subscribe for new messages (async push via TCP)

     {
     "sub":"queue1"
     }

     Response:

     "ok"

During new messages is comming you will get them as in example bellow:

       {
       "queue":"queue1","messages":"6","marker":5
       }      


TODO: improve error and information format in response

#### Subscribe with **offset**

You can take previous messages from queue using **offset** key in the *get** call. 

Take newest 2 messages from queue:

    {
    "get": "queue1","offset": 2
    }

    Response:

    {
    "queue":"queue1","messages":["4","5"],"marker":5
    }
    
Take oldes 2 messages from head of queue:

    {
    "get": "queue1","offset": -2
    }

    Response:

    {
    "queue":"queue1","messages":["1","2"],"marker":5
    }

TCP clients will automaticly subscribed for new messages. 

#### Subscribe with **marker**

All messages in queue are strong ordered. You can get older messages starting from particular position using **marker**.

    {
    "get": "queue1","marker": 2
    }

In response, server will include the number of current last number of messages in the queue at this time.

    {
    "get":"queue1","marker":0
    }

    Response:

    {
    "queue":"queue1",
    "messages":["1","2","3","4","5"],
    "marker":5
    }

TCP clients will automaticly subscribed for new messages. 

So you can use this *marker* position to pull next messages from the queue in new sessions. This is convinient way to pulling messages through UDP, or after reconnections at TCP clients. 

