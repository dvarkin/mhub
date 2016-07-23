# MHUB

Kafka flavored message hub. Main goals:

      1. Durable queues
      2. Push new messages to subscribers through TCP
      3. Pull messages with Offset (TCP and UDP)
      4. Pull messages with Marker (TCP and UDP)

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

Queues creates autmaticly, with first "pub" or "sub" call.

     {
     "pub": "queueName", "message": "Test Messge1"
     }

     Response:

     "ok"
     
#### Subscribe to new messages (async push via TCP)

     {
     "sub": "queueName"
     }

     Response:

     "ok"

TODO: improve error and information in response

#### Subscribe with **offset**

You can take previous messages from queue using **offset** key in the subscribe call:

Take newest 2 messages from queue:

    {
    "sub": "queueName", "offset": 2
    }

    Response:

    {
    "queue":"queue1","messages":["4","5"]
    }
    
Take oldes 2 messages from head of queue:

    {
    "sub": "queueName", "offset": -2
    }

    Response:

    {
    "queue":"queue1","messages":["1","2"]
    }

#### Subscribe with **marker**

All messages in queue are strong ordered. You can get older messages and automaticly subscribe to new messages (or pulling in case of UDP) starting from particular position using **marker**.

     {
     "sub": "queueName", "marker": 2
     }

In response, server will include the number of current last number of messages in the queue at this time.

   {
   "sub":"queue1","marker":0
   }

   Response:

   {
   "queue":"queue1",
   "messages":["1","2","3","4","5"],
   "marker":5
   }

So you can use this *marker* position to pull next messages from the queue in new sessions. This is convinient way to pulling messages through UDP, or after reconnections of TCP clients. 

