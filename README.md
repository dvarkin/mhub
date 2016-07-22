# MHUB

Kafka flavored message hub. Main goals:

      1. Durable queues
      2. Push new messages through TCP
      2. Pull messages with Offset

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

RxDB binary protocol is JSON based. 

### Actions

#### Publishing messages to queue

Queues creates autmaticly, with first "pub" or "sub" call.

     {
     "pub": "queueName", "message": "Test Messge1"
     }

#### Subscribe to new events

     {
     "sub": "queueName"
     }

#### Subscribe with offset

You can take previous messages from queue using **offset** key in subscribe call:

Take newest 2 messages from queue:

    {
    "sub": "queueName", "offset": 2
    }

Take oldes 2 messages from head of queue:

    {
    "sub": "queueName", "offset": -2
    }
