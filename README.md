#Overview
Embed data within ICMP (Internet Control Message Protocol) Echo packets, a form of steganography.

This uses [gen_icmp](https://github.com/msantos/gen_icmp) for the packet sending and receiving.

#Building
This assumes that `rebar` is in your path.

```
$ make deps
$ make
$ make chmod
$ make run
```

`make chmod` is necessary due to the elevated privileges needed by gen_icmp (via its use of [procket](https://github.com/msantos/procket) for socket access). More information is available at the procket link on Github.

#Techniques Available for Placing the Data
See [details on ICMP Echo](http://en.wikipedia.org/wiki/Ping_%28networking_utility%29) for information on packet construction.

##Payload
Places the data to be sent in the data/payload portion of the ICMP packet.

##Identifier
Places the data to be sent in the 16-bit Identifier field (sending 2 bytes at a time). Currently a 0 byte is used as a flag, so arbitrary binary data may not be sent (as opposed to messages containing text).

##Parity of Message Length
Sends a single bit at a time, inferring its value by modifying the length of the data/payload. An even length represents a 0 bit, and an odd length represents a 1 bit.

#Using
On the listener, begin waiting to receive a message, given the technique to use. Technique is one of the atoms `payload`, `identifier`, or `payload_len`.

```
1> icmp_data_receive_server:start_receive(Technique).
```

On the sending side, the technique is also included.

```
1> icmp_data_send:send("example.com", payload, Message).
```

To stop receiving:

```
1> icmp_data_receive_server:stop_receive().
```

#Observing Network Traffic
ICMP packets may be observed with tcpdump (or others). This may be done for packets sent by way of a standard ping, or with this project.

```
$ # If lo0 is the local loopback interface reported by netstat
$ sudo tcpdump -X -i lo0 'icmp[icmptype] == icmp-echo'
```

And in another window, send 1 ping (echo request) to 127.0.0.1:

```
$ ping -c1 127.0.0.1
```

#Possible Improvements
* Add verification that the 8 packets that arrive for a given byte when using `payload_len` arrive in order. This can be known from the sequence number field, as the sequence number for bit 0 of a byte is always evenly divisible by 8, with subsequent bits increasing from there.



