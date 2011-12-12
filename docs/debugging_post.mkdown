<markdown>
It's time to bang on the networking aspect and interact with real ZMQ frames. I have written a pure C test app against libzmq and implemented a one-way transmission test. To gather the wire data I am using tshark, the command line version of the wireshark network packet sniffer.

Problem overview: the pure C version has no problem talking to itself but I get too few, unexpected bytes in the Haskell network listener.

Test tool: a C app sends a short string in one message. The message is short so I can focus on single-frame semantics. Also of note, I explicitly use ZMQ_PAIR to avoid any complicated behavior. The documentation indicates ZMQ_PAIR is experimental! Should have seen that before I started using it. But let's continue with some network debugging.

The Haskell listener application creates a listener socket, binds it to a public port, and waits for incoming bytes. This is done using a bytestring socket interfacie -- it does a large read, 2048 bytes, larger than an MTU so probably unimportant. The bytestring is then passed to a callback. For now the callback just formats the bytes as hex. 

Let's take some time to quantify the problem: I only ever read 2 bytes, 0x017E when I should expect a full frame with the payload "ASDFASDFASDFASDFASDFASDFASDF". What I actually expect is 0x011D and then the payload. So the bytes I read would not be valid if I had more data.

Time to compare wireshark output from C -> Haskell and C -> C.

C -> Haskell
------------

    $> tshark tcp port 7890
    4331.365739 99.174.250.39 -> 184.106.107.229 TCP 55906 > 7890 [SYN] Seq=0 Win=65535 Len=0 MSS=1460 WS=3 TSV=951696379 TSER=0
    4331.365771 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55906 [SYN, ACK] Seq=0 Ack=1 Win=5792 Len=0 MSS=1460 TSV=1910559673 TSER=951696379 WS=7
    4331.440504 99.174.250.39 -> 184.106.107.229 TCP 55906 > 7890 [ACK] Seq=1 Ack=1 Win=524280 Len=0 TSV=951696380 TSER=1910559673
    4331.440959 99.174.250.39 -> 184.106.107.229 TCP 55906 > 7890 [PSH, ACK] Seq=1 Ack=1 Win=524280 Len=2 TSV=951696380 TSER=1910559673
    4331.441004 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55906 [ACK] Seq=1 Ack=3 Win=5888 Len=0 TSV=1910559680 TSER=951696380
    4332.366005 99.174.250.39 -> 184.106.107.229 TCP 55906 > 7890 [FIN, ACK] Seq=3 Ack=1 Win=524280 Len=0 TSV=951696389 TSER=1910559680
    4332.366228 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55906 [FIN, ACK] Seq=1 Ack=4 Win=5888 Len=0 TSV=1910559773 TSER=951696389
    4332.440774 99.174.250.39 -> 184.106.107.229 TCP 55906 > 7890 [ACK] Seq=4 Ack=2 Win=524280 Len=0 TSV=951696390 TSER=1910559773

C -> C
------

    $> tshark tcp port 7890
    4366.164063 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [SYN] Seq=0 Win=65535 Len=0 MSS=1460 WS=3 TSV=951696727 TSER=0
    4366.164096 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55908 [SYN, ACK] Seq=0 Ack=1 Win=5792 Len=0 MSS=1460 TSV=1910563152 TSER=951696727 WS=7
    4366.239330 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [ACK] Seq=1 Ack=1 Win=524280 Len=0 TSV=951696727 TSER=1910563152
    4366.239555 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55908 [PSH, ACK] Seq=1 Ack=1 Win=5888 Len=2 TSV=1910563160 TSER=951696727
    4366.240018 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [PSH, ACK] Seq=1 Ack=1 Win=524280 Len=2 TSV=951696727 TSER=1910563152
    4366.240025 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55908 [ACK] Seq=3 Ack=3 Win=5888 Len=0 TSV=1910563160 TSER=951696727
    4366.315044 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [ACK] Seq=3 Ack=3 Win=524280 Len=0 TSV=951696728 TSER=1910563160
    4366.315075 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [PSH, ACK] Seq=3 Ack=3 Win=524280 Len=30 TSV=951696728 TSER=1910563160
    4366.315084 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55908 [ACK] Seq=3 Ack=33 Win=5888 Len=0 TSV=1910563168 TSER=951696728
    4367.163824 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [FIN, ACK] Seq=33 Ack=3 Win=524280 Len=0 TSV=951696737 TSER=1910563168
    4367.163981 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55908 [FIN, ACK] Seq=3 Ack=34 Win=5888 Len=0 TSV=1910563252 TSER=951696737
    4367.239089 99.174.250.39 -> 184.106.107.229 TCP 55908 > 7890 [ACK] Seq=34 Ack=4 Win=524280 Len=0 TSV=951696737 TSER=1910563252

It looks like there are more exchanges going on in the C version (7 exchanges for C -> Haskell and 12 for C -> C). My intuition says there is an application level control protocol I am missing. Perhaps the documentation is not the whole picture? Now I need to view some hex dumps of those transmissions.

C -> C (detailed)

Super long dump:


      0.000000 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [SYN] Seq=0 Win=65535 Len=0 MSS=1460 WS=3 TSV=951698969 TSER=0
      
    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 40 e1 91 40 00 32 06 e4 20 63 ae fa 27 b8 6a   .@..@.2.. c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b 8f 00 00 00 00 b0 02   k..e...9{.......
    0030  ff ff 94 05 00 00 02 04 05 b4 01 03 03 03 01 01   ................
    0040  08 0a 38 b9 c6 19 00 00 00 00 04 02 00 00         ..8...........
      
      0.000033 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55909 [SYN, ACK] Seq=0 Ack=1 Win=5792 Len=0 MSS=1460 TSV=1910585611 TSER=951698969 WS=7
      
    0000  00 00 0c 9f f0 01 40 40 ac c4 ec b0 08 00 45 00   ......@@......E.
    0010  00 3c 00 00 40 00 40 06 b8 96 b8 6a 6b e5 63 ae   .<..@.@....jk.c.
    0020  fa 27 1e d2 da 65 a3 1e f4 5a ad 39 7b 90 a0 12   .'...e...Z.9{...
    0030  16 a0 82 54 00 00 02 04 05 b4 04 02 08 0a 71 e1   ...T..........q.
    0040  39 0b 38 b9 c6 19 01 03 03 07                     9.8.......
      
      0.075158 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [ACK] Seq=1 Ack=1 Win=524280 Len=0 TSV=951698970 TSER=1910585611
      
    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 34 88 f5 40 00 32 06 3c c9 63 ae fa 27 b8 6a   .4..@.2.<.c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b 90 a3 1e f4 5b 80 10   k..e...9{....[..
    0030  ff ff 91 5a 00 00 01 01 08 0a 38 b9 c6 1a 71 e1   ...Z......8...q.
    0040  39 0b                                             9.
      
      0.075393 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55909 [PSH, ACK] Seq=1 Ack=1 Win=5888 Len=2 TSV=1910585618 TSER=951698970

    0000  00 00 0c 9f f0 01 40 40 ac c4 ec b0 08 00 45 00   ......@@......E.
    0010  00 36 cd 49 40 00 40 06 eb 52 b8 6a 6b e5 63 ae   .6.I@.@..R.jk.c.
    0020  fa 27 1e d2 da 65 a3 1e f4 5b ad 39 7b 90 80 18   .'...e...[.9{...
    0030  00 2e 82 4e 00 00 01 01 08 0a 71 e1 39 12 38 b9   ...N......q.9.8.
    0040  c6 1a 01 7e                                       ...~

      0.075616 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [PSH, ACK] Seq=1 Ack=1 Win=524280 Len=2 TSV=951698970 TSER=1910585611

    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 36 e2 c3 40 00 32 06 e2 f8 63 ae fa 27 b8 6a   .6..@.2...c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b 90 a3 1e f4 5b 80 18   k..e...9{....[..
    0030  ff ff 8f d2 00 00 01 01 08 0a 38 b9 c6 1a 71 e1   ..........8...q.
    0040  39 0b 01 7e                                       9..~

      0.075624 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55909 [ACK] Seq=3 Ack=3 Win=5888 Len=0 TSV=1910585618 TSER=951698970

    0000  00 00 0c 9f f0 01 40 40 ac c4 ec b0 08 00 45 00   ......@@......E.
    0010  00 34 cd 4a 40 00 40 06 eb 53 b8 6a 6b e5 63 ae   .4.J@.@..S.jk.c.
    0020  fa 27 1e d2 da 65 a3 1e f4 5d ad 39 7b 92 80 10   .'...e...].9{...
    0030  00 2e 82 4c 00 00 01 01 08 0a 71 e1 39 12 38 b9   ...L......q.9.8.
    0040  c6 1a                                             ..

      0.150647 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [ACK] Seq=3 Ack=3 Win=524280 Len=0 TSV=951698971 TSER=1910585618

    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 34 e4 88 40 00 32 06 e1 35 63 ae fa 27 b8 6a   .4..@.2..5c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b 92 a3 1e f4 5d 80 10   k..e...9{....]..
    0030  ff ff 91 4e 00 00 01 01 08 0a 38 b9 c6 1b 71 e1   ...N......8...q.
    0040  39 12                                             9.

      0.151389 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [PSH, ACK] Seq=3 Ack=3 Win=524280 Len=30 TSV=951698971 TSER=1910585618

    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 52 be 99 40 00 32 06 07 07 63 ae fa 27 b8 6a   .R..@.2...c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b 92 a3 1e f4 5d 80 18   k..e...9{....]..
    0030  ff ff cc 77 00 00 01 01 08 0a 38 b9 c6 1b 71 e1   ...w......8...q.
    0040  39 12 1d 7e 41 53 44 46 41 53 44 46 41 53 44 46   9..~ASDFASDFASDF
    0050  41 53 44 46 41 53 44 46 41 53 44 46 41 53 44 46   ASDFASDFASDFASDF
      
      0.151408 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55909 [ACK] Seq=3 Ack=33 Win=5888 Len=0 TSV=1910585626 TSER=951698971
      
    0000  00 00 0c 9f f0 01 40 40 ac c4 ec b0 08 00 45 00   ......@@......E.
    0010  00 34 cd 4b 40 00 40 06 eb 52 b8 6a 6b e5 63 ae   .4.K@.@..R.jk.c.
    0020  fa 27 1e d2 da 65 a3 1e f4 5d ad 39 7b b0 80 10   .'...e...].9{...
    0030  00 2e 82 4c 00 00 01 01 08 0a 71 e1 39 1a 38 b9   ...L......q.9.8.
    0040  c6 1b                                             ..
      
      0.998488 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [FIN, ACK] Seq=33 Ack=3 Win=524280 Len=0 TSV=951698979 TSER=1910585626
        
    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 34 14 a7 40 00 32 06 b1 17 63 ae fa 27 b8 6a   .4..@.2...c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b b0 a3 1e f4 5d 80 11   k..e...9{....]..
    0030  ff ff 91 1f 00 00 01 01 08 0a 38 b9 c6 23 71 e1   ..........8..#q.
    0040  39 1a                                             9.
      
      0.998588 184.106.107.229 -> 99.174.250.39 TCP 7890 > 55909 [FIN, ACK] Seq=3 Ack=34 Win=5888 Len=0 TSV=1910585711 TSER=951698979
        
    0000  00 00 0c 9f f0 01 40 40 ac c4 ec b0 08 00 45 00   ......@@......E.
    0010  00 34 cd 4c 40 00 40 06 eb 51 b8 6a 6b e5 63 ae   .4.L@.@..Q.jk.c.
    0020  fa 27 1e d2 da 65 a3 1e f4 5d ad 39 7b b1 80 11   .'...e...].9{...
    0030  00 2e 82 4c 00 00 01 01 08 0a 71 e1 39 6f 38 b9   ...L......q.9o8.
    0040  c6 23                                             .#
      
      1.073163 99.174.250.39 -> 184.106.107.229 TCP 55909 > 7890 [ACK] Seq=34 Ack=4 Win=524280 Len=0 TSV=951698980 TSER=1910585711
        
    0000  40 40 ac c4 ec b0 c8 4c 75 f5 eb 3f 08 00 45 e0   @@.....Lu..?..E.
    0010  00 34 1d ea 40 00 32 06 a7 d4 63 ae fa 27 b8 6a   .4..@.2...c..'.j
    0020  6b e5 da 65 1e d2 ad 39 7b b1 a3 1e f4 5e 80 10   k..e...9{....^..
    0030  ff ff 90 c8 00 00 01 01 08 0a 38 b9 c6 24 71 e1   ..........8..$q.
    0040  39 6f                                             9o
</markdown>