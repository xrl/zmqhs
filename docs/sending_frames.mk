<markdown>
Sending Data From ZMQHS to libzmq
====================

So, I've broken down the semantics of the ZMQ opening handshake and gotten data in to Haskell. Now let's send data out of a Haskell program and in to a C/libzmq program.

Previous Posts
-------------------

http://xrl.tureus.com/a-quick-post-on-zmqhs

http://xrl.tureus.com/haskell-and-binary-streams-parsing-with-field

http://xrl.tureus.com/black-box-reverse-engineering-zmq

Find the Code
-------------------

http://github.com/xrl/zmqhs

The Spec
-------------------

    more-frame  = length more body
    final-frame = length final body
    length      = OCTET / (%xFF 8OCTET)
    more        = %x01
    final       = %x00
    body        = *OCTET

Basics of Sockets
====================

Here are the basics of using a stream (aka, TCP) socket:

  1. Ask the OS to translate an IP/hostname to a native address structure 
  2. Allocate a socket descriptor with your OS using the address structre
  3. Perform the connection operation, establishing the connection
  4. Start sending data over the socket

Writing to Sockets in Haskell
--------------------

Haskell's Socket API is essentially a wrapper around your standard libraries. Here's how you perform those opening steps:

    open_connection = do
      addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
      let servinfo = head addrinfos
      sock <- S.socket (S.addrFamily servinfo) S.Stream S.defaultProtocol
      S.connect sock (S.addrAddress servinfo)
      connected <- S.sIsConnected sock
      case connected of
        True  -> putStrLn "connected!"
        False -> putStrLn "not connected!"
      return sock

This function does not return whether the operation was successful or not. I can think of three ways to handle the error: a parameterized return value (e.g., Either or Maybe), a tuple return value of (success,sock), or the error (fail).

Let's say we have a good strategy for handling failure and we're connected. ZMQHS works with ByteString datatypes so we can choose between regular and lazy.

And here's how you write to the socket

    import qualified Data.ByteString.Lazy as B
    import qualified Network.Socket.ByteString.Lazy as LSB
    send_and_read sock = do
      let opening_salvo = B.pack [0x01, 0x7E]
      LSB.send sock opening_salvo
      stuff <- LSB.recv sock 1024
      let outgoing_data = ZF.payload_response (B.pack [65,66,67,68,69])
      LSB.send sock outgoing_data

I'll explain those semantics later.

A Note on Strict vs Lazy ByteStrings
=======================

A ByteString is a binary blob with a length field. This is quite different from a Haskell list, which is backed by a linked list data structure. A lazy ByteString is similar to the strict version, but it's actually a linked list of strict ByteStrings (I've heard them called chords). As for their performance tradeoffs -- I'm not sure yet!

The reader side of things is strict (attoparsec works on strict ByteStrings) -- how about we make the writer side of things lazy?

Creating VS Parsing Frames
=======================

What I want in the end is a DRY way to send data in and out

  ZMQHS <------> Outside Word
    zmq_msg -> bytestring
    zmq_msg <- bytestring

Unfortunately, attoparsec does not work in the opposite direction! Instead I will use the Data.Binary.Put monad. Put lets me write words and bytestrings.

    import qualified Data.Binary.Put as P
    payload_response a_byte = do
        P.runPut (generator 0x7E a_byte)
    generator header body = do
        let header_len = 1
        let body_len   = B.length body
        let len = header_len + body_len
        case len of
            x | x < 256 -> P.putWord8    $ fromIntegral len
            otherwise   -> P.putWord8 0xFF >>
                          (P.putWord64be $ fromIntegral len)
        P.putWord8 header
        P.putLazyByteString body

With the Put monad you have to construct the generator then run it through runPut. The data you're writing to wire has to be passed in when creating the generator NOT when running the generator. It's similar to how you parse data: create a parser then run the parser against data.

Writing Out Covariant Datatypes
--------------------------

One of my bigger stumbling blocks was how to write the payload length to the stream. ZMQ framing gives you two options for describing the payload byte length: one byte with the values 0-254 OR 0xFF and an 8-byte size. The ByteString payload length function has this type definition:

    Data.ByteString.length :: ByteString -> Int

Compared to the Put monad's byte method:

    Data.Binary.Put.putWord8 :: GHC.Word.Word8 -> Put

The Int and Word8 values are not directly equivalent. The compiler will let you know you're doing something wrong. Let's look at what Haskell knows about the data types

    Prelude> :info Int
    data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in GHC.Types
    instance Bounded Int -- Defined in GHC.Enum
    instance Enum Int -- Defined in GHC.Enum
    instance Eq Int -- Defined in GHC.Base
    instance Integral Int -- Defined in GHC.Real
    instance Num Int -- Defined in GHC.Num
    instance Ord Int -- Defined in GHC.Base
    instance Read Int -- Defined in GHC.Read
    instance Real Int -- Defined in GHC.Real
    instance Show Int -- Defined in GHC.Show

    Prelude> :info GHC.Word.Word8
    data GHC.Word.Word8 = GHC.Word.W8# GHC.Prim.Word#
        -- Defined in GHC.Word
    instance Bounded GHC.Word.Word8 -- Defined in GHC.Word
    instance Enum GHC.Word.Word8 -- Defined in GHC.Word
    instance Eq GHC.Word.Word8 -- Defined in GHC.Word
    instance Integral GHC.Word.Word8 -- Defined in GHC.Word
    instance Num GHC.Word.Word8 -- Defined in GHC.Word
    instance Ord GHC.Word.Word8 -- Defined in GHC.Word
    instance Read GHC.Word.Word8 -- Defined in GHC.Word
    instance Real GHC.Word.Word8 -- Defined in GHC.Word
    instance Show GHC.Word.Word8 -- Defined in GHC.Word

They're both bounded integrals and I can do some range-checking logic to see whether to put the value in the 1-byte or 8-byte frame. Now, how to do that? Well, fromIntegral of course!

    fromIntegral :: (Num b, Integral a) => a -> b

Take something Integral and turn it into a more generic Num. So we'll take the ByteString lenght Int and turn it into a more generic Num and then let Haskell slim it down to a Word8. Type inference at work. We, the programmer, still have to worry about truncation (data loss), so we'll bin the values ourselves.

    let header_len = 1
    let body_len   = B.length body
    let len = header_len + body_len
    case len of
        x | x < 255 -> P.putWord8    $ fromIntegral len
        otherwise   -> P.putWord8 0xFF >>
                      (P.putWord64be $ fromIntegral len)

I should do upperbounds checking on the length but I will pass on that for now.

Back to the Generator
---------------------

    import qualified Data.Binary.Put as P
    payload_response a_byte = do
        P.runPut (generator 0x7E a_byte)
    generator header body = do
        let header_len = 1
        let body_len   = B.length body
        let len = header_len + body_len
        case len of
            x | x < 256 -> P.putWord8    $ fromIntegral len
            otherwise   -> P.putWord8 0xFF >>
                          (P.putWord64be $ fromIntegral len)
        P.putWord8 header
        P.putLazyByteString body


    generator :: Word8 -> ByteString -> P.PutM ()

The generator takes two arguments to give you an instance of the Put monad. Once it's satisfied you can use runPut

    Data.Binary.Put.runPut :: P.Put -> B.ByteString

And that's how you get bytes out of Haskell native datatypes.

Handshake
==================

You can't just send a well-formed frame to the libzmq program. You must first send it an empty message. This message is one empty frame: [0x01, 0x7E].

The 1-byte more/final byte is hex: 0x7E, bits: 0b01111110. Martin Sustrik (a ZMQ developer) chimed in to say the most significant bit is a bug in my version of libzmq. The least significant bit signifies this is the 'final' frame in the transmission.

  send_and_read sock = do
    let opening_salvo = B.pack [0x01, 0x7E]
    LSB.send sock opening_salvo
    --stuff <- LSB.recv sock 1024
    --ZF.debug_it stuff
    let outgoing_data = ZF.payload_response (B.pack [65,66,67,68,69])
    LSB.send sock outgoing_data

Testing it Out
==================

Let's make sure it works. I wrote a sample C program called oneframe which will either send or receive a ZMQ message. If receiving it just prints the contents to stdout. The sample program has a lot of error checking code to make sure I'm doing the right thing C-wise. Check it out: https://github.com/xrl/zmqhs/blob/master/test/c/oneframe/oneframe.c

I also wrote a minimal Haskell script: https://github.com/xrl/zmqhs/blob/master/test/send/OneFrame.hs . It connects to the receiver C app and sends one test frame with the message (B.pack [65,66,67,68,69]).

From OneFrame.hs's perspective:

    xavierlange $> ./test/send/OneFrame.hs 
    connected!
    1 7e 
    7

From oneframe.c's perspective:

    xavierlange $> ./test/c/oneframe/oneframe recv
    Server up. Waiting for message.
    oneframe receiver got ABCDE
    cleaning up sock... done!
    shutting down zmq... done!

Wrap Up
===================

I hope the little chat on converting number values was helpful. Once you've written a monadic parser the monadic generator is very similar. What took me a week to figure out parsing-wise took an evening to figure out generating-wise.

Possible future updates will be to use the Data.Binary.Bit.Put to reconstitute the flag data in to the wire protocol.

Next up: a solid data model for representing the ZMQ messages in the abstract. With an eye on serialization.
</markdown>