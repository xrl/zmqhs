ZMQHS: Pure ZMQ
===============

Pure ZMQ. No FFI.

  http://rfc.zeromq.org/spec:13

  http://www.haskell.org/haskellwiki/DealingWithBinaryData

  http://stackoverflow.com/questions/3120796/haskell-testing-workflow/3120826#3120826

http://hpaste.org/53004#a53006

ZMQ ABNF
--------

    zmtp        = *connection
    
    connection  = greeting content
    greeting    = anonymous / identity
    anonymous   = %x01 final
    identity    = length final (%x01-ff) *OCTET
    
    message     = *more-frame final-frame
    more-frame  = length more body
    final-frame = length final body
    length      = OCTET / (%xFF 8OCTET)
    more        = %x01
    final       = %x00
    body        = *OCTET
    
    content     = *broadcast / *addressed / *neutral
    
    broadcast   = message
    
    addressed   = envelope message
    envelope    = *more-frame delimiter
    delimiter   = %x01 more
    
    neutral     = message

combinatorrent
--------------

    getAPMsg :: Int -> Parser Message
    getAPMsg l = do
        c <- A.anyWord8
        case c of
            0  -> return Choke
            1  -> return Unchoke
            2  -> return Interested
            3  -> return NotInterested
            4  -> (Have <$> apW32be)
            5  -> (BitField <$> (A.take (l-1)))
            6  -> (Request <$> apW32be <*> (Block <$> apW32be <*> apW32be))
            7  -> (Piece <$> apW32be <*> apW32be <*> A.take (l - 9))
            8  -> (Cancel <$> apW32be <*> (Block <$> apW32be <*> apW32be))
            9  -> (Port . fromIntegral <$> apW16be)
            0x0D -> (Suggest <$> apW32be)
            0x0E -> return HaveAll
            0x0F -> return HaveNone
            0x10 -> (RejectRequest <$> apW32be <*> (Block <$> apW32be <*> apW32be))
            0x11 -> (AllowedFast <$> apW32be)
            20 -> (ExtendedMsg <$> A.anyWord8 <*> A.take (l - 2))
            k  -> fail $ "Illegal parse, code: " ++ show k

Want Something That Works?
--------------------------

    A kind soul has already created a working FFI binding: zeromq-haskell

    http://hackage.haskell.org/package/zeromq-haskell

Contact
-------

@tureus or xrlange [at] tureus.com
