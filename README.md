# Data.Codec

Tired of writing complementary `parseJSON`/`toJSON`, `peek`/`poke` or Binary `get`/`put` functions?

`codec` provides easy bidirectional serialization of plain Haskell records in any Applicative context.
All you need to do is provide a de/serializer for every record field in any order you like,
and you get a de/serializer for the whole structure. The type system ensures that you provide
every record exactly once. It also includes a library for general record construction in an Applicative context,
of which creating codecs is just one application.
  
JSON!
  
    userCodec :: JSONCodec User
    userCodec = obj "user object" $
    User
      $>> f_username      >-< "user"
      >>> f_userEmail     >-< "email"
      >>> f_userLanguages >-< "languages"
      >>> f_userReferrer  >-< opt "referrer"
  
    instance FromJSON User where
      parseJSON = parseVal userCodec
    
    instance ToJSON User where
      toJSON = produceVal userCodec
  
Bit fields!
  
    ipv4Codec :: BinaryCodec IPv4
    ipv4Codec = toBytes $
      IPv4
        $>> f_version         >-< word8 4
        >>> f_ihl             >-< word8 4
        >>> f_dscp            >-< word8 6
        >>> f_ecn             >-< word8 2
        >>> f_totalLength     >-< word16be 16
        >>> f_identification  >-< word16be 16
        >>> f_flags           >-< word8 3
        >>> f_fragmentOffset  >-< word16be 13
        >>> f_timeToLive      >-< word8 8
        >>> f_protocol        >-< word8 8
        >>> f_headerChecksum  >-< word16be 16
        >>> f_sourceIP        >-< word32be 32
        >>> f_destIP          >-< word32be 32
       
    instance Binary IPv4 where
      get = parse ipv4Codec
      put = produce ipv4Codec
  
Storable!
  
    timeSpecCodec :: ForeignCodec TimeSpec
    timeSpecCodec =
      TimeSpec
        $>> f_seconds     >-< field (#offset struct timespec, tv_sec)  cInt
        >>> f_nanoseconds >-< field (#offset struct timespec, tv_nsec) cInt
    
    instance Storable TimeSpec where
      peek = peekWith timeSpecCodec
      poke = pokeWith timeSpecCodec
      ...

All of these examples use the same types and logic for constructing Codecs,
and it's very easy to create Codecs for any parsing/serialization library.
