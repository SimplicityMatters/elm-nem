module Model exposing (..)


type alias Address =
    String


type alias PublicKey =
    String


type alias Balance =
    Int


type alias Proportion =
    Float


type alias AccountInfo =
    { address : Address -- "TALICELCD3XPH4FFI5STGGNSNSWPOTG5E4DS2TOS",
    , balance : Balance -- 124446551689680,
    , vestedBalance : Balance -- 1041345514976241,
    , importance : Proportion -- 0.010263666447108395,
    , publicKey : Maybe PublicKey -- "a11a1a6c17a24252e674d151713cdf51991ad101751e4af02a20c61b59f1fe1a",
    , harvestedBlocks : Int -- 645,

    --, label:  -- always null,
    -- , multisigInfo: -- {} ???
    }


type alias AccountMetaData =
    { status : String -- "LOCKED",
    , remoteStatus : String -- "ACTIVE",
    , cosignatoryOf : List AccountInfo -- [ <AccountInfo>, <AccountInfo> ],
    , cosignatories : List AccountInfo -- [ <AccountInfo>, <AccountInfo ] ,
    }


type alias AccountMetaDataPair =
    { account : AccountInfo
    , meta : AccountMetaData
    }
