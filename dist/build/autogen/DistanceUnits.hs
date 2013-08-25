module RunningDistance (
          Distance(..)
        , Microinch
        , Mil
        , Line
        , Barleycorn
        , Finger
        , Inch
        , Nail
        , Palm
        , Foot
        , Cubit
        , Yard
        , Ell
        , Fathom
        , Perch
        , Rod
        , Chain
        , Furlong
        , Mile
        , Meter
        , Kilometer
        , addDist
        , subDist
        , convertDist
        )
where

import Data.Data(Data)
import Data.List(isPrefixOf)

-- | A generic class that defines all the units of measure in running.
-- The base unit of meters was chosen because Metric is orders more sane than
-- the English system

class Distance d where
    -- | Converts the given unit of distance into meters
    toMeters   :: d -> Meter
    -- | Converts the given number of meters into the unit of measure
    fromMeters :: Meter -> d
    -- | Yields the given distance as a float
    toFloat    :: d -> Float
--------------------------------------------
-- | Functions

-- | Add two distances together to get a common distance
addDist :: (Distance a, Distance b, Distance c) => a -> b -> c
addDist x y = fromMeters (toMeters x + toMeters y)

-- | Find the difference between two distcances
subDist :: (Distance a, Distance b, Distance c) => a -> b -> c
subDist x y = fromMeters (toMeters x - toMeters y)

-- | Convert one distance to another.
convertDist :: (Distance a, Distance b) => a -> b
convertDist = fromMeters . toMeters

-- | Reads a string into a corresponding distance
readDist :: (Float -> a) -> String ->
            Int -> String ->
            [(a, String)]
readDist builder unitstr prec str = processItems builder (readsPrec prec str)
    where processItems :: (Float -> a) -> [(Float,String)] -> [(a,String)]
          processItems builder  [] = []
          processItems builder ((a,s):rest)
            | unitstr `isPrefixOf` s =
                 (builder a, drop (length unitstr) s) : processItems builder rest
            | otherwise              =
               processItems builder rest
--------------------------------------------
-- | Unit Definitions

--
-- | English, Ascending
--

newtype Microinch = Microinch Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Microinch where
    toMeters (Microinch x)  = Meter     (x / 39370000)
    fromMeters (Meter x)    = Microinch (x * 39370000)
    toFloat (Microinch x)   = x

instance Show Microinch where
    show (Microinch x) = show x ++ "μ"
 
instance Read Microinch where
    readsPrec = readDist Microinch "μ"

--


newtype Mil = Mil Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Mil where
    toMeters    (Mil x)    = Meter (x / 39370)
    fromMeters  (Meter x)  = Mil   (x * 39370)
    toFloat     (Mil x)    = x

instance Show Mil where
    show (Mil x) = show x ++ "mil"
instance Read Mil where
    readsPrec = readDist Mil "mil"

--

newtype Line = Line Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Line where
    toMeters    (Line x)   = Meter (x / 472.4)
    fromMeters (Meter x)   = Line  (x * 472.4)
    toFloat (Line x)        = x

instance Show Line where
    show (Line x) = show x ++ "line" 
instance Read Line where
    readsPrec = readDist Line "line"

--

newtype Barleycorn = Barleycorn Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Barleycorn where
    toMeters (Barleycorn x) =  Meter ( x / 118.1)
    fromMeters (Meter x)    = Barleycorn ( x * 118.1)
    toFloat (Barleycorn x)  = x

instance Show Barleycorn where
    show (Barleycorn x) = show x ++ "barleycorn"
instance Read Barleycorn where
    readsPrec = readDist Barleycorn "barleycorn"

--

newtype Finger = Finger Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Finger where
    toMeters    (Finger x)  = Meter ( x / 44.99)
    fromMeters  (Meter x)   = Finger (x * 44.99)
    toFloat     (Finger x)  = x

instance Show Finger where
    show (Finger x) = show x ++ "finger"
instance Read Finger where 
    readsPrec = readDist Finger "finger"

--

newtype Inch = Inch Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Inch where
    toMeters   (Inch x)  = Meter ( x / 39.37)
    fromMeters (Meter x) = Inch  ( x * 39.37)
    toFloat    (Inch x)  = x

instance Show Inch where
    show (Inch x) = show x ++ "in"
instance Read Inch where
    readsPrec =  readDist Inch "in"

--

newtype Nail = Nail Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Nail where
    toMeters    (Nail x)  = Meter (x / 17.5)
    fromMeters  (Meter x) = Nail (x * 17.5)
    toFloat     (Nail x)  = x

instance Show Nail where
    show (Nail x) = show x ++ "nail"
instance Read Nail where
    readsPrec = readDist Nail "nail"

--

newtype Palm = Palm Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Palm where
    toMeters  (Palm x)   = Meter ( x / 13.12)
    fromMeters (Meter x) = Palm  ( x * 13.12)
    toFloat   (Palm x)   = x

instance Show Palm where
    show (Palm x) = show x ++ "palm"
instance Read Palm where
    readsPrec = readDist Palm "palm"

--

newtype Hand = Hand Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Hand where
    toMeters (Hand x) = Meter ( x / 9.843)
    fromMeters (Meter x) = Hand ( x * 9.843)
    toFloat (Hand x)    = x

instance Show Hand where
    show (Hand x) = show x ++ "hand"

instance Read Hand where
    readsPrec = readDist Hand "hand"

--

newtype Link = Link Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Link where 
    toMeters (Link x) = Meter ( x / 4.971)
    fromMeters (Meter x) = Link ( x * 4.971)
    toFloat (Link x) = x

instance Show Link where
    show (Link x) = show x ++ "link"

instance Read Link where
    readsPrec = readDist Link "link"

--

newtype Span = Span Float 
    deriving (Eq, Ord, Num, Fractional)

instance Distance Span where
    toMeters (Span x) = Meter ( x / 4.374)
    fromMeters (Meter x) = Span ( x * 4.374)
    toFloat (Span x) = x

instance Show Span where
    show (Span x) = show x ++ "span"
instance Read Span where
    readsPrec = readDist Span "span"

--

newtype Foot = Foot Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Foot where
    toMeters   (Foot x)  = Meter(x / 3.28084) 
    fromMeters (Meter x) = Foot (x * 3.28084)
    toFloat    (Foot x)  = x
instance Show Foot where
    show (Foot x) = show x ++ "ft"

instance Read Foot where
    readsPrec = readDist Foot "ft"

--

newtype Cubit = Cubit Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Cubit where
    toMeters (Cubit x) = Meter ( x / 2.187)
    fromMeters (Meter x) = Cubit (x * 2.187)
    toFloat (Cubit x) = x

instance Show Cubit where
    show (Cubit x) = show x ++ "cubit"
instance Read Cubit where
    readsPrec = readDist Cubit "cubit"

--

newtype Yard = Yard Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Yard where
    toMeters   (Yard x)  = Meter(x * (1/1.09361))
    fromMeters (Meter x) = Yard (x * 1.09361)
    toFloat    (Yard x)  = x
instance Show Yard where
    show (Yard x) = show x ++ "yd"

instance Read Yard where
    readsPrec = readDist Yard "yd"

--

newtype Ell = Ell Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Ell where
    toMeters (Ell x) = Meter ( x / 0.8749)
    fromMeters (Meter x) = Ell ( x* 0.8749)
    toFloat (Ell x) = x
   
instance Show Ell where
    show (Ell x) = show x ++ "ell"

instance Read Ell where
    readsPrec = readDist Ell "ell"

--

newtype Fathom = Fathom Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Fathom where
    toMeters (Fathom x) = Meter (x / 0.5468)
    fromMeters (Meter x) = Fathom ( x * 0.5468)
    toFloat (Fathom x) = x

instance Show Fathom where
    show (Fathom x) = show x ++ "fathom"
instance Read Fathom where
    readsPrec = readDist Fathom "fathom"

--

newtype Perch = Perch Float
    deriving (Eq, Ord, Num, Fractional)
    
instance Distance Perch where
    toMeters (Perch x) = Meter ( x / 0.1988)
    fromMeters (Meter x) = Perch ( x * 0.1988)
    toFloat (Perch x) = x

instance Show Perch where
    show (Perch x) = show x ++ "perch"

instance Read Perch where
    readsPrec = readDist Perch "perch"

--

newtype Rod = Rod Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Rod where
    toMeters (Rod x) = Meter ( x / 0.1988)
    fromMeters (Meter x) = Rod ( x * 0.1988)
    toFloat (Rod x) = x

instance Show Rod where
    show (Rod x) = show x ++ "rd"
instance Read Rod where
    readsPrec = readDist Rod "rd"

--    

newtype Chain = Chain Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Chain where
    toMeters (Chain x) = Meter ( x / 0.04971)
    fromMeters (Meter x) = Chain ( x * 0.04971)
    toFloat (Chain x) = x

instance Show Chain where
    show (Chain x) = show x ++ "chain"
instance Read Chain where
    readsPrec = readDist Chain "chain"

--

newtype Furlong = Furlong Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Furlong where
    toMeters (Furlong x) = Meter ( x / 0.004971)
    fromMeters (Meter x) = Furlong ( x * 0.004971)
    toFloat (Furlong x) = x

instance Show Furlong where
    show (Furlong x) = show x ++ "furlong"
instance Read Furlong where
    readsPrec = readDist Furlong "furlong"

--

newtype Mile = Mile Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Mile where
    toMeters   (Mile x)  = Meter(x*1609.34)
    fromMeters (Meter x) = Mile (x*(1/1609.34))
    toFloat    (Mile x)  = x
instance Show Mile where
    show (Mile x) = show x ++ "mi"

instance Read Mile where
    readsPrec = readDist Mile "mi"

--

newtype League = League Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance League where
    toMeters (League x) = Meter (x / 0.0002071)
    fromMeters ( Meter x) = League ( x * 0.0002071)
    toFloat (League x) = x

instance Show League where 
    show (League x)  = show x ++ "league"
instance Read League where
    readsPrec = readDist League "league"

--
-- | Metric Ascending
--

newtype Angstrom = Angstrom Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Angstrom where
    toMeters (Angstrom x) = Meter       (x/(10*10))
    fromMeters (Meter x)  = Angstrom    (x*10*10) 
    toFloat (Angstrom x)  = x
--

newtype Nanometer  = Nanometer Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Nanometer where
    toMeters (Nanometer x) = Meter      (x/(10**9))  
    fromMeters (Meter x)   = Nanometer  (x*(10**9))
    toFloat  (Nanometer x) = x

instance Show Nanometer where
    show (Nanometer x) = show x ++ "nm"
instance Read Nanometer where
    readsPrec = readDist Nanometer "nm"

--

type Micron     = Micrometer
newtype Micrometer = Micrometer Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Micrometer where
    toMeters (Micrometer x) = Meter     (x/1000000)
    fromMeters (Meter x)    = Micrometer(x*1000000)
    toFloat (Micrometer x)   = x
instance Show Micrometer where
    show (Micrometer x) = show x ++ "µm"

instance Read Micrometer where
    readsPrec = readDist Micrometer "µm"

--

newtype Millimeter = Millimeter Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Millimeter where
    toMeters (Millimeter x) = Meter    (x/1000)
    fromMeters (Meter x)    = Millimeter(x*1000)
    toFloat (Millimeter x)   = x
instance Show Millimeter where
    show (Millimeter x) = show x ++ "mm"

instance Read Millimeter where
    readsPrec = readDist Millimeter "mm"

--

newtype Meter = Meter Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Meter where
    toMeters   x = x
    fromMeters x = x
    toFloat (Meter x) = x
instance Show Meter where
    show (Meter x) = show x ++ "m"

instance Read Meter where
    readsPrec = readDist Meter "m"

--

newtype Kilometer = Kilometer Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Kilometer where
    toMeters (Kilometer x) = Meter    (x*1000)
    fromMeters (Meter x)   = Kilometer(x*0.001)
    toFloat (Kilometer x)  = x

instance Show Kilometer where
    show (Kilometer x) = show x ++ "km"

instance Read Kilometer where
    readsPrec = readDist Kilometer "km"

--
-- | Astronomical, ascending
--

newtype AU = AU Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance AU where
    toMeters (AU x) = Meter ( x / 0.000000000006685)
    fromMeters (Meter x) = AU (x * 0.000000000006685)
    toFloat (AU x) = x

instance Show AU where
    show (AU x) = show x ++ "au"
instance Read AU where
    readsPrec = readDist AU "au"

--

newtype Lightyear = Lightyear Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Lightyear where
    toMeters (Lightyear x) = Meter (x / (1.057*(10**(-16))))
    fromMeters (Meter x) = Lightyear ( x * (1.057*(10**(-16))))
    toFloat (Lightyear x) = x

instance Show Lightyear where
    show (Lightyear x) = show x ++ "lightyear"
instance Read Lightyear where
    readsPrec = readDist Lightyear "lightyear"

--

newtype Parsec = Parsec Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance Parsec where
    toMeters (Parsec x) = Meter ( x / (3.241*(10**(-17))))
    fromMeters (Meter x) = Parsec (x *  (3.241*(10**(-17))))
    toFloat (Parsec x) = x

instance Show Parsec where
    show (Parsec x) = show x ++ "pc"
instance Read Parsec where
    readsPrec = readDist Parsec "pc"

--

newtype RedShift = RedShift Float
    deriving (Eq, Ord, Num, Fractional)

instance Distance RedShift where
    toMeters (RedShift x) = Meter ( x / (7.67*(10**(-17))))
    fromMeters (Meter x) = RedShift ( x * (7.67*(10**(-17))))
    toFloat (RedShift x) = x

instance Show RedShift where
    show (RedShift x) = show x ++ "z"
instance Read RedShift where
    readsPrec = readDist RedShift "z"
