module RunningDistance (
          Distance(..)
        , Foot
        , Yard
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

-- | Returns weather or not a distance qualifies as an ultra marathon
-- ultraMarathon :: Distance a => a -> Bool


-- | Reads a string into a corresponding distance
readDist :: (Float -> a) -> String ->
            Int -> String ->
            [(a, String)]
readDist builder unitstr prec str = processItems builder (readsPrec prec str)
    where processItems :: (Float -> a) -> [(Float,String)] -> [(a,String)]
          processItems builder  [] = []
          processItems builder ((a,s):rest)
            | unitstr `isPrefixOf` s =
                 (builder a, drop (length unitstr) s) : (processItems builder rest)
            | otherwise              =
               processItems builder rest
--------------------------------------------
-- | Unit Definitions

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
