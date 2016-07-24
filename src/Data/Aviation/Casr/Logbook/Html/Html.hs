{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Html.Html where

import Lucid
import Prelude

import Data.Aviation.Casr.Logbook
import Control.Lens
import Data.Digit
import Data.Time
import Data.String
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text(Text)
import qualified Data.Text as Text

data AircraftUsageExpense =
  AircraftUsageExpense {
    _aircraftusageexpenseperhour :: Int
  , _aircraftusageexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftUsageExpense

data AircraftLandingExpense =
  AircraftLandingExpense {
    _aircraftlandingexpenseamount :: Int
  , _aircraftlandingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftLandingExpense

data AircraftFlightExpense =
  ExpenseAircraftUsage AircraftUsageExpense
  | ExpenseAircraftLanding AircraftLandingExpense
  deriving (Eq, Ord, Show)

makeClassyPrisms ''AircraftFlightExpense

data SimulatorFlightExpense =
  SimulatorFlightExpense {
    _simulatorflightexpenseperhour :: Int
  , _simulatorflightexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightExpense

data ExamExpense =
  ExamExpense {
    _examexpenseamount :: Int
  , _examexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''ExamExpense

data BriefingExpense =
  BriefingExpense {
    _briefingexpenseperhour :: Int
  , _briefingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''BriefingExpense

data Visualisation =
  Doarama {
    _doaramaid :: String
  , _oembedid :: String
  , _doaramaname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Visualisation

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ImageType

data Image =
  Image {
    _imageuri :: String
  , _imagename :: Maybe String
  , _imagetype :: ImageType
  } deriving (Eq, Ord, Show)
  
makeClassy ''Image

data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageTrackLog ImageType
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TrackLogType

data TrackLog =
  TrackLog {
    _trackloguri :: String
  , _tracklogtype :: TrackLogType
  , _tracklogsource :: (Maybe String)
  , _tracklogname :: (Maybe String)
  } deriving (Eq, Ord, Show)
  
makeClassy ''TrackLog

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)
  
makeClassyPrisms ''VideoType

data Video =
  Video {
    _videouri :: String
  , _videoname :: Maybe String
  , _videotype :: VideoType
  } deriving (Eq, Ord, Show)

makeClassy ''Video

data Expense =
  Expense {
    _expensecents :: Int
  , _expensetime :: Maybe TimeAmount
  , _expensedate :: Day
  , _expensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''Expense

data AircraftFlightMeta =
  AircraftFlightMeta {
    _tracklogs :: [TrackLog]
  , _visualisations :: [Visualisation]
  , _images :: [Image]
  , _videos :: [Video]
  , _expenses :: [AircraftFlightExpense]
  } deriving (Eq, Ord, Show)

makeClassy '' AircraftFlightMeta

----

tonymorris ::
  Aviator
tonymorris =
  dobaviator
    "Morris"
    "Tony"
    [x1, x0, x0, x7, x0, x3, x6]
    (fromGregorian 1977 11 2)
    []

michaelward ::
  Aviator
michaelward =
  aviatorwithname
    "Ward"
    "Michael"
   
ryanlow ::
  Aviator
ryanlow =
  aviatorwithname
    "Low"
    "Ryan"

davidschofield ::
  Aviator
davidschofield =
  nodobaviator
    "Schofield"
    "David"
    [x5, x8, x9, x5, x2, x2]
    [Rating "FIR II" Nothing]

damienboyer ::
  Aviator
damienboyer =
  aviatorwithname
    "Boyer"
    "Damien"

kenosbourne ::
  Aviator
kenosbourne =
  aviatorwithname
    "Osbourne"
    "Ken"

vhwkm ::
  Aircraft
vhwkm =
  singleaircraft
    "1980 American Champion Citabria 7GCBC"
    "VH-WKM"

vhldo ::
  Aircraft
vhldo =
  singleaircraft
    "2011 Cessna 162 Skycatcher"
    "VH-LDO"

vhafr ::
  Aircraft
vhafr =
  singleaircraft
    "2000 Cessna 172S Skyhawk"
    "VH-AFR"

vhvvo ::
  Aircraft
vhvvo =
  singleaircraft
    "2000 Cessna 172R Skyhawk"
    "VH-VVO"

vhzwy ::
  Aircraft
vhzwy =
  singleaircraft
    "2000 Cessna 172S Skyhawk"
    "VH-ZWY"

flightone ::
  Location
flightone =
  Location
    "Flight One"
    (-27.566768)
    153.014955

flightoneC162AircraftUsageExpense ::
  String
  -> AircraftFlightExpense
flightoneC162AircraftUsageExpense =
  ExpenseAircraftUsage . AircraftUsageExpense 34100

flightoneC172AircraftUsageExpense ::
  String
  -> AircraftFlightExpense
flightoneC172AircraftUsageExpense =
  ExpenseAircraftUsage . AircraftUsageExpense 41800

vhldoUnderInstruction ::
  AircraftFlightExpense
vhldoUnderInstruction =
  flightoneC162AircraftUsageExpense "VH-LDO Under Instruction"

vhldoLanding ::
  AircraftFlightExpense
vhldoLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 2979 "LDO Landing & Terminal Charge")  

vhafrUnderInstruction ::
  AircraftFlightExpense
vhafrUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-AFR Under Instruction"

vhafrLanding ::
  AircraftFlightExpense
vhafrLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "AFR Landing & Terminal Charge")

vhzwyUnderInstruction ::
  AircraftFlightExpense
vhzwyUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction"

vhzwyLanding ::
  AircraftFlightExpense
vhzwyLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "ZWY Landing & Terminal Charge")

vhvvoUnderInstruction ::
  AircraftFlightExpense
vhvvoUnderInstruction =
  flightoneC172AircraftUsageExpense "VH-VVO Under Instruction"

vhvvoLanding ::
  AircraftFlightExpense
vhvvoLanding =
  ExpenseAircraftLanding (AircraftLandingExpense 3273 "VVO Landing & Terminal Charge")

preflightBriefing ::
  Briefing
preflightBriefing =
  Briefing "Pre-flight Inspection" flightone (dayonly (fromGregorian 2015 12 10)) michaelward (parttimeamount x5)

preflightBriefingMeta ::
  [BriefingExpense]
preflightBriefingMeta =
  [BriefingExpense 7700 "Briefing - pre flight inspection"] 

effectofcontrols ::
  AircraftFlight
effectofcontrols =
  noif_dualonlyflight
    "Effects of Controls"
    vhldo
    michaelward
    (day 1 x2)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 14)))    

effectofcontrolsMeta ::
  AircraftFlightMeta
effectofcontrolsMeta =
  AircraftFlightMeta
    []
    []
    []
    []
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]
  
effectofcontrolsBriefing ::
  Briefing
effectofcontrolsBriefing =
  Briefing "Effect of controls" flightone (dayonly (fromGregorian 2015 12 14)) michaelward (parttimeamount x8)

effectofcontrolsBriefingMeta ::
  [BriefingExpense]
effectofcontrolsBriefingMeta =
  [BriefingExpense 7700 "Briefing - Operation and effects"] 

straightandlevelBriefing :: 
  Briefing
straightandlevelBriefing =
  Briefing "Straight and Level" flightone (dayonly (fromGregorian 2015 12 18)) michaelward (TimeAmount 1 x0)

straightandlevelBriefingMeta ::
  [BriefingExpense]
straightandlevelBriefingMeta =
  [BriefingExpense 7700 "Briefing - Basic"] 

straightandlevel ::
  AircraftFlight
straightandlevel =
  noif_dualonlyflight
    "Straight & Level"
    vhldo
    michaelward
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 18)))    
    
straightandlevelMeta ::
  AircraftFlightMeta
straightandlevelMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151218-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151218-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "595690"
        "6rAdypE"
        Nothing
    ]
    []
    [
      Video
        "13BVior4VmY"
        (Just "Head camera")
        YouTube
    ]
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

climbinganddescendingBriefing ::
  Briefing
climbinganddescendingBriefing =
  Briefing "Climbing and Descending" flightone (dayonly (fromGregorian 2015 12 20)) ryanlow (TimeAmount 1 x0)

climbinganddescendingBriefingMeta ::
  [BriefingExpense]
climbinganddescendingBriefingMeta =
  [BriefingExpense 7700 "Briefing - Basic"] 

climbinganddescending ::
  AircraftFlight
climbinganddescending =
  noif_dualonlyflight
    "Climbing & Descending"
    vhldo
    michaelward
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2015 12 20)))    
    
climbinganddescendingMeta ::
  AircraftFlightMeta
climbinganddescendingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151220-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151220-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "596790"
        "6rZrPp6"
        Nothing
    ]
    []
    [
      Video
        "8tZ8kxsVz6E"
        (Just "Head camera")
        YouTube
    ]
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

turning ::
  AircraftFlight
turning =
  noif_dualonlyflight
    "Turning"
    vhldo
    ryanlow
    (day 1 x3)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 4)))    
    
turningMeta ::
  AircraftFlightMeta
turningMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160104-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160104-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "607004"
        "6x5Az8e"
        Nothing
    ]
    []
    []
    [
      vhldoUnderInstruction
    , ExpenseAircraftUsage (AircraftUsageExpense 1000 "Concierge - Headset rental")
    , vhldoLanding
    ]

stallingBriefing1 :: 
  Briefing
stallingBriefing1 =
  Briefing "Stalling" flightone (dayonly (fromGregorian 2016 1 4)) michaelward (TimeAmount 1 x0)

stallingBriefing1Meta ::
  [BriefingExpense]
stallingBriefing1Meta =
  [BriefingExpense 7700 "Briefing - stalling"] 

stalling1 ::
  AircraftFlight
stalling1 =
  noif_dualonlyflight
    "Stalling (1)"
    vhldo
    davidschofield
    (day 1 x3)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 8)))    
    
stalling1Meta ::
  AircraftFlightMeta
stalling1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160108-vh-ldo.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160108-vh-ldo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "611980"
        "eYQA7ve"
        Nothing
    ]
    []
    []
    [
      vhldoUnderInstruction
    , vhldoLanding
    ]

stallingBriefing2 :: 
  Briefing
stallingBriefing2 =
  Briefing "Stalling" flightone (dayonly (fromGregorian 2016 1 8)) davidschofield (TimeAmount 1 x3)

stallingBriefing2Meta ::
  [BriefingExpense]
stallingBriefing2Meta =
  [BriefingExpense 7700 "Briefing - basic"] 

stalling2 ::
  AircraftFlight
stalling2 =
  noif_dualonlyflight
    "Stalling (2)"
    vhafr
    davidschofield
    (day 1 x4)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 15)))    
    
stalling2Meta ::
  AircraftFlightMeta
stalling2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160115-vh-afr.gpx"
        Gpx
        (Just "Garmin 62s")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160115-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "617610"
        "k8p38wk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR dual -ab-initio consolidation"
    , vhafrLanding
    ]
  
circuits1 ::
  AircraftFlight
circuits1 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 22)))    
    
circuits1Meta ::
  AircraftFlightMeta
circuits1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160122-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160122-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "622258"
        "k0nXwd6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
        Nothing
        Jpg
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
        Nothing
        Jpg 
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
        Nothing
        Jpg 
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
        Nothing
        Jpg      
    ]
    [
      Video
        "gz8Ivcjas9o"
        (Just "Head camera")
        YouTube
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR dual/ circuits #1"
    , vhafrLanding
    ]
  
circuits2 ::
  AircraftFlight
circuits2 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 1 29)))    
    
circuits2Meta ::
  AircraftFlightMeta
circuits2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160129-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160129-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "622258"
        "k0nXwd6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
        Nothing
        Jpg
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
        Nothing
        Jpg 
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
        Nothing
        Jpg 
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
        Nothing
        Jpg      
    ]
    [
      Video
        "gz8Ivcjas9o"
        (Just "Head camera")
        YouTube
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]
  
circuits3 ::
  AircraftFlight
circuits3 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 5)))
    
circuits3Meta ::
  AircraftFlightMeta
circuits3Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160205-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160205-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "635027"
        "eBdaGlE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR circuits #3"
    , vhafrLanding
    ]
  
circuits4 ::
  AircraftFlight
circuits4 =
  noif_dualonlyflight
    "Circuits"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 12)))    
    
circuits4Meta ::
  AircraftFlightMeta
circuits4Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160212-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160212-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "640001"
        "E2y3mnk"
        Nothing
    ]
    []
    [
      Video
        "e7UcjgxOmDw"
        (Just "Head camera")
        YouTube
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR circuits #4"
    , vhafrLanding
    ]

circuitemergenciesBriefing1 ::
  Briefing
circuitemergenciesBriefing1 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 12)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing1Meta ::
  [BriefingExpense]
circuitemergenciesBriefing1Meta =
  [BriefingExpense 7700 "Briefing - Basic"] 

circuitemergencies1 ::
  AircraftFlight
circuitemergencies1 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 2 18)))    
    
circuitemergencies1Meta ::
  AircraftFlightMeta
circuitemergencies1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160218-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160218-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "644663"
        "Ep1r9P6"
        Nothing
    ]
    [
      Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090347.jpg"
        Nothing
        Jpg
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090348.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090351.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090409.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090411.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090421.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090423.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090432.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090436.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090441.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090446.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090452.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090454.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090459.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090501.jpg"
        Nothing
        Jpg            
    , Image
        "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090502.jpg"
        Nothing
        Jpg            
    ]
    [
      Video
        "EO4D4zFnpIU"
        (Just "Head camera")
        YouTube
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

circuitemergenciesBriefing2 ::
  Briefing
circuitemergenciesBriefing2 =  
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 18)) davidschofield (TimeAmount 1 x0)

circuitemergenciesBriefing2Meta ::
  [BriefingExpense]
circuitemergenciesBriefing2Meta =
  [BriefingExpense 7700 "Briefing - Basic cct emer"] 

firstsoloexam ::
  Exam
firstsoloexam =
  dayonlyexam
    "First Solo Theory Exam"
    flightone
    (fromGregorian 2016 2 25)
    davidschofield
    31
    40

firstsoloexamMeta ::
  [ExamExpense]
firstsoloexamMeta =
  [ExamExpense 13200 "EXAM - In House Solo exam Pass 77%"]

circuitemergencies2 ::
  AircraftFlight
circuitemergencies2 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 4)))    

circuitemergencies2Meta ::
  AircraftFlightMeta
circuitemergencies2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160304-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160304-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "674600"
        "6XbQDjk"
        Nothing
    ]
    []
    [
      Video
        "jWUijREg4aE"
        (Just "Head camera")
        YouTube
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

circuitemergenciesBriefing3 ::
  Briefing
circuitemergenciesBriefing3 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 2 25)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing3Meta ::
  [BriefingExpense]
circuitemergenciesBriefing3Meta =
  [BriefingExpense 7700 "Briefing - CCT emergencies (DS)"] 

areasoloexam ::
  Exam
areasoloexam =
  dayonlyexam
    "First Solo Theory Exam"
    flightone
    (fromGregorian 2016 3 9)
    davidschofield
    38
    40

areasoloexamMeta ::
  [ExamExpense]
areasoloexamMeta =
  [ExamExpense 13200 "EXAM - In House area solo Pass!"]

circuitemergencies3 ::
  AircraftFlight
circuitemergencies3 =
  noif_dualonlyflight
    "Circuit Emergencies"
    vhzwy
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 24)))    
    
circuitemergencies3Meta ::
  AircraftFlightMeta
circuitemergencies3Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160324-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160324-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "691106"
        "6bPVnnk"
        Nothing
    ]
    []
    [
      Video
        "RNiYu3ZsfZw"
        (Just "Forward camera")
        YouTube
    ]
    [
      vhzwyUnderInstruction
    , vhzwyLanding
    ]

circuitemergenciesBriefing4 ::
  Briefing
circuitemergenciesBriefing4 =
  Briefing "Circuit Emergencies" flightone (dayonly (fromGregorian 2016 3 4)) davidschofield (parttimeamount x7)

circuitemergenciesBriefing4Meta ::
  [BriefingExpense]
circuitemergenciesBriefing4Meta =
  [BriefingExpense 7700 "Briefing - circuits"] 

circuits5 ::
  AircraftFlight
circuits5 =
  noif_dualonlyflight
    "Circuits"
    vhzwy
    davidschofield
    (day 0 x9)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 30)))    
    
circuits5Meta ::
  AircraftFlightMeta
circuits5Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "696748"
        "6xV5dVk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction.9/solo .4"
    ]

circuitemergenciesBriefing5 ::
  Briefing
circuitemergenciesBriefing5 =    
  Briefing "Circuits" flightone (dayonly (fromGregorian 2016 3 9)) davidschofield (parttimeamount x5)

circuitemergenciesBriefing5Meta ::
  [BriefingExpense]
circuitemergenciesBriefing5Meta =
  [BriefingExpense 7700 "Briefing - Basic"]

firstsolo ::
  AircraftFlight
firstsolo =
  noif_commandonlyflight
    "Circuits (first solo)"
    vhzwy
    (day 0 x4)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 3 30)))    
    
firstsoloMeta ::
  AircraftFlightMeta
firstsoloMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "696748"
        "6xV5dVk"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-ZWY Under Instruction.9/solo .4"
    , vhzwyLanding
    ]

circuitemergenciesBriefing6 ::
  Briefing
circuitemergenciesBriefing6 =
  Briefing "Circuits" flightone (dayonly (fromGregorian 2016 3 24)) davidschofield (parttimeamount x4)

circuitemergenciesBriefing6Meta ::
  [BriefingExpense]
circuitemergenciesBriefing6Meta =
  [BriefingExpense 7700 "Briefing - Basic"]

circuitscrosswind1 ::
  AircraftFlight
circuitscrosswind1 =
  noif_dualonlyflight
    "Circuits (Crosswind)"
    vhafr
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 15)))    
    
circuitscrosswind1Meta ::
  AircraftFlightMeta
circuitscrosswind1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160415-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160415-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "711553"
        "k9nWdbE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction.9/solo .4"
    , vhafrLanding
    ]

circuitscrosswindBriefing :: 
  Briefing
circuitscrosswindBriefing =
  Briefing "Circuits Crosswind" flightone (dayonly (fromGregorian 2016 4 15)) davidschofield (parttimeamount x5)

circuitscrosswindBriefingMeta :: 
  [BriefingExpense]
circuitscrosswindBriefingMeta =
  [BriefingExpense 7700 "Briefing - X-Wind CCTs"]

circuitssolocheck1 ::
  AircraftFlight
circuitssolocheck1 =
  noif_dualonlyflight
    "Circuits (solo check)"
    vhafr
    davidschofield
    (day 0 x5)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 22)))    
    
circuitssolocheck1Meta ::
  AircraftFlightMeta
circuitssolocheck1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "719248"
        "6xVbR8k"
        Nothing
    ]
    []
    [
      Video
        "UR1IkF5RSh4"
        (Just "Forward camera")
        YouTube
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction/solo"
    ]

circuitssolo1 ::
  AircraftFlight
circuitssolo1 =
  noif_commandonlyflight
    "Circuits (solo)"
    vhafr
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 22)))    
    
circuitssolo1Meta ::
  AircraftFlightMeta
circuitssolo1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "719248"
        "6xVbR8k"
        Nothing
    ]
    []
    [
      Video
        "UR1IkF5RSh4"
        (Just "Forward camera")
        YouTube
    ]
    [
      flightoneC172AircraftUsageExpense "VH-AFR Under Instruction/solo"
    , vhafrLanding
    ]

circuitssolocheck2 ::
  AircraftFlight
circuitssolocheck2 =
  noif_dualonlyflight
    "Circuits (solo check)"
    vhvvo
    davidschofield
    (day 0 x6)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 27)))   
    
circuitssolocheck2Meta ::
  AircraftFlightMeta
circuitssolocheck2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "724934"
        "eo1yvBE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction.6/Solo.8"
    ]

circuitssolo2 ::
  AircraftFlight
circuitssolo2 =
  noif_commandonlyflight
    "Circuits (solo)"
    vhvvo
    (day 0 x8)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 4 27)))    
    
circuitssolo2Meta ::
  AircraftFlightMeta
circuitssolo2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "724934"
        "eo1yvBE"
        Nothing
    ]
    []
    []
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction.6/Solo.8"
    , vhvvoLanding
    ]

practiceforcedlandingsBriefing :: 
  Briefing
practiceforcedlandingsBriefing =
  Briefing "Practice Forced Landings" flightone (dayonly (fromGregorian 2016 5 9)) davidschofield (TimeAmount 1 x5)

practiceforcedlandingsBriefingMeta ::
  [BriefingExpense]
practiceforcedlandingsBriefingMeta =
  [BriefingExpense 7700 "Briefing - Basic"]

practiceforcedlandings1 ::
  AircraftFlight
practiceforcedlandings1 =
  noif_dualonlyflight
    "Practice Forced Landings"
    vhafr
    davidschofield
    (day 1 x0)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 9)))    
     
practiceforcedlandings1Meta ::
  AircraftFlightMeta
practiceforcedlandings1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160509-vh-afr.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160509-vh-afr.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "741813"
        "edWMLO6"
        Nothing
    ]
    []
    [
      Video
        "o1dx3hZuov0"
        (Just "Forward camera")
        YouTube
    , Video
        "6250595"
        (Just "Live Stream")
        Bambuser
    ]
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

practiceforcedlandings2 ::
  AircraftFlight
practiceforcedlandings2 =
  noif_dualonlyflight
    "Practice Forced Landings"
    vhvvo
    davidschofield
    (day 1 x2)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 14)))
    
practiceforcedlandings2Meta ::
  AircraftFlightMeta
practiceforcedlandings2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160514-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160514-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "748638"
        "6jAODJE"
        Nothing
    ]
    []
    [
      Video
        "vw9KjjqkOEg"
        (Just "Forward camera")
        YouTube
    , Video
        "6258610"
        (Just "Live Stream")
        Bambuser
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

steepturnsBriefing ::
  Briefing
steepturnsBriefing =
  Briefing "Steep Turns" flightone (dayonly (fromGregorian 2016 5 9)) davidschofield (TimeAmount 1 x5)

steepturnsBriefingMeta ::
  [BriefingExpense]
steepturnsBriefingMeta =
  [BriefingExpense 7700 "Briefing"]

steepturns ::
  AircraftFlight
steepturns =
  noif_dualonlyflight
    "Steep Turns"
    vhvvo
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 19)))    
    
steepturnsMeta ::
  AircraftFlightMeta
steepturnsMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160519-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160519-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "756397"
        "k9nNR4E"
        Nothing
    ]
    []
    [
      Video
        "ZEABSxrN4xM"
        (Just "Forward camera")
        YouTube
    , Video
        "6267124"
        (Just "Live Stream")
        Bambuser
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction - steep turns"
    , vhvvoLanding
    ]

sydneycircuit ::
  AircraftFlight
sydneycircuit =
  noif_dualonlyflight
    "Sydney Circuit, Stalls"
    vhwkm
    kenosbourne
    (day 1 x8)
    (pointsatdate "YSCN" ["PSP", "PAR", "LRF", "V1", "JIBN", "SECF"] "YSCN" (fromGregorian 2016 5 19))
   
sydneycircuitMeta ::
  AircraftFlightMeta
sydneycircuitMeta =
  AircraftFlightMeta
    []
    []
    []
    []
    [
      ExpenseAircraftUsage (AircraftUsageExpense 28844 "VH-WKM 1.8 hours dual")
    ]

steepturnsandsideslipping ::
  AircraftFlight
steepturnsandsideslipping =
  noif_dualonlyflight
    "Steep Turns and Sideslipping"
    vhvvo
    damienboyer
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 5 30)))    
    
steepturnsandsideslippingMeta ::
  AircraftFlightMeta
steepturnsandsideslippingMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160530-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160530-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "770143"
        "65oMwXk"
        Nothing
    ]
    []
    [
      Video
        "6288135"
        (Just "Live Stream")
        Bambuser
    ]
    [
      vhvvoUnderInstruction
    , vhvvoLanding
    ]

circuitscrosswind2 ::
  AircraftFlight
circuitscrosswind2 =
  noif_dualonlyflight
    "Circuits (Crosswind)"
    vhvvo
    davidschofield
    (day 0 x9)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 6)))    
  
circuitscrosswind2Meta ::
  AircraftFlightMeta
circuitscrosswind2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160606-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160606-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "778822"
        "eOrqL86"
        Nothing
    ]
    []
    [
      Video
        "OYuR7E1xjyg"
        (Just "Forward camera")
        YouTube            
    , Video
        "6300417"
        (Just "Live stream")
        Bambuser
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO x-wind circuits"
    , vhvvoLanding
    ]

areasolocheck ::
  AircraftFlight
areasolocheck =
  noif_dualonlyflight
    "Area Solo Check"
    vhvvo
    davidschofield
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 16)))    

areasolocheckMeta ::
  AircraftFlightMeta
areasolocheckMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160616-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160616-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "798173"
        "67W2vz6"
        Nothing
    ]
    []
    [
      Video
        "6318524"
        (Just "Live stream")
        Bambuser
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO Under Instruction (DS)"
    , vhvvoLanding
    ]

areasoloBriefing1 ::
  Briefing
areasoloBriefing1 =
  Briefing "Area Solo Preparation" flightone (dayonly (fromGregorian 2016 6 16)) davidschofield (parttimeamount x5)

areasoloBriefing1Meta ::
  [BriefingExpense]
areasoloBriefing1Meta =
  [BriefingExpense 7700 "Briefing - Area solo prep"]    

areasolo1 ::
  AircraftFlight
areasolo1 =
  noif_commandonlyflight
    "Area Solo"
    vhvvo
    (day 1 x5)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 17)))    
    
areasolo1Meta ::
  AircraftFlightMeta
areasolo1Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160617-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160617-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "801102"
        "6rabNGE"
        Nothing
    ]
    []
    [
      Video
        "zF51UQ377E8"
        (Just "Forward camera -- Eastern Departure")
        YouTube            
    , Video
        "AWLegaBJGFs"
        (Just "Forward camera -- Practice Forced Landings")
        YouTube            
    , Video
        "6320415"
        (Just "Live stream")
        Bambuser
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO area solo"
    , vhvvoLanding
    ]

areasoloBriefing2 ::
  Briefing
areasoloBriefing2 =
  Briefing "Area Solo Post Brief" flightone (dayonly (fromGregorian 2016 6 17)) davidschofield (parttimeamount x4)

areasoloBriefing2Meta ::
  [BriefingExpense]
areasoloBriefing2Meta =
  [BriefingExpense 7700 "Briefing - area solo pre & post brief"]

areasolo2 ::
  AircraftFlight
areasolo2 =
  noif_commandonlyflight
    "Area Solo"
    vhvvo
    (day 1 x1)
    (directcircuit (pointatdate "YBAF" (fromGregorian 2016 6 26)))    
    
areasolo2Meta ::
  AircraftFlightMeta
areasolo2Meta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160626-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160626-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "815958"
        "k0zbK26"
        Nothing
    ]
    []
    [
      Video
        "G0-RoTxRsGw"
        (Just "Eastern Arrival, fly-over")
        YouTube        
    ]
    [
      flightoneC172AircraftUsageExpense "VH-VVO 2nd area solo"
    , vhvvoLanding
    ]

frolexam ::
  Exam
frolexam =
  dayonlyexam
    "Flight Radiotelephone Operator Licence"
    flightone
    (fromGregorian 2016 6 26)
    davidschofield
    38
    40

frolexamMeta ::
  [ExamExpense]
frolexamMeta =
  [ExamExpense 17600 "Flight Radio Operators Licence", ExamExpense (-17600) "Flight Radio Operators Licence"]

areasoloBriefing3 :: 
  Briefing
areasoloBriefing3 =
  Briefing "Area Solo Post Brief" flightone (dayonly (fromGregorian 2016 6 26)) davidschofield (parttimeamount x4)

areasoloBriefing3Meta ::
  [BriefingExpense]
areasoloBriefing3Meta =
  [BriefingExpense 7700 "Briefing - Area Solo"]

basicinstrumentflightsim ::
  SimulatorFlight
basicinstrumentflightsim =
  dayonlysimulator
    "Basic Instrument Flight"
    (fromGregorian 2016 7 6)
    "ATD-02"
    [damienboyer]
    (parttimeamount x5)

basicinstrumentflightsimMeta ::
  [SimulatorFlightExpense]
basicinstrumentflightsimMeta =
  [SimulatorFlightExpense 26400 "Synthetic Trainer Under Instruction"]

basicinstrumentflightBriefing1 ::
  Briefing
basicinstrumentflightBriefing1 =
  Briefing "Basic Instrument Flight" flightone (dayonly (fromGregorian 2016 7 5)) damienboyer (TimeAmount 1 x0)

basicinstrumentflightBriefing1Meta ::
  [BriefingExpense]
basicinstrumentflightBriefing1Meta =
  [BriefingExpense 7700 "Briefing - Basic"]

basicinstrumentflight ::
  AircraftFlight
basicinstrumentflight =
  dualonlyflight
    "Basic Instrument Flight"
    vhafr
    davidschofield
    (day 1 x1)
    (directcircuit (runwayatdate "YBAF" "28R" (fromGregorian 2016 7 14)))
    (parttimeamount x8)

basicinstrumentflightMeta ::
  AircraftFlightMeta
basicinstrumentflightMeta =
  AircraftFlightMeta
    [
      TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160714-vh-vvo.gpx"
        Gpx
        (Just "Samsung Galaxy S4")
        Nothing
    , TrackLog
        "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160714-vh-vvo.png"
        (ImageTrackLog Png)
        (Just "gpsvisualizer.com")
        Nothing
    ]
    [
      Doarama
        "841382"
        "6jr3Rb6"
        Nothing
    ]
    []
    []
    [
      vhafrUnderInstruction
    , vhafrLanding
    ]

basicinstrumentflightBriefing2 ::
  Briefing
basicinstrumentflightBriefing2 =
  Briefing "Basic Instrument Flight Post Brief" flightone (dayonly (fromGregorian 2016 7 14)) davidschofield (parttimeamount x3)

basicinstrumentflightBriefing2Meta ::
  [BriefingExpense]
basicinstrumentflightBriefing2Meta =
  [BriefingExpense 7700 "Briefing - Basic"]

loog ::
  Logbook AircraftFlightMeta [SimulatorFlightExpense] [ExamExpense] [BriefingExpense]
loog =
  aviatorlogbook
    tonymorris
    [
      BriefingEntry preflightBriefing preflightBriefingMeta
    , AircraftFlightEntry effectofcontrols effectofcontrolsMeta
    , BriefingEntry effectofcontrolsBriefing effectofcontrolsBriefingMeta
    , AircraftFlightEntry straightandlevel straightandlevelMeta
    , BriefingEntry straightandlevelBriefing straightandlevelBriefingMeta
    , AircraftFlightEntry climbinganddescending climbinganddescendingMeta
    , BriefingEntry climbinganddescendingBriefing climbinganddescendingBriefingMeta
    , AircraftFlightEntry turning turningMeta
    , BriefingEntry stallingBriefing1 stallingBriefing1Meta
    , AircraftFlightEntry stalling1 stalling1Meta
    , BriefingEntry stallingBriefing2 stallingBriefing2Meta
    , AircraftFlightEntry stalling2 stalling2Meta
    , AircraftFlightEntry circuits1 circuits1Meta
    , AircraftFlightEntry circuits2 circuits2Meta
    , AircraftFlightEntry circuits3 circuits3Meta
    , AircraftFlightEntry circuits4 circuits4Meta
    , BriefingEntry circuitemergenciesBriefing1 circuitemergenciesBriefing1Meta
    , AircraftFlightEntry circuitemergencies1 circuitemergencies1Meta
    , BriefingEntry circuitemergenciesBriefing2 circuitemergenciesBriefing2Meta
    , ExamEntry firstsoloexam firstsoloexamMeta
    , BriefingEntry circuitemergenciesBriefing3 circuitemergenciesBriefing3Meta
    , AircraftFlightEntry circuitemergencies2 circuitemergencies2Meta
    , BriefingEntry circuitemergenciesBriefing4 circuitemergenciesBriefing4Meta
    , ExamEntry areasoloexam areasoloexamMeta
    , BriefingEntry circuitemergenciesBriefing5 circuitemergenciesBriefing5Meta 
    , AircraftFlightEntry circuitemergencies3 circuitemergencies3Meta
    , BriefingEntry circuitemergenciesBriefing6 circuitemergenciesBriefing6Meta 
    , AircraftFlightEntry circuits5 circuits5Meta
    , AircraftFlightEntry firstsolo firstsoloMeta
    , AircraftFlightEntry circuitscrosswind1 circuitscrosswind1Meta
    , BriefingEntry circuitscrosswindBriefing circuitscrosswindBriefingMeta
    , AircraftFlightEntry circuitssolocheck1 circuitssolocheck1Meta
    , AircraftFlightEntry circuitssolo1 circuitssolo1Meta
    , AircraftFlightEntry circuitssolocheck2 circuitssolocheck2Meta
    , AircraftFlightEntry circuitssolo2 circuitssolo2Meta
    , BriefingEntry practiceforcedlandingsBriefing practiceforcedlandingsBriefingMeta
    , AircraftFlightEntry practiceforcedlandings1 practiceforcedlandings1Meta
    , AircraftFlightEntry practiceforcedlandings2 practiceforcedlandings2Meta
    , BriefingEntry steepturnsBriefing steepturnsBriefingMeta
    , AircraftFlightEntry steepturns steepturnsMeta
    , AircraftFlightEntry sydneycircuit sydneycircuitMeta
    , AircraftFlightEntry steepturnsandsideslipping steepturnsandsideslippingMeta
    , AircraftFlightEntry circuitscrosswind2 circuitscrosswind2Meta
    , AircraftFlightEntry areasolocheck areasolocheckMeta
    , BriefingEntry areasoloBriefing1 areasoloBriefing1Meta
    , AircraftFlightEntry areasolo1 areasolo1Meta
    , BriefingEntry areasoloBriefing2 areasoloBriefing2Meta 
    , AircraftFlightEntry areasolo2 areasolo2Meta
    , ExamEntry frolexam frolexamMeta
    , BriefingEntry areasoloBriefing3 areasoloBriefing3Meta  
    , BriefingEntry basicinstrumentflightBriefing1 basicinstrumentflightBriefing1Meta
    , SimulatorFlightEntry basicinstrumentflightsim basicinstrumentflightsimMeta
    , AircraftFlightEntry basicinstrumentflight basicinstrumentflightMeta
    , BriefingEntry basicinstrumentflightBriefing2 basicinstrumentflightBriefing2Meta
    ]

----

-- todo move to casr-logbook

totalDayNight ::
  DayNight
  -> TimeAmount
totalDayNight (DayNight d n) =
  d `mappend` n

showCentsAsDollars ::
  Int
  -> String
showCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        "0.0" ++ [x]
      pos [x, y] =
        "0." ++ [y, x]
      pos (x:y:z) =
        reverse z ++ "." ++ [y, x]
  in  (if n < 0 then ('-':) else id) . pos . reverse . show . abs $ n

showThousandCentsAsDollars ::
  Int
  -> String
showThousandCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        [x] ++ "0.0"
      pos [x, y] =
        [x, y] ++ ".0"
      pos [x, y, z] =
        [x, y, z] ++ ".0"
      pos (x:y:z:r) =
        [x, y, z] ++ "." ++ r
      drop0 [] =
        []
      drop0 ('0':r) =
        r
      drop0 w =
        w
  in  (if n < 0 then ('-':) else id) . reverse . drop0 . pos . reverse . show . abs $ n

timeAmountBy10 ::
  TimeAmount
  -> Int
timeAmountBy10 (TimeAmount a b) =
  a * 10 + digit # b

----

writetest1 :: IO ()
writetest1 =
  renderToFile "/tmp/x.html" test1

test1 :: Html ()
test1 =
  doctypehtml_ (do head_ (title_ "title"); body_ test1')

test1' :: Html ()
test1' =
  do  htmlAircraftUsageExpense turning (AircraftUsageExpense 34107 "expense")
      hr_ []
      htmlVisualisation turning (Doarama "595690" "6rAdypE" Nothing)
      hr_ []
      htmlImage turning (Image "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg" Nothing Jpg)

htmlAircraftUsageExpense ::
  AircraftFlight
  -> AircraftUsageExpense
  -> Html ()
htmlAircraftUsageExpense fl (AircraftUsageExpense perhour name) =
  let z = totalDayNight (fl ^. daynight)
  in  div_ [class_ "AircraftUsageExpense"] .
        ul_ $
          do  when (not . null $ name) . li_ $
                do  span_ [class_ "AircraftUsageExpense_name_key"] "Aircraft usage: "
                    span_ [class_ "AircraftUsageExpense_name_value"] . fromString $ name
              li_ $
                do  span_ [class_ "AircraftUsageExpense_perhour_key"] "Per hour: "
                    span_ [class_ "AircraftUsageExpense_perhour_value"] . fromString . ('$':) . showCentsAsDollars $ perhour
              li_ $
                do  span_ [class_ "AircraftUsageExpense_expense_key"] "Expense: "
                    span_ [class_ "AircraftUsageExpense_expense"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour

htmlAircraftLandingExpense ::
  AircraftFlight
  -> AircraftLandingExpense
  -> Html ()
htmlAircraftLandingExpense _ (AircraftLandingExpense amount name) =
  div_ [class_ "AircraftLandingExpense"] .
    ul_ $
      do  when (not . null $ name) . li_ $
            do  span_ [class_ "AircraftLandingExpense_name_key"] "Aircraft landing: "
                span_ [class_ "AircraftLandingExpense_name_value"] . fromString $ name
          li_ $
            do  span_ [class_ "AircraftLandingExpense_expense_key"] "Expense: "
                span_ [class_ "AircraftLandingExpense_expense_value"] . fromString . ('$':) . showCentsAsDollars $ amount

htmlAircraftFlightExpense ::
  AircraftFlight
  -> AircraftFlightExpense
  -> Html ()
htmlAircraftFlightExpense fl (ExpenseAircraftUsage e) =
  htmlAircraftUsageExpense fl e
htmlAircraftFlightExpense fl (ExpenseAircraftLanding e) =
  htmlAircraftLandingExpense fl e

htmlSimulatorFlightExpense ::
  SimulatorFlight
  -> SimulatorFlightExpense
  -> Html ()
htmlSimulatorFlightExpense sf (SimulatorFlightExpense perhour name) =
  let z = sf ^. instrumentsimulatorTimeAmount
  in  div_ [class_ "SimulatorFlightExpense"] .
        ul_ $
          do  when (not . null $ name) . li_ $
                do  span_ [class_ "SimulatorFlightExpense_name_key"] "Simulator flight: "
                    span_ [class_ "SimulatorFlightExpense_name_value"] . fromString $ name
              li_ $
                do  span_ [class_ "SimulatorFlightExpense_perhour_key"] "Per hour: "
                    span_ [class_ "SimulatorFlightExpense_perhour_value"] . fromString . ('$':) . showCentsAsDollars $ perhour
              li_ $
                do  span_ [class_ "SimulatorFlightExpense_expense_key"] "Expense: "
                    span_ [class_ "SimulatorFlightExpenseSimulatorFlightExpense_expense"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour

htmlExamExpense ::
  Exam
  -> ExamExpense
  -> Html ()
htmlExamExpense _ (ExamExpense amount name) =
  div_ [class_ "ExamExpense"] .
    ul_ $
      do  when (not . null $ name) . li_ $
            do  span_ [class_ "ExamExpense_name_key"] "Exam: "
                span_ [class_ "ExamExpense_name_value"] . fromString $ name
          li_ $
            do  span_ [class_ "ExamExpense_expense_key"] "Expense: "
                span_ [class_ "ExamExpense_expense_value"] . fromString . ('$':) . showCentsAsDollars $ amount

htmlBriefingExpense ::
  Briefing
  -> BriefingExpense
  -> Html ()
htmlBriefingExpense br (BriefingExpense perhour name) =
  let z = br ^. briefingTimeAmount
  in  div_ [class_ "BriefingExpense"] .
        ul_ $
          do  when (not . null $ name) . li_ $
                do  span_ [class_ "BriefingExpense_name_key"] "Briefing: "
                    span_ [class_ "BriefingExpense_name_value"] . fromString $ name
              li_ $
                do  span_ [class_ "BriefingExpense_perhour_key"] "Per hour: "
                    span_ [class_ "BriefingExpense_perhour_value"] . fromString . ('$':) . showCentsAsDollars $ perhour
              li_ $
                do  span_ [class_ "BriefingExpense_expense_key"] "Expense: "
                    span_ [class_ "BriefingExpenseSimulatorFlightExpense_expense"] . fromString . ('$':) . showThousandCentsAsDollars $ timeAmountBy10 z * perhour

htmlVisualisation ::
  AircraftFlight
  -> Visualisation
  -> Html ()
htmlVisualisation _ (Doarama i e n) =
  let n' = fromMaybe "doarama visualisation" n
  in  do  a_ [href_ ("http://doarama.com/view/" <> Text.pack i)] $ 
            span_ [class_ "Visualisation_name"] (fromString n')
          p_ (iframe_ [src_ ("http://www.doarama.com/embed?k=" <> Text.pack e), width_ "560", height_ "315", termWith "allowfullscreen" [] "allowfullscreen"] "")

strImageType ::
  ImageType
  -> String
strImageType Jpg =
  "jpg"
strImageType Png =
  "png"
strImageType Gif =
  "gif"

htmlImage ::
  AircraftFlight
  -> Image
  -> Html ()
htmlImage _ (Image u n t) =
  let u' = fromString u      
      n' = fromMaybe ("Image (" ++ strImageType t ++ ")") n
  in  a_ [href_ u'] $
        img_ [src_ u', width_ "120", alt_ (Text.pack n')]

strTrackLogType ::
  TrackLogType
  -> String
strTrackLogType Gpx =
  "gpx"
strTrackLogType Kml =
  "kml"
strTrackLogType Kmz =
  "kmz"
strTrackLogType (ImageTrackLog i) =
  strImageType i

{-
ase ttype of 
            ImageLog _ ->              
                [
                  "  * "
                , case n of
                    Nothing ->
                      "*" ++ t ++ "*"
                    Just n' ->
                      n'
                , "\n\n    "
                , "<a href=\""
                , uri
                , "\"><img src=\""
                , uri
                , "\" width=\"360\" alt=\""
                , fromMaybe t name
                , "\"></a>"
                ]
            _ ->              
                [
                  "  * "
                , case n of
                    Nothing ->
                      ""
                    Just n' ->
                      n' ++ ": "
                , "["
                , t
                , "]("
                , uri
                , ")"
                -}

htmlTrackLog ::
  AircraftFlight
  -> TrackLog
  -> Html ()
htmlTrackLog fl (TrackLog u t s n) =
  let r = ""
  in  undefined                 

{-

data TrackLog =
  TrackLog {
    _trackloguri :: String
  , _tracklogtype :: TrackLogType
  , _tracklogsource :: (Maybe String)
  , _tracklogname :: (Maybe String)
  } deriving (Eq, Ord, Show)
  
makeClassy ''TrackLog

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)
  
makeClassyPrisms ''VideoType

data Video =
  Video {
    _videouri :: String
  , _videoname :: Maybe String
  , _videotype :: VideoType
  } deriving (Eq, Ord, Show)

makeClassy ''Video

data Expense =
  Expense {
    _expensecents :: Int
  , _expensetime :: Maybe TimeAmount
  , _expensedate :: Day
  , _expensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''Expense

data AircraftFlightMeta =
  AircraftFlightMeta {
    _tracklogs :: [TrackLog]
  , _visualisations :: [Visualisation]
  , _images :: [Image]
  , _videos :: [Video]
  , _expenses :: [AircraftFlightExpense]
  } deriving (Eq, Ord, Show)

makeClassy '' AircraftFlightMeta

-}

