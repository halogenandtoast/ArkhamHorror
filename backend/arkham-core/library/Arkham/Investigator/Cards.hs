module Arkham.Investigator.Cards where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Name
import Arkham.Trait hiding (Supply)

investigator :: CardCode -> Name -> ClassSymbol -> [Trait] -> CardDef
investigator cardCode name classSymbol traits =
  (emptyCardDef cardCode name InvestigatorType)
    { cdClassSymbols = singleton classSymbol
    , cdCardTraits = setFromList traits
    , cdUnique = True
    , cdLevel = Nothing
    }

allInvestigatorCards :: Map CardCode CardDef
allInvestigatorCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ rolandBanks
      , daisyWalker
      , skidsOToole
      , agnesBaker
      , wendyAdams
      , zoeySamaras
      , rexMurphy
      , jennyBarnes
      , jimCulver
      , ashcanPete
      , markHarrigan
      , minhThiPhan
      , sefinaRousseau
      , akachiOnyele
      , williamYorick
      , lolaHayes
      , leoAnderson
      , ursulaDowns
      , finnEdwards
      , fatherMateo
      , calvinWright
      , carolynFern
      , joeDiamond
      , prestonFairmont
      , dianaStanley
      , ritaYoung
      , marieLambeau
      , tommyMuldoon
      , mandyThompson
      , tonyMorgan
      , lukeRobinson
      , patriceHathaway
      , sisterMary
      , amandaSharpe
      , trishScarborough
      , dexterDrake
      , silasMarsh
      , danielaReyes
      , normanWithers
      , montereyJack
      , lilyChen
      , bobJenkins
      , carsonSinclair
      , vincentLee
      , kymaniJones
      , aminaZidane
      , darrellSimmons
      , charlieKane
      , wilsonRichards
      , kateWinthrop
      , alessandraZorzi
      , nathanielCho
      , harveyWalters
      , winifredHabbamock
      , jacquelineFine
      , stellaClark
      , gloriaGoldberg
      , daisyWalkerParallel
      ]

allEncounterInvestigatorCards :: Map CardCode CardDef
allEncounterInvestigatorCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [bodyOfAYithian, gavriellaMizrah, jeromeDavids, valentinoRivas, pennyWhite]

withAlternate :: CardCode -> CardDef -> CardDef
withAlternate ccode = withAlternates [ccode]

withAlternates :: [CardCode] -> CardDef -> CardDef
withAlternates ccodes def = def {cdAlternateCardCodes = ccodes}

rolandBanks :: CardDef
rolandBanks =
  withAlternates ["01501", "98004"]
    $ investigator
      "01001"
      ("Roland Banks" <:> "The Fed")
      Guardian
      [Agency, Detective]

daisyWalker :: CardDef
daisyWalker =
  withAlternate "01502"
    $ investigator
      "01002"
      ("Daisy Walker" <:> "The Librarian")
      Seeker
      [Miskatonic]

skidsOToole :: CardDef
skidsOToole =
  withAlternate "01503"
    $ investigator
      "01003"
      ("\"Skids\" O'Toole" <:> "The Ex-Con")
      Rogue
      [Criminal]

agnesBaker :: CardDef
agnesBaker =
  withAlternate "01504"
    $ investigator "01004" ("Agnes Baker" <:> "The Waitress") Mystic [Sorcerer]

wendyAdams :: CardDef
wendyAdams =
  withAlternate "01505"
    $ investigator "01005" ("Wendy Adams" <:> "The Urchin") Survivor [Drifter]

zoeySamaras :: CardDef
zoeySamaras =
  investigator
    "02001"
    ("Zoey Samaras" <:> "The Chef")
    Guardian
    [Believer, Hunter]

rexMurphy :: CardDef
rexMurphy =
  investigator "02002" ("Rex Murphy" <:> "The Reporter") Seeker [Reporter]

jennyBarnes :: CardDef
jennyBarnes =
  withAlternate "98001" $ investigator "02003" ("Jenny Barnes" <:> "The Dilettante") Rogue [Drifter]

jimCulver :: CardDef
jimCulver =
  investigator "02004" ("Jim Culver" <:> "The Musician") Mystic [Performer]

ashcanPete :: CardDef
ashcanPete =
  investigator "02005" ("\"Ashcan\" Pete" <:> "The Drifter") Survivor [Drifter]

markHarrigan :: CardDef
markHarrigan =
  investigator "03001" ("Mark Harrigan" <:> "The Soldier") Guardian [Veteran]

minhThiPhan :: CardDef
minhThiPhan =
  investigator "03002" ("Minh Thi Phan" <:> "The Secretary") Seeker [Assistant]

sefinaRousseau :: CardDef
sefinaRousseau =
  investigator "03003" ("Sefina Rousseau" <:> "The Painter") Rogue [Artist]

akachiOnyele :: CardDef
akachiOnyele =
  investigator "03004" ("Akachi Onyele" <:> "The Shaman") Mystic [Sorcerer]

williamYorick :: CardDef
williamYorick =
  investigator
    "03005"
    ("William Yorick" <:> "The Gravedigger")
    Survivor
    [Warden]

lolaHayes :: CardDef
lolaHayes =
  investigator "03006" ("Lola Hayes" <:> "The Actress") Neutral [Performer]

leoAnderson :: CardDef
leoAnderson =
  investigator
    "04001"
    ("Leo Anderson" <:> "The Expedition Leader")
    Guardian
    [Veteran, Wayfarer]

ursulaDowns :: CardDef
ursulaDowns =
  investigator "04002" ("Ursula Downs" <:> "The Explorer") Seeker [Wayfarer]

finnEdwards :: CardDef
finnEdwards =
  investigator "04003" ("Finn Edwards" <:> "The Bootlegger") Rogue [Criminal]

fatherMateo :: CardDef
fatherMateo =
  investigator
    "04004"
    ("Father Mateo" <:> "The Priest")
    Mystic
    [Believer, Warden]

calvinWright :: CardDef
calvinWright =
  investigator
    "04005"
    ("Calvin Wright" <:> "The Haunted")
    Survivor
    [Cursed, Drifter]

bodyOfAYithian :: CardDef
bodyOfAYithian =
  ( investigator
      "04244"
      ("Body of a Yithian" <:> "Captive in Another Form")
      Neutral
      [Monster, Yithian]
  )
    { cdUnique = False
    }

carolynFern :: CardDef
carolynFern =
  withAlternate "98010"
    $ investigator
      "05001"
      ("Carolyn Fern" <:> "The Psychologist")
      Guardian
      [Medic]

joeDiamond :: CardDef
joeDiamond =
  investigator
    "05002"
    ("Joe Diamond" <:> "The Private Investigator")
    Seeker
    [Detective]

prestonFairmont :: CardDef
prestonFairmont =
  investigator
    "05003"
    ("Preston Fairmont" <:> "The Millionaire")
    Rogue
    [SilverTwilight, Socialite]

dianaStanley :: CardDef
dianaStanley =
  investigator
    "05004"
    ("Diana Stanley" <:> "The Redeemed Cultist")
    Mystic
    [Cultist, SilverTwilight]

ritaYoung :: CardDef
ritaYoung =
  investigator
    "05005"
    ("Rita Young" <:> "The Athlete")
    Survivor
    [Miskatonic]

marieLambeau :: CardDef
marieLambeau =
  withAlternate "99001"
    $ investigator
      "05006"
      ("Marie Lambeau" <:> "The Entertainer")
      Mystic
      [Performer, Sorcerer]

gavriellaMizrah :: CardDef
gavriellaMizrah =
  investigator
    "05046"
    ("Gavriella Mizrah" <:> "Private Security")
    Neutral
    [Veteran]

jeromeDavids :: CardDef
jeromeDavids =
  investigator
    "05047"
    ("Jerome Davids" <:> "Josef's Secretary")
    Neutral
    [Assistant, SilverTwilight]

valentinoRivas :: CardDef
valentinoRivas =
  investigator
    "05048"
    ("Valentino Rivas" <:> "Wealthy Philanthropist")
    Neutral
    [SilverTwilight, Socialite]

pennyWhite :: CardDef
pennyWhite =
  investigator
    "05049"
    ("Penny White" <:> "Josef's Housekeeper")
    Neutral
    [Assistant]

tommyMuldoon :: CardDef
tommyMuldoon =
  investigator
    "06001"
    ("Tommy Muldoon" <:> "The Rookie Cop")
    Guardian
    [Police, Warden]

mandyThompson :: CardDef
mandyThompson =
  investigator
    "06002"
    ("Mandy Thompson" <:> "The Researcher")
    Seeker
    [Assistant, Scholar]

tonyMorgan :: CardDef
tonyMorgan =
  investigator
    "06003"
    ("Tony Morgan" <:> "The Bounty Hunter")
    Rogue
    [Criminal, Hunter]

lukeRobinson :: CardDef
lukeRobinson =
  investigator
    "06004"
    ("Luke Robinson" <:> "The Dreamer")
    Mystic
    [Dreamer, Drifter, Wayfarer]

patriceHathaway :: CardDef
patriceHathaway =
  investigator
    "06005"
    ("Patrice Hathaway" <:> "The Violinist")
    Survivor
    [Performer, Cursed]

sisterMary :: CardDef
sisterMary =
  investigator
    "07001"
    ("Sister Mary" <:> "The Nun")
    Guardian
    [Believer, Blessed]

amandaSharpe :: CardDef
amandaSharpe =
  investigator
    "07002"
    ("Amanda Sharpe" <:> "The Student")
    Seeker
    [Miskatonic, Scholar]

trishScarborough :: CardDef
trishScarborough =
  investigator
    "07003"
    ("Trish Scarborough" <:> "The Spy")
    Rogue
    [Agency, Detective]

dexterDrake :: CardDef
dexterDrake =
  withAlternate "98016"
    $ investigator
      "07004"
      ("Dexter Drake" <:> "The Magician")
      Mystic
      [Sorcerer, Veteran]

silasMarsh :: CardDef
silasMarsh =
  withAlternate "98013"
    $ investigator
      "07005"
      ("Silas Marsh" <:> "The Sailor")
      Survivor
      [Drifter]

danielaReyes :: CardDef
danielaReyes =
  investigator
    "08001"
    ("Daniela Reyes" <:> "The Mechanic")
    Guardian
    [Entrepreneur]

normanWithers :: CardDef
normanWithers =
  withAlternate "98007"
    $ investigator
      "08004"
      ("Norman Withers" <:> "The Astronomer")
      Seeker
      [Miskatonic]

montereyJack :: CardDef
montereyJack =
  investigator
    "08007"
    ("Monterey Jack" <:> "The Archaeologist")
    Rogue
    [Wayfarer]

lilyChen :: CardDef
lilyChen =
  investigator
    "08010"
    ("Lily Chen" <:> "The Martial Artist")
    Mystic
    [Chosen, Warden]

bobJenkins :: CardDef
bobJenkins =
  investigator
    "08016"
    ("Bob Jenkins" <:> "The Salesman")
    Survivor
    [Entrepreneur]

carsonSinclair :: CardDef
carsonSinclair =
  investigator
    "09001"
    ("Carson Sinclair" <:> "The Butler")
    Guardian
    [Assistant]

vincentLee :: CardDef
vincentLee =
  investigator
    "09004"
    ("Vincent Lee" <:> "The Doctor")
    Seeker
    [Medic]

kymaniJones :: CardDef
kymaniJones =
  investigator
    "09008"
    ("Kymani Jones" <:> "The Security Consultant")
    Rogue
    [Criminal]

aminaZidane :: CardDef
aminaZidane =
  investigator
    "09011"
    ("Amina Zidane" <:> "The Operator")
    Mystic
    [Chosen, Cursed]

darrellSimmons :: CardDef
darrellSimmons =
  investigator
    "09015"
    ("Darrell Simmons" <:> "The Photographer")
    Survivor
    [Reporter]

charlieKane :: CardDef
charlieKane =
  investigator
    "09018"
    ("Charlie Kane" <:> "The Politician")
    Neutral
    [Civic, Socialite]

wilsonRichards :: CardDef
wilsonRichards =
  investigator
    "10001"
    ("Wilson Richards" <:> "The Handyman")
    Guardian
    [Drifter]

kateWinthrop :: CardDef
kateWinthrop =
  investigator
    "10004"
    ("Kate Winthrop" <:> "The Scientist")
    Seeker
    [Miskatonic, Scholar]

alessandraZorzi :: CardDef
alessandraZorzi =
  investigator
    "10009"
    ("Alessandra Zorzi" <:> "The Countess")
    Rogue
    [Drifter, Socialite]

nathanielCho :: CardDef
nathanielCho =
  investigator
    "60101"
    ("Nathaniel Cho" <:> "The Boxer")
    Guardian
    [Criminal, Warden]

harveyWalters :: CardDef
harveyWalters =
  investigator
    "60201"
    ("Harvey Walters" <:> "The Professor")
    Seeker
    [Miskatonic]

winifredHabbamock :: CardDef
winifredHabbamock =
  investigator
    "60301"
    ("Winifred Habbamock" <:> "The Aviatrix")
    Rogue
    [Criminal]

jacquelineFine :: CardDef
jacquelineFine =
  investigator
    "60401"
    ("Jacqueline Fine" <:> "The Psychic")
    Mystic
    [Clairvoyant]

stellaClark :: CardDef
stellaClark =
  investigator
    "60501"
    ("Stella Clark" <:> "The Letter Carrier")
    Survivor
    [Chosen, Civic]

gloriaGoldberg :: CardDef
gloriaGoldberg =
  investigator
    "98019"
    ("Gloria Goldberg" <:> "The Writer")
    Mystic
    [Clairvoyant]

daisyWalkerParallel :: CardDef
daisyWalkerParallel =
  investigator "90001" ("Daisy Walker" <:> "The Librarian") Seeker [Miskatonic]
