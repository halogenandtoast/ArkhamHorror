module Arkham.Investigator.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Name
import Arkham.Trait hiding ( Supply )

investigator :: CardCode -> Name -> ClassSymbol -> [Trait] -> CardDef
investigator cardCode name classSymbol traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardType = InvestigatorType
  , cdCardSubType = Nothing
  , cdClassSymbols = singleton classSymbol
  , cdSkills = mempty
  , cdCardTraits = setFromList traits
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
  , cdCriteria = mempty
  , cdOverrideActionPlayableIfCriteriaMet = False
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  , cdPlayableFromDiscard = False
  , cdStage = Nothing
  , cdSlots = []
  , cdCardInHandEffects = False
  , cdCardInDiscardEffects = False
  , cdCardInSearchEffects = False
  , cdAlternateCardCodes = []
  , cdArt = unCardCode cardCode
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  , cdCanReplace = True
  }

allInvestigatorCards :: HashMap CardCode CardDef
allInvestigatorCards = mapFromList $ concatMap
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
  , gavriellaMizrah
  , normanWithers
  , nathanielCho
  , harveyWalters
  , stellaClark
  , daisyWalkerParallel
  ]

allEncounterInvestigatorCards :: HashMap CardCode CardDef
allEncounterInvestigatorCards = mapFromList $ concatMap
  toCardCodePairs
  [ bodyOfAYithian ]

withAlternate :: CardCode -> CardDef -> CardDef
withAlternate ccode def = def { cdAlternateCardCodes = [ccode] }

rolandBanks :: CardDef
rolandBanks = withAlternate "01501" $ investigator
  "01001"
  ("Roland Banks" <:> "The Fed")
  Guardian
  [Agency, Detective]

daisyWalker :: CardDef
daisyWalker = withAlternate "01502" $ investigator
  "01002"
  ("Daisy Walker" <:> "The Librarian")
  Seeker
  [Miskatonic]

skidsOToole :: CardDef
skidsOToole = withAlternate "01503" $ investigator
  "01003"
  ("\"Skids\" O'Toole" <:> "The Ex-Con")
  Rogue
  [Criminal]

agnesBaker :: CardDef
agnesBaker = withAlternate "01504"
  $ investigator "01004" ("Agnes Baker" <:> "The Waitress") Mystic [Sorcerer]

wendyAdams :: CardDef
wendyAdams = withAlternate "01505"
  $ investigator "01005" ("Wendy Adams" <:> "The Urchin") Survivor [Drifter]

zoeySamaras :: CardDef
zoeySamaras = investigator
  "02001"
  ("Zoey Samaras" <:> "The Chef")
  Guardian
  [Believer, Hunter]

rexMurphy :: CardDef
rexMurphy =
  investigator "02002" ("Rex Murphy" <:> "The Reporter") Seeker [Reporter]

jennyBarnes :: CardDef
jennyBarnes =
  investigator "02003" ("Jenny Barnes" <:> "The Dilettante") Rogue [Drifter]

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
williamYorick = investigator
  "03005"
  ("William Yorick" <:> "The Gravedigger")
  Survivor
  [Warden]

lolaHayes :: CardDef
lolaHayes =
  investigator "03006" ("Lola Hayes" <:> "The Actress") Neutral [Performer]

leoAnderson :: CardDef
leoAnderson = investigator
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
fatherMateo = investigator
  "04004"
  ("Father Mateo" <:> "The Priest")
  Mystic
  [Believer, Warden]

calvinWright :: CardDef
calvinWright = investigator
  "04005"
  ("Calvin Wright" <:> "The Haunted")
  Survivor
  [Cursed, Drifter]

bodyOfAYithian :: CardDef
bodyOfAYithian = investigator
  "04244"
  ("Body of a Yithian" <:> "Captive in Another Form")
  Neutral
  [Monster, Yithian]

carolynFern :: CardDef
carolynFern = investigator
  "05001"
  ("Carolyn Fern" <:> "The Psychologist")
  Guardian
  [Miskatonic]

joeDiamond :: CardDef
joeDiamond = investigator
  "05002"
  ("Joe Diamond" <:> "The Private Investigator")
  Seeker
  [Detective]

prestonFairmont :: CardDef
prestonFairmont = investigator
  "05003"
  ("Preston Fairmont" <:> "The Millionaire")
  Rogue
  [SilverTwilight, Socialite]

dianaStanley :: CardDef
dianaStanley = investigator
  "05004"
  ("Diana Stanley" <:> "The Redeemed Cultist")
  Mystic
  [Cultist, SilverTwilight]

ritaYoung :: CardDef
ritaYoung = investigator
  "05005"
  ("Rita Young" <:> "The Athlete")
  Survivor
  [Miskatonic]

marieLambeau :: CardDef
marieLambeau = investigator
  "05006"
  ("Marie Lambeau" <:> "The Entertainer")
  Mystic
  [Performer, Sorcerer]

gavriellaMizrah :: CardDef
gavriellaMizrah = investigator
  "05046"
  ("Gavriella Mizrah" <:> "Private Security")
  Neutral
  [Veteran]

normanWithers :: CardDef
normanWithers = investigator
  "08004"
  ("Norman Withers" <:> "The Astronomer")
  Seeker
  [Miskatonic]

nathanielCho :: CardDef
nathanielCho = investigator
  "60101"
  ("Nathanial Cho" <:> "The Boxer")
  Guardian
  [Criminal, Warden]

harveyWalters :: CardDef
harveyWalters = investigator
  "60201"
  ("Harvey Walters" <:> "The Professor")
  Seeker
  [Miskatonic]

stellaClark :: CardDef
stellaClark = investigator
  "60501"
  ("Stella Clark" <:> "The Letter Carrier")
  Survivor
  [Chosen, Civic]

daisyWalkerParallel :: CardDef
daisyWalkerParallel =
  investigator "90001" ("Daisy Walker" <:> "The Librarian") Seeker [Miskatonic]
