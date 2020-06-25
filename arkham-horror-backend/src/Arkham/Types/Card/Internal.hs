module Arkham.Types.Card.Internal
  ( cardsInternal
  , toInternalCard
  , ArkhamCardInternal(..)
  , ArkhamCardType(..)
  )
where

import Arkham.Types hiding (hand)
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Extras

data ArkhamCardType = ArkhamCardTypeAsset | ArkhamCardTypeEvent | ArkhamCardTypeSkill | ArkhamCardTypeTreachery
data ArkhamSlot = ArkhamSlotHand | ArkhamSlotAlly
data ArkhamDrawLocation = ArkhamDrawLocationHand

data ArkhamCardInternal = ArkhamCardInternal
  { aciType :: ArkhamCardType
  , aciCost :: Maybe Int
  , aciSlots :: [ArkhamSlot]
  , aciTestIcons :: [ArkhamSkillType]
  , aciDrawToLocation :: ArkhamGameState -> ArkhamCard -> ArkhamDrawLocation
  , aciAfterDraw :: ArkhamGameState -> ArkhamGameState
  , aciPlay :: ArkhamGameState -> ArkhamCard -> ArkhamCard
  , aciAfterPlay :: ArkhamGameState -> ArkhamGameState
  , aciActionsAvailable :: ArkhamGameState -> ArkhamCard -> Bool
  , aciActions :: [ArkhamGameState -> ArkhamGameState]
  , aciAssignHealthDamage :: ArkhamGameState -> ArkhamGameState
  , aciAssignSanityDamage :: ArkhamGameState -> ArkhamGameState
  , aciHealth :: Maybe Int
  , aciSanity :: Maybe Int
  }

card :: Int -> ArkhamCardType -> ArkhamCardInternal
card cost cardType = ArkhamCardInternal
  cardType -- type
  (Just cost) -- cost
  [] -- slots
  [] -- test icons
  (\_ _ -> ArkhamDrawLocationHand) -- draw to location
  id -- after draw
  (flip const) -- play
  id -- after play
  (\_ _ -> False) -- actions available
  [] -- actions
  id -- assign health damange
  id -- assign sanity damage
  Nothing -- health
  Nothing -- sanity

skill :: [ArkhamSkillType] -> ArkhamCardInternal
skill testIcons = (card 0 ArkhamCardTypeSkill) { aciTestIcons = testIcons }

treachery :: ArkhamCardInternal
treachery = card 0 ArkhamCardTypeTreachery

asset :: Int -> ArkhamCardInternal
asset cost = card cost ArkhamCardTypeAsset

withUses :: Int -> ArkhamCardInternal -> ArkhamCardInternal
withUses uses' c = c
  { aciPlay = \_state -> uses ?~ uses'
  , aciActionsAvailable = const hasUsesRemaining
  }

event :: Int -> ArkhamCardInternal
event cost = card cost ArkhamCardTypeEvent

hand :: Int -> ArkhamCardInternal
hand cost = (asset cost) { aciSlots = [ArkhamSlotHand] }

ally :: Int -> Int -> Int -> ArkhamCardInternal
ally cost health sanity = (asset cost)
  { aciSlots = [ArkhamSlotAlly]
  , aciHealth = Just health
  , aciSanity = Just sanity
  }

hasUsesRemaining :: HasUses a => a -> Bool
hasUsesRemaining = view (uses . non 0 . to (> 0))

willpower :: ArkhamSkillType
willpower = ArkhamSkillWillpower

intellect :: ArkhamSkillType
intellect = ArkhamSkillIntellect

combat :: ArkhamSkillType
combat = ArkhamSkillCombat

agility :: ArkhamSkillType
agility = ArkhamSkillAgility

wild :: ArkhamSkillType
wild = ArkhamSkillWild

flashlight :: ArkhamCardInternal
flashlight = withUses 3 $ (hand 2) { aciTestIcons = [intellect] }

knife :: ArkhamCardInternal
knife = (hand 1)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [combat]
  }

machete :: ArkhamCardInternal
machete = (hand 3)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [combat]
  }

fortyFiveAutomatic :: ArkhamCardInternal
fortyFiveAutomatic = withUses 4 $ (hand 4) { aciTestIcons = [agility] }

emergencyCache :: ArkhamCardInternal
emergencyCache = (event 0) { aciAfterPlay = player . resources +~ 3 }

rolands38Special :: ArkhamCardInternal
rolands38Special =
  withUses 4 $ (hand 3) { aciTestIcons = [combat, agility, wild] }

guardDog :: ArkhamCardInternal
guardDog = (ally 3 3 1) { aciAssignHealthDamage = id, aciTestIcons = [combat] }

physicalTraining :: ArkhamCardInternal
physicalTraining = (asset 2)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [willpower, combat]
  }

dodge :: ArkhamCardInternal
dodge = (event 1) { aciTestIcons = [willpower, agility] }

dynamiteBlast :: ArkhamCardInternal
dynamiteBlast = (event 5) { aciTestIcons = [willpower] }

evidence :: ArkhamCardInternal
evidence = (event 1) { aciTestIcons = replicate 2 intellect }

workingAHunch :: ArkhamCardInternal
workingAHunch = (event 2) { aciTestIcons = replicate 2 intellect }

deduction :: ArkhamCardInternal
deduction = skill [intellect]

guts :: ArkhamCardInternal
guts = skill $ replicate 2 willpower

overpower :: ArkhamCardInternal
overpower = skill $ replicate 2 combat

unexpectedCourage :: ArkhamCardInternal
unexpectedCourage = skill $ replicate 2 wild

viciousBlow :: ArkhamCardInternal
viciousBlow = skill [combat]

coverUp :: ArkhamCardInternal
coverUp = treachery

toInternalCard :: ArkhamCard -> Maybe ArkhamCardInternal
toInternalCard c = HashMap.lookup (c ^. cardCode) cardsInternal

cardsInternal :: HashMap ArkhamCardCode ArkhamCardInternal
cardsInternal = HashMap.fromList
  [ (ArkhamCardCode "01006", rolands38Special)
  , (ArkhamCardCode "01007", coverUp)
  , (ArkhamCardCode "01016", fortyFiveAutomatic)
  , (ArkhamCardCode "01017", physicalTraining)
  , (ArkhamCardCode "01020", machete)
  , (ArkhamCardCode "01021", guardDog)
  , (ArkhamCardCode "01022", evidence)
  , (ArkhamCardCode "01023", dodge)
  , (ArkhamCardCode "01025", viciousBlow)
  , (ArkhamCardCode "01023", dynamiteBlast)
  , (ArkhamCardCode "01037", workingAHunch)
  , (ArkhamCardCode "01039", deduction)
  , (ArkhamCardCode "01086", knife)
  , (ArkhamCardCode "01087", flashlight)
  , (ArkhamCardCode "01088", emergencyCache)
  , (ArkhamCardCode "01089", guts)
  , (ArkhamCardCode "01091", overpower)
  , (ArkhamCardCode "01093", unexpectedCourage)
  ]
