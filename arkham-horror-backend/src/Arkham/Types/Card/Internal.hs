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

data ArkhamCardType = ArkhamCardTypeAsset | ArkhamCardTypeEvent | ArkhamCardTypeSkill | ArkhamCardTypeTreachery
data ArkhamSlot = ArkhamSlotHand | ArkhamSlotAlly

data ArkhamCardInternal = ArkhamCardInternal
  { aciType :: ArkhamCardType
  , aciCost :: Maybe Int
  , aciSlots :: [ArkhamSlot]
  , aciTestIcons :: [ArkhamSkillType]
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
  cardType
  (Just cost)
  []
  []
  (flip const)
  id
  (\_ _ -> False)
  []
  id
  id
  Nothing
  Nothing

skill :: [ArkhamSkillType] -> ArkhamCardInternal
skill testIcons = (card 0 ArkhamCardTypeSkill) { aciTestIcons = testIcons }

treachery :: ArkhamCardInternal
treachery = card 0 ArkhamCardTypeTreachery

asset :: Int -> ArkhamCardInternal
asset cost = card cost ArkhamCardTypeAsset

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

flashlight :: ArkhamCardInternal
flashlight = (hand 2)
  { aciPlay = \_state c -> c & uses ?~ 3
  , aciActionsAvailable = \_state c -> maybe False (> 0) $ c ^. uses
  , aciTestIcons = [ArkhamSkillIntellect]
  }

knife :: ArkhamCardInternal
knife = (hand 1)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [ArkhamSkillCombat]
  }

machete :: ArkhamCardInternal
machete = (hand 3)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [ArkhamSkillCombat]
  }

fortyFiveAutomatic :: ArkhamCardInternal
fortyFiveAutomatic = (hand 4)
  { aciPlay = \_state c -> c & uses ?~ 4
  , aciActionsAvailable = \_state c -> maybe False (> 0) $ c ^. uses
  , aciTestIcons = [ArkhamSkillAgility]
  }

emergencyCache :: ArkhamCardInternal
emergencyCache =
  (event 0) { aciAfterPlay = \state -> state & player . resources +~ 3 }

rolands38Special :: ArkhamCardInternal
rolands38Special = (hand 3)
  { aciPlay = \_state c -> c & uses ?~ 4
  , aciActionsAvailable = \_state c -> maybe False (> 0) $ c ^. uses
  , aciTestIcons = [ArkhamSkillCombat, ArkhamSkillAgility, ArkhamSkillWild]
  }

guardDog :: ArkhamCardInternal
guardDog = (ally 3 3 1)
  { aciAssignHealthDamage = id
  , aciTestIcons = [ArkhamSkillCombat]
  }

physicalTraining :: ArkhamCardInternal
physicalTraining = (asset 2)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [ArkhamSkillWillpower, ArkhamSkillCombat]
  }

dodge :: ArkhamCardInternal
dodge = (event 1) { aciTestIcons = [ArkhamSkillWillpower, ArkhamSkillAgility] }

dynamiteBlast :: ArkhamCardInternal
dynamiteBlast = (event 5) { aciTestIcons = [ArkhamSkillWillpower] }

evidence :: ArkhamCardInternal
evidence = (event 1) { aciTestIcons = replicate 2 ArkhamSkillIntellect }

workingAHunch :: ArkhamCardInternal
workingAHunch = (event 2) { aciTestIcons = replicate 2 ArkhamSkillIntellect }

deduction :: ArkhamCardInternal
deduction = skill [ArkhamSkillIntellect]

guts :: ArkhamCardInternal
guts = skill $ replicate 2 ArkhamSkillWillpower

overpower :: ArkhamCardInternal
overpower = skill $ replicate 2 ArkhamSkillCombat

unexpectedCourage :: ArkhamCardInternal
unexpectedCourage = skill $ replicate 2 ArkhamSkillWild

viciousBlow :: ArkhamCardInternal
viciousBlow = skill [ArkhamSkillCombat]

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

