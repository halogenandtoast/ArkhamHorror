module Arkham.Internal.PlayerCard
  ( playerCardsInternal
  , toInternalPlayerCard
  , ArkhamPlayerCardInternal(..)
  , ArkhamPlayerCardType(..)
  )
where

import Arkham.Types hiding (hand)
import Arkham.Types.Card
import Arkham.Types.GameState
import Arkham.Types.Investigator
import Arkham.Types.Location
import Arkham.Types.Skill
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.Platform ()
import Safe hiding (at)

data ArkhamPlayerCardType = PlayerAsset | PlayerEvent | PlayerSkill | PlayerTreachery
data ArkhamSlot = SlotHand | SlotAlly
data ArkhamDrawLocation = DrawLocationHand

-- Types of mythos cards: Enemy, Treachery,

data ArkhamPlayerCardInternal = ArkhamPlayerCardInternal
  { aciType :: ArkhamPlayerCardType
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
  , aciActionCost :: ArkhamGameState -> Int
  }

card :: Int -> ArkhamPlayerCardType -> ArkhamPlayerCardInternal
card cost cardType = ArkhamPlayerCardInternal
  cardType -- type
  (Just cost) -- cost
  [] -- slots
  [] -- test icons
  (\_ _ -> DrawLocationHand) -- draw to location
  id -- after draw
  (flip const) -- play
  id -- after play
  (\_ _ -> False) -- actions available
  [] -- actions
  id -- assign health damange
  id -- assign sanity damage
  Nothing -- health
  Nothing -- sanity
  (const 1)

fast :: ArkhamPlayerCardInternal -> ArkhamPlayerCardInternal
fast c = c { aciActionCost = const 0 }

skill :: [ArkhamSkillType] -> ArkhamPlayerCardInternal
skill testIcons = (card 0 PlayerSkill) { aciTestIcons = testIcons }

treachery :: ArkhamPlayerCardInternal
treachery = card 0 PlayerTreachery

asset :: Int -> ArkhamPlayerCardInternal
asset cost = card cost PlayerAsset

withUses :: Int -> ArkhamPlayerCardInternal -> ArkhamPlayerCardInternal
withUses uses' c = c
  { aciPlay = \_state -> uses ?~ uses'
  , aciActionsAvailable = const hasUsesRemaining
  }

event :: Int -> ArkhamPlayerCardInternal
event cost = card cost PlayerEvent

hand :: Int -> ArkhamPlayerCardInternal
hand cost = (asset cost) { aciSlots = [SlotHand] }

ally :: Int -> Int -> Int -> ArkhamPlayerCardInternal
ally cost health sanity = (asset cost)
  { aciSlots = [SlotAlly]
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

flashlight :: ArkhamPlayerCardInternal
flashlight = withUses 3 $ (hand 2) { aciTestIcons = [intellect] }

knife :: ArkhamPlayerCardInternal
knife = (hand 1)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [combat]
  }

machete :: ArkhamPlayerCardInternal
machete = (hand 3)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [combat]
  }

fortyFiveAutomatic :: ArkhamPlayerCardInternal
fortyFiveAutomatic = withUses 4 $ (hand 4) { aciTestIcons = [agility] }

emergencyCache :: ArkhamPlayerCardInternal
emergencyCache = (event 0) { aciAfterPlay = activePlayer . resources +~ 3 }

rolands38Special :: ArkhamPlayerCardInternal
rolands38Special =
  withUses 4 $ (hand 3) { aciTestIcons = [combat, agility, wild] }

guardDog :: ArkhamPlayerCardInternal
guardDog = (ally 3 3 1) { aciAssignHealthDamage = id, aciTestIcons = [combat] }

physicalTraining :: ArkhamPlayerCardInternal
physicalTraining = (asset 2)
  { aciActionsAvailable = \_state -> const True
  , aciTestIcons = [willpower, combat]
  }

dodge :: ArkhamPlayerCardInternal
dodge = (event 1) { aciTestIcons = [willpower, agility] }

dynamiteBlast :: ArkhamPlayerCardInternal
dynamiteBlast = (event 5) { aciTestIcons = [willpower] }

evidence :: ArkhamPlayerCardInternal
evidence = (event 1) { aciTestIcons = replicate 2 intellect }

getCurrentLocation :: ArkhamGameState -> ArkhamInvestigator -> ArkhamLocation
getCurrentLocation g i =
  fromJustNote "could not find investigator's location"
    $ find (\l -> i `elem` alInvestigators l)
    $ HashMap.elems (g ^. locations)

-- brittany-disable-next-binding
workingAHunch :: ArkhamPlayerCardInternal
workingAHunch = fast $ (event 2)
  { aciTestIcons = replicate 2 intellect
  , aciAfterPlay = \g ->
    let location = getCurrentLocation g (g ^. activePlayer . investigator)
    in
      if alClues location > 0
        then g & locations . at (alCardCode location) . _Just . clues -~ 1
               & activePlayer . clues +~ 1
        else g
  }

deduction :: ArkhamPlayerCardInternal
deduction = skill [intellect]

guts :: ArkhamPlayerCardInternal
guts = skill $ replicate 2 willpower

overpower :: ArkhamPlayerCardInternal
overpower = skill $ replicate 2 combat

unexpectedCourage :: ArkhamPlayerCardInternal
unexpectedCourage = skill $ replicate 2 wild

viciousBlow :: ArkhamPlayerCardInternal
viciousBlow = skill [combat]

coverUp :: ArkhamPlayerCardInternal
coverUp = treachery

toInternalPlayerCard :: ArkhamCard -> Maybe ArkhamPlayerCardInternal
toInternalPlayerCard c = HashMap.lookup (c ^. cardCode) playerCardsInternal

playerCardsInternal :: HashMap ArkhamCardCode ArkhamPlayerCardInternal
playerCardsInternal = HashMap.fromList
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
