module Arkham.Types.Card.PlayerCard where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Id
import Arkham.Types.FastWindow
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ClassSymbol
  = Guardian
  | Seeker
  | Survivor
  | Rogue
  | Mystic
  | Neutral
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PlayerCard = MkPlayerCard
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: Int
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: [Trait]
  , pcFast :: Bool
  , pcFastWindows :: HashSet FastWindow
  , pcId :: CardId
  }
  deriving stock (Show, Generic)

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasCardCode PlayerCard where
  getCardCode = pcCardCode

instance HasCardId PlayerCard where
  getCardId = pcId

instance HasCost PlayerCard where
  getCost = pcCost

lookupPlayerCard :: CardCode -> (CardId -> PlayerCard)
lookupPlayerCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ HashMap.lookup cardCode allPlayerCards

basePlayerCard
  :: CardId
  -> CardCode
  -> Text
  -> Int
  -> PlayerCardType
  -> ClassSymbol
  -> PlayerCard
basePlayerCard cardId cardCode name cost cardType classSymbol = MkPlayerCard
  { pcCardCode = cardCode
  , pcName = name
  , pcCost = cost
  , pcLevel = 0
  , pcCardType = cardType
  , pcWeakness = False
  , pcClassSymbol = classSymbol
  , pcSkills = mempty
  , pcTraits = mempty
  , pcFast = False
  , pcFastWindows = mempty
  , pcId = cardId
  }

asset :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> PlayerCard
asset cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost AssetType classSymbol

event :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> PlayerCard
event cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost EventType classSymbol

skill :: CardId -> CardCode -> Text -> [SkillType] -> ClassSymbol -> PlayerCard
skill cardId cardCode name skills classSymbol =
  (basePlayerCard cardId cardCode name 0 SkillType classSymbol)
    { pcSkills = skills
    }

treachery :: CardId -> CardCode -> Text -> Int -> PlayerCard
treachery cardId cardCode name cost =
  (basePlayerCard cardId cardCode name cost PlayerTreacheryType Neutral)
    { pcWeakness = True
    }

allPlayerCards :: HashMap CardCode (CardId -> PlayerCard)
allPlayerCards = HashMap.fromList
  [ ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01008", daisysToteBag)
  , ("01016", fortyFiveAutomatic)
  , ("01017", physicalTraining)
  , ("01020", machete)
  , ("01021", guardDog)
  , ("01022", evidence)
  , ("01023", dodge)
  , ("01024", dynamiteBlast)
  , ("01025", viciousBlow)
  , ("01037", workingAHunch)
  , ("01039", deduction)
  , ("01059", holyRosary)
  , ("01067", fearless)
  , ("01086", knife)
  , ("01087", flashlight)
  , ("01088", emergencyCache)
  , ("01089", guts)
  , ("01091", overpower)
  , ("01092", manualDexterity)
  , ("01093", unexpectedCourage)
  , ("01117", litaChantler)
  ]

rolands38Special :: CardId -> PlayerCard
rolands38Special cardId =
  (asset cardId "01006" "Roland's .38 Special" 3 Neutral)
    { pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = [Item, Weapon, Firearm]
    }

coverUp :: CardId -> PlayerCard
coverUp cardId = (treachery cardId "01007" "Cover Up" 0) { pcTraits = [Task] }

daisysToteBag :: CardId -> PlayerCard
daisysToteBag cardId = (asset cardId "01008" "Daisy's Tote Bag" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = [Item]
  }

fortyFiveAutomatic :: CardId -> PlayerCard
fortyFiveAutomatic cardId = (asset cardId "01016" ".45 Automatic" 4 Guardian)
  { pcSkills = [SkillAgility]
  , pcTraits = [Item, Weapon, Firearm]
  }

physicalTraining :: CardId -> PlayerCard
physicalTraining cardId = (asset cardId "01017" "Physical Training" 2 Guardian)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = [Talent]
  }

machete :: CardId -> PlayerCard
machete cardId = (asset cardId "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }

guardDog :: CardId -> PlayerCard
guardDog cardId = (asset cardId "01021" "Guard Dog" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Ally, Creature]
  }

evidence :: CardId -> PlayerCard
evidence cardId = (event cardId "01022" "Evidence!" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = [Insight]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [WhenEnemyDefeated You]
  }

dodge :: CardId -> PlayerCard
dodge cardId = (event cardId "01023" "Dodge" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Tactic]
  , pcFast = True
  , pcFastWindows = HashSet.fromList
    [WhenEnemyAttacks InvestigatorAtYourLocation]
  }

dynamiteBlast :: CardId -> PlayerCard
dynamiteBlast cardId = (event cardId "01024" "Dynamite Blast" 5 Guardian)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Tactic]
  }

viciousBlow :: CardId -> PlayerCard
viciousBlow cardId =
  (skill cardId "01025" "Vicious Blow" [SkillCombat] Guardian)
    { pcTraits = [Practiced]
    }

workingAHunch :: CardId -> PlayerCard
workingAHunch cardId = (event cardId "01037" "Working a Hunch" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = [Insight]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

deduction :: CardId -> PlayerCard
deduction cardId = (skill cardId "01039" "Deduction" [SkillIntellect] Seeker)
  { pcTraits = [Practiced]
  }

holyRosary :: CardId -> PlayerCard
holyRosary cardId = (asset cardId "01059" "Holy Rosary" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Item, Charm]
  }

fearless :: CardId -> PlayerCard
fearless cardId = (skill cardId "01067" "Fearless" [SkillWillpower] Mystic)
  { pcTraits = [Innate]
  }

knife :: CardId -> PlayerCard
knife cardId = (asset cardId "01086" "Knife" 1 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }

flashlight :: CardId -> PlayerCard
flashlight cardId = (asset cardId "01087" "Flashlight" 2 Neutral)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Item, Tool]
  }

emergencyCache :: CardId -> PlayerCard
emergencyCache cardId =
  (event cardId "01088" "Emergency Cache" 0 Neutral) { pcTraits = [Supply] }

guts :: CardId -> PlayerCard
guts cardId =
  (skill cardId "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { pcTraits = [Innate]
    }

overpower :: CardId -> PlayerCard
overpower cardId =
  (skill cardId "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
    { pcTraits = [Practiced]
    }

manualDexterity :: CardId -> PlayerCard
manualDexterity cardId =
  (skill cardId "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { pcTraits = [Innate]
    }

unexpectedCourage :: CardId -> PlayerCard
unexpectedCourage cardId =
  (skill cardId "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { pcTraits = [Innate]
    }

litaChantler :: CardId -> PlayerCard
litaChantler cardId =
  (asset cardId "01117" "Lita Chantler" 0 Neutral) { pcTraits = [Ally] }
