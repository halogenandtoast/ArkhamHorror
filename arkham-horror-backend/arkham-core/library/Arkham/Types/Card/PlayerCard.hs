module Arkham.Types.Card.PlayerCard where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
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
  }
  deriving stock (Show, Generic)

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasCardCode PlayerCard where
  getCardCode = pcCardCode

instance HasCost PlayerCard where
  getCost = pcCost

lookupPlayerCard :: CardCode -> PlayerCard
lookupPlayerCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ HashMap.lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode PlayerCard
allPlayerCards = HashMap.fromList $ map
  (\c -> (getCardCode c, c))
  [ MkPlayerCard
    { pcCardCode = "01006"
    , pcName = "Roland's .38 Special"
    , pcCost = 3
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = [Item, Weapon, Firearm]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01007"
    , pcName = "Cover Up"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = PlayerTreacheryType
    , pcWeakness = True
    , pcClassSymbol = Neutral
    , pcSkills = []
    , pcTraits = [Task]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01008"
    , pcName = "Daisy's Tote Bag"
    , pcCost = 2
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
    , pcTraits = [Item]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01016"
    , pcName = ".45 Automatic"
    , pcCost = 4
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillAgility]
    , pcTraits = [Item, Weapon, Firearm]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01017"
    , pcName = "Physical Training"
    , pcCost = 2
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillIntellect, SkillCombat]
    , pcTraits = [Talent]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01020"
    , pcName = "Machete"
    , pcCost = 3
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillCombat]
    , pcTraits = [Item, Weapon, Melee]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01021"
    , pcName = "Guard Dog"
    , pcCost = 3
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillCombat]
    , pcTraits = [Ally, Creature]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01022"
    , pcName = "Evidence!"
    , pcCost = 1
    , pcLevel = 0
    , pcCardType = EventType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillIntellect, SkillIntellect]
    , pcTraits = [Insight]
    , pcFast = True
    , pcFastWindows = HashSet.fromList [WhenEnemyDefeated You]
    }
  , MkPlayerCard
    { pcCardCode = "01023"
    , pcName = "Dodge"
    , pcCost = 1
    , pcLevel = 0
    , pcCardType = EventType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillIntellect, SkillAgility]
    , pcTraits = [Tactic]
    , pcFast = True
    , pcFastWindows = HashSet.fromList
      [WhenEnemyAttacks InvestigatorAtYourLocation]
    }
  , MkPlayerCard
    { pcCardCode = "01024"
    , pcName = "Dynamite Blast"
    , pcCost = 5
    , pcLevel = 0
    , pcCardType = EventType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillIntellect]
    , pcTraits = [Tactic]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01025"
    , pcName = "Vicious Blow"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = SkillType
    , pcWeakness = False
    , pcClassSymbol = Guardian
    , pcSkills = [SkillCombat]
    , pcTraits = [Practiced]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01037"
    , pcName = "Working a Hunch"
    , pcCost = 2
    , pcLevel = 0
    , pcCardType = EventType
    , pcWeakness = False
    , pcClassSymbol = Seeker
    , pcSkills = [SkillIntellect, SkillIntellect]
    , pcTraits = [Insight]
    , pcFast = True
    , pcFastWindows = HashSet.fromList [DuringTurn You]
    }
  , MkPlayerCard
    { pcCardCode = "01039"
    , pcName = "Deduction"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = SkillType
    , pcWeakness = False
    , pcClassSymbol = Seeker
    , pcSkills = [SkillIntellect]
    , pcTraits = [Practiced]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01086"
    , pcName = "Knife"
    , pcCost = 1
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillCombat]
    , pcTraits = [Item, Weapon, Melee]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01087"
    , pcName = "Flashlight"
    , pcCost = 2
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillIntellect]
    , pcTraits = [Item, Tool]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01088"
    , pcName = "Emergency Cache"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = EventType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = []
    , pcTraits = [Supply]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01089"
    , pcName = "Guts"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = SkillType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillWillpower, SkillWillpower]
    , pcTraits = [Innate]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01091"
    , pcName = "Overpower"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = SkillType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillCombat, SkillCombat]
    , pcTraits = [Practiced]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01093"
    , pcName = "Unexpected Courage"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = SkillType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = [SkillWild, SkillWild]
    , pcTraits = [Innate]
    , pcFast = False
    , pcFastWindows = mempty
    }
  , MkPlayerCard
    { pcCardCode = "01117"
    , pcName = "Lita Chantler"
    , pcCost = 0
    , pcLevel = 0
    , pcCardType = AssetType
    , pcWeakness = False
    , pcClassSymbol = Neutral
    , pcSkills = []
    , pcTraits = [Ally]
    , pcFast = False
    , pcFastWindows = mempty
    }
  ]
