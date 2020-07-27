module Arkham.Types.Card.EncounterCard where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap

data EncounterCardType
  = TreacheryType
  | EnemyType
  | LocationType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncounterCard = MkEncounterCard
  { ecCardCode :: CardCode
  , ecName :: Text
  , ecCardType :: EncounterCardType
  , ecTraits   :: [Trait]
  , ecKeywords   :: [Keyword]
  }
  deriving stock (Show, Generic)

instance ToJSON EncounterCard where
  toJSON = genericToJSON $ aesonOptions $ Just "ec"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ec"

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ec"

instance HasCardCode EncounterCard where
  getCardCode = ecCardCode

encounterCardMatch :: (EncounterCardType, Trait) -> EncounterCard -> Bool
encounterCardMatch (cardType, trait) MkEncounterCard {..} =
  ecCardType == cardType && trait `elem` ecTraits

allEncounterCards :: HashMap CardCode EncounterCard
allEncounterCards = HashMap.fromList $ map
  (\c -> (getCardCode c, c))
  [ MkEncounterCard
    { ecCardCode = "01116"
    , ecName = "Ghoul Priest"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul, Elite]
    , ecKeywords = [Keyword.Hunter, Keyword.Retaliate]
    }
  , MkEncounterCard
    { ecCardCode = "01118"
    , ecName = "Flesh-Eater"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = []
    }
  , MkEncounterCard
    { ecCardCode = "01119"
    , ecName = "Icy Ghoul"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = []
    }
  , MkEncounterCard
    { ecCardCode = "01159"
    , ecName = "Swarm of Rats"
    , ecCardType = EnemyType
    , ecTraits = [Creature]
    , ecKeywords = [Keyword.Hunter]
    }
  , MkEncounterCard
    { ecCardCode = "01160"
    , ecName = "Ghoul Minion"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01161"
    , ecName = "Ravenous Ghoul"
    , ecCardType = EnemyType
    , ecTraits = [Humanoid, Monster, Ghoul]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01162"
    , ecName = "Grasping Hands"
    , ecCardType = TreacheryType
    , ecTraits = [Hazard]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01163"
    , ecName = "Rotting Remains"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01164"
    , ecName = "Frozen in Fear"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01165"
    , ecName = "Dissonant Voices"
    , ecCardType = TreacheryType
    , ecTraits = [Terror]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01166"
    , ecName = "Ancient Evils"
    , ecCardType = TreacheryType
    , ecTraits = [Omen]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01167"
    , ecName = "Crypt Chill"
    , ecCardType = TreacheryType
    , ecTraits = [Hazard]
    , ecKeywords = mempty
    }
  , MkEncounterCard
    { ecCardCode = "01168"
    , ecName = "Obscuring Fog"
    , ecCardType = TreacheryType
    , ecTraits = [Hazard]
    , ecKeywords = mempty
    }
  ]
