module Arkham.Types.Card.EncounterCard where

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Id
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
  , ecId :: CardId
  }
  deriving stock (Show, Generic)

baseEncounterCard
  :: CardId -> CardCode -> Text -> EncounterCardType -> EncounterCard
baseEncounterCard cardId cardCode name encounterCardType = MkEncounterCard
  { ecCardCode = cardCode
  , ecId = cardId
  , ecName = name
  , ecTraits = mempty
  , ecKeywords = mempty
  , ecCardType = encounterCardType
  }

enemy :: CardId -> CardCode -> Text -> EncounterCard
enemy cardId cardCode name = baseEncounterCard cardId cardCode name EnemyType

treachery :: CardId -> CardCode -> Text -> EncounterCard
treachery cardId cardCode name =
  baseEncounterCard cardId cardCode name TreacheryType

instance ToJSON EncounterCard where
  toJSON = genericToJSON $ aesonOptions $ Just "ec"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ec"

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ec"

instance HasCardCode EncounterCard where
  getCardCode = ecCardCode

instance HasCardId EncounterCard where
  getCardId = ecId

encounterCardMatch :: (EncounterCardType, Trait) -> EncounterCard -> Bool
encounterCardMatch (cardType, trait) MkEncounterCard {..} =
  ecCardType == cardType && trait `elem` ecTraits

allEncounterCards :: HashMap CardCode (CardId -> EncounterCard)
allEncounterCards = HashMap.fromList
  [ ("01116", ghoulPriest)
  , ("01118", fleshEater)
  , ("01119", icyGhoul)
  , ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  , ("01161", ravenousGhoul)
  , ("01162", graspingHands)
  , ("01163", rottingRemains)
  , ("01164", frozenInFear)
  , ("01165", dissonantVoices)
  , ("01166", ancientEvils)
  , ("01167", cryptChill)
  , ("01168", obscuringFog)
  ]

ghoulPriest :: CardId -> EncounterCard
ghoulPriest cardId = (enemy cardId "01116" "Ghoul Priest")
  { ecTraits = [Humanoid, Monster, Ghoul, Elite]
  , ecKeywords = [Keyword.Hunter, Keyword.Retaliate]
  }

fleshEater :: CardId -> EncounterCard
fleshEater cardId =
  (enemy cardId "01118" "Flesh-Eater") { ecTraits = [Humanoid, Monster, Ghoul] }

icyGhoul :: CardId -> EncounterCard
icyGhoul cardId =
  (enemy cardId "01119" "Icy Ghoul") { ecTraits = [Humanoid, Monster, Ghoul] }

swarmOfRats :: CardId -> EncounterCard
swarmOfRats cardId = (enemy cardId "01159" "Swarm of Rats")
  { ecTraits = [Creature]
  , ecKeywords = [Keyword.Hunter]
  }

ghoulMinion :: CardId -> EncounterCard
ghoulMinion cardId = (enemy cardId "01160" "Ghoul Minion")
  { ecTraits = [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardId -> EncounterCard
ravenousGhoul cardId = (enemy cardId "01161" "Ravenous Ghoul")
  { ecTraits = [Humanoid, Monster, Ghoul]
  }

graspingHands :: CardId -> EncounterCard
graspingHands cardId =
  (treachery cardId "01162" "Grasping Hands") { ecTraits = [Hazard] }

rottingRemains :: CardId -> EncounterCard
rottingRemains cardId =
  (treachery cardId "01163" "Rotting Remains") { ecTraits = [Terror] }

frozenInFear :: CardId -> EncounterCard
frozenInFear cardId =
  (treachery cardId "01164" "Frozen in Fear") { ecTraits = [Terror] }

dissonantVoices :: CardId -> EncounterCard
dissonantVoices cardId =
  (treachery cardId "01165" "Dissonant Voices") { ecTraits = [Terror] }

ancientEvils :: CardId -> EncounterCard
ancientEvils cardId =
  (treachery cardId "01166" "Ancient Evils") { ecTraits = [Omen] }

cryptChill :: CardId -> EncounterCard
cryptChill cardId =
  (treachery cardId "01167" "Crypt Chill") { ecTraits = [Hazard] }

obscuringFog :: CardId -> EncounterCard
obscuringFog cardId =
  (treachery cardId "01168" "Obscuring Fog") { ecTraits = [Hazard] }
