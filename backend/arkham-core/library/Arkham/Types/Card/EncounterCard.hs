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
import Safe (fromJustNote)

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
  , ecVictoryPoints :: Maybe Int
  }
  deriving stock (Show, Generic)

lookupEncounterCard :: CardCode -> (CardId -> EncounterCard)
lookupEncounterCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ HashMap.lookup cardCode allEncounterCards

baseEncounterCard
  :: CardId -> CardCode -> Text -> EncounterCardType -> EncounterCard
baseEncounterCard cardId cardCode name encounterCardType = MkEncounterCard
  { ecCardCode = cardCode
  , ecId = cardId
  , ecName = name
  , ecTraits = mempty
  , ecKeywords = mempty
  , ecCardType = encounterCardType
  , ecVictoryPoints = Nothing
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
  , ("01121b", theMaskedHunter)
  , ("01135", huntingShadow)
  , ("01136", falseLead)
  , ("01137", wolfManDrew)
  , ("01138", hermanCollins)
  , ("01139", peterWarren)
  , ("01140", victoriaDevereux)
  , ("01141", ruthTurner)
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
  , ("01169", acolyte)
  , ("01170", wizardOfTheOrder)
  , ("01171", mysteriousChanting)
  , ("01172", huntingNightgaunt)
  , ("01173", onWingsOfDarkness)
  , ("01174", lockedDoor)
  ]

ghoulPriest :: CardId -> EncounterCard
ghoulPriest cardId = (enemy cardId "01116" "Ghoul Priest")
  { ecTraits = [Humanoid, Monster, Ghoul, Elite]
  , ecKeywords = [Keyword.Hunter, Keyword.Retaliate]
  , ecVictoryPoints = Just 2
  }

fleshEater :: CardId -> EncounterCard
fleshEater cardId = (enemy cardId "01118" "Flesh-Eater")
  { ecTraits = [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

icyGhoul :: CardId -> EncounterCard
icyGhoul cardId = (enemy cardId "01119" "Icy Ghoul")
  { ecTraits = [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

theMaskedHunter :: CardId -> EncounterCard
theMaskedHunter cardId = (enemy cardId "01121b" "The Masked Hunter")
  { ecTraits = [Humanoid, Cultist, Elite]
  , ecKeywords = [Keyword.Hunter]
  , ecVictoryPoints = Just 2
  }

huntingShadow :: CardId -> EncounterCard
huntingShadow cardId = (treachery cardId "01135" "Hunting Shadow")
  { ecTraits = [Curse]
  , ecKeywords = [Keyword.Peril]
  }

falseLead :: CardId -> EncounterCard
falseLead cardId = treachery cardId "01136" "False Lead"

wolfManDrew :: CardId -> EncounterCard
wolfManDrew cardId = (enemy cardId "01137" "\"Wolf-Man\" Drew")
  { ecTraits = [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

hermanCollins :: CardId -> EncounterCard
hermanCollins cardId = (enemy cardId "01138" "Herman Collins")
  { ecTraits = [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

peterWarren :: CardId -> EncounterCard
peterWarren cardId = (enemy cardId "01139" "Peter Warren")
  { ecTraits = [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

victoriaDevereux :: CardId -> EncounterCard
victoriaDevereux cardId = (enemy cardId "01140" "Victoria Devereux")
  { ecTraits = [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

ruthTurner :: CardId -> EncounterCard
ruthTurner cardId = (enemy cardId "01141" "Ruth Turner")
  { ecTraits = [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

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

acolyte :: CardId -> EncounterCard
acolyte cardId =
  (enemy cardId "01169" "Acolyte") { ecTraits = [Humanoid, Cultist] }

wizardOfTheOrder :: CardId -> EncounterCard
wizardOfTheOrder cardId = (enemy cardId "01170" "Wizard of the Order")
  { ecTraits = [Humanoid, Cultist]
  , ecKeywords = [Keyword.Retaliate]
  }

mysteriousChanting :: CardId -> EncounterCard
mysteriousChanting cardId =
  (treachery cardId "01171" "Mysterious Chanting") { ecTraits = [Hex] }

huntingNightgaunt :: CardId -> EncounterCard
huntingNightgaunt cardId = (enemy cardId "01172" "Hunting Nightgaunt")
  { ecTraits = [Monster, Nightgaunt]
  , ecKeywords = [Keyword.Hunter]
  }

onWingsOfDarkness :: CardId -> EncounterCard
onWingsOfDarkness cardId = treachery cardId "01173" "On Wings of Darkness"

lockedDoor :: CardId -> EncounterCard
lockedDoor cardId =
  (treachery cardId "01174" "Locked Door") { ecTraits = [Obstacle] }
