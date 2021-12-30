module Arkham.Act.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards
import Arkham.Act.Sequence
import Arkham.Act.Sequence qualified as AS
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Source
import Arkham.Target

class IsAct a

type ActCard a = CardBuilder (Int, ActId) a

data ActAttrs = ActAttrs
  { actId :: ActId
  , actSequence :: ActSequence
  , actAdvanceCost :: Maybe Cost
  , actClues :: Maybe Int
  , actTreacheries :: HashSet TreacheryId
  , actDeckId :: Int
  }
  deriving stock (Show, Eq, Generic)

sequenceL :: Lens' ActAttrs ActSequence
sequenceL = lens actSequence $ \m x -> m { actSequence = x }

cluesL :: Lens' ActAttrs (Maybe Int)
cluesL = lens actClues $ \m x -> m { actClues = x }

treacheriesL :: Lens' ActAttrs (HashSet TreacheryId)
treacheriesL = lens actTreacheries $ \m x -> m { actTreacheries = x }

actWith
  :: (Int, ActSide)
  -> (ActAttrs -> a)
  -> CardDef
  -> Maybe Cost
  -> (ActAttrs -> ActAttrs)
  -> CardBuilder (Int, ActId) a
actWith (n, side) f cardDef mCost g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(deckId, aid) -> f . g $ ActAttrs
    { actId = aid
    , actSequence = AS.Act n side
    , actClues = Nothing
    , actAdvanceCost = mCost
    , actTreacheries = mempty
    , actDeckId = deckId
    }
  }

act
  :: (Int, ActSide)
  -> (ActAttrs -> a)
  -> CardDef
  -> Maybe Cost
  -> CardBuilder (Int, ActId) a
act actSeq f cardDef mCost = actWith actSeq f cardDef mCost id

instance HasCardDef ActAttrs where
  toCardDef e = case lookup (unActId $ actId e) allActCards of
    Just def -> def
    Nothing -> error $ "missing card def for act " <> show (unActId $ actId e)

instance ToJSON ActAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "act"
  toEncoding = genericToEncoding $ aesonOptions $ Just "act"

instance FromJSON ActAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "act"

instance HasStep ActStep env ActAttrs where
  getStep = pure . actStep . actSequence

instance Entity ActAttrs where
  type EntityId ActAttrs = ActId
  type EntityAttrs ActAttrs = ActAttrs
  toId = actId
  toAttrs = id

instance Named ActAttrs where
  toName = toName . toCardDef

instance TargetEntity ActAttrs where
  toTarget = ActTarget . toId
  isTarget ActAttrs { actId } (ActTarget aid) = actId == aid
  isTarget _ _ = False

instance SourceEntity ActAttrs where
  toSource = ActSource . toId
  isSource ActAttrs { actId } (ActSource aid) = actId == aid
  isSource _ _ = False

onSide :: ActSide -> ActAttrs -> Bool
onSide side ActAttrs {..} = actSide actSequence == side

instance HasAbilities ActAttrs where
  getAbilities attrs@ActAttrs {..} = case actAdvanceCost of
    Just cost -> [mkAbility attrs 100 (Objective $ FastAbility cost)]
    Nothing -> []
