module Arkham.Types.Act.Attrs
  ( module Arkham.Types.Act.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Act.Cards
import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Act.Sequence as X
import qualified Arkham.Types.Act.Sequence as AS
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost as X
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window hiding (when)
import qualified Arkham.Types.Window as Window

class IsAct a

type ActCard a = CardBuilder ActId a

data ActAttrs = ActAttrs
  { actId :: ActId
  , actSequence :: ActSequence
  , actAdvanceCost :: Maybe Cost
  , actClues :: Maybe Int
  , actTreacheries :: HashSet TreacheryId
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
  -> CardBuilder ActId a
actWith (n, side) f cardDef mCost g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \aid -> f . g $ ActAttrs
    { actId = aid
    , actSequence = AS.Act n side
    , actClues = Nothing
    , actAdvanceCost = mCost
    , actTreacheries = mempty
    }
  }

act
  :: (Int, ActSide)
  -> (ActAttrs -> a)
  -> CardDef
  -> Maybe Cost
  -> CardBuilder ActId a
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

instance HasModifiersFor env ActAttrs

instance HasStep ActAttrs ActStep where
  getStep = asks $ actStep . actSequence

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

instance HasActions ActAttrs where
  getActions attrs@ActAttrs {..} = case actAdvanceCost of
    Just cost -> [mkAbility attrs 100 (Objective $ FastAbility cost)]
    Nothing -> []

type ActAttrsRunner env
  = ( HasQueue env
    , HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasCount PlayerCount env ()
    , HasId LeadInvestigatorId env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasSet InvestigatorId env LocationId
    )

advanceActSideA
  :: (MonadReader env m, HasId LeadInvestigatorId env ())
  => ActAttrs
  -> m [Message]
advanceActSideA attrs = do
  leadInvestigatorId <- getLeadInvestigatorId
  pure
    [ CheckWindow leadInvestigatorId [Window.when (ActAdvance $ toId attrs)]
    , chooseOne leadInvestigatorId [AdvanceAct (toId attrs) (toSource attrs)]
    ]

instance ActAttrsRunner env => RunMessage env ActAttrs where
  runMessage msg a@ActAttrs {..} = case msg of
    AdvanceAct aid _ | aid == actId && onSide A a -> do
      pushAll =<< advanceActSideA a
      pure $ a & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AttachTreachery tid (ActTarget aid) | aid == actId ->
      pure $ a & treacheriesL %~ insertSet tid
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when (null investigatorIds) (push AllInvestigatorsResigned)
    UseCardAbility iid source _ 100 _ | isSource a source ->
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid))
    _ -> pure a
