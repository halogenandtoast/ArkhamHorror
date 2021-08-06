module Arkham.Types.Act.Attrs
  ( module Arkham.Types.Act.Attrs
  , module X
  ) where

import Arkham.Prelude

import Arkham.Act.Cards
import Arkham.Json
import Arkham.Types.Act.Sequence as X
import qualified Arkham.Types.Act.Sequence as AS
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.RequiredClues as X
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

class IsAct a

type ActCard a = CardBuilder ActId a

data ActAttrs = ActAttrs
  { actId :: ActId
  , actSequence :: ActSequence
  , actRequiredClues :: Maybe RequiredClues
  , actClues :: Maybe Int
  , actTreacheries :: Set TreacheryId
  }
  deriving stock (Show, Eq, Generic)

sequenceL :: Lens' ActAttrs ActSequence
sequenceL = lens actSequence $ \m x -> m { actSequence = x }

cluesL :: Lens' ActAttrs (Maybe Int)
cluesL = lens actClues $ \m x -> m { actClues = x }

treacheriesL :: Lens' ActAttrs (Set TreacheryId)
treacheriesL = lens actTreacheries $ \m x -> m { actTreacheries = x }

actWith
  :: (Int, ActSide)
  -> (ActAttrs -> a)
  -> CardDef
  -> Maybe RequiredClues
  -> (ActAttrs -> ActAttrs)
  -> CardBuilder ActId a
actWith (n, side) f cardDef mRequiredClues g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \aid -> f . g $ ActAttrs
    { actId = aid
    , actSequence = AS.Act n side
    , actClues = Nothing
    , actRequiredClues = mRequiredClues
    , actTreacheries = mempty
    }
  }

act
  :: (Int, ActSide)
  -> (ActAttrs -> a)
  -> CardDef
  -> Maybe RequiredClues
  -> CardBuilder ActId a
act actSeq f cardDef mRequiredClues =
  actWith actSeq f cardDef mRequiredClues id

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

instance ActionRunner env => HasActions env ActAttrs where
  getActions _ FastPlayerWindow attrs@ActAttrs {..} = case actRequiredClues of
    Just (RequiredClues requiredClues Nothing) -> do
      totalSpendableClues <- unSpendableClueCount <$> getCount ()
      totalRequiredClues <- getPlayerCountValue requiredClues
      pure
        [ AdvanceAct actId (toSource attrs)
        | totalSpendableClues >= totalRequiredClues
        ]
    Just (RequiredClues requiredClues (Just locationMatcher)) -> do
      mLocationId <- getId @(Maybe LocationId) locationMatcher
      case mLocationId of
        Just lid -> do
          iids <- getSetList @InvestigatorId lid
          totalSpendableClues <- sum
            <$> for iids ((unSpendableClueCount <$>) . getCount)
          totalRequiredClues <- getPlayerCountValue requiredClues
          pure
            [ AdvanceAct actId (toSource attrs)
            | totalSpendableClues >= totalRequiredClues
            ]
        Nothing -> pure []
    Nothing -> pure []
  getActions _ _ _ = pure []

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
  :: ( MonadReader env m
     , HasId LeadInvestigatorId env ()
     , HasCount PlayerCount env ()
     )
  => [InvestigatorId]
  -> GameValue Int
  -> ActAttrs
  -> m [Message]
advanceActSideA investigatorIds requiredClues attrs = do
  leadInvestigatorId <- getLeadInvestigatorId
  totalRequiredClues <- getPlayerCountValue requiredClues
  pure
    ([ SpendClues totalRequiredClues investigatorIds | totalRequiredClues > 0 ]
    <> [ CheckWindow leadInvestigatorId [WhenActAdvance (toId attrs)]
       , chooseOne leadInvestigatorId [AdvanceAct (toId attrs) (toSource attrs)]
       ]
    )

instance ActAttrsRunner env => RunMessage env ActAttrs where
  runMessage msg a@ActAttrs {..} = case msg of
    AdvanceAct aid _ | aid == actId && onSide A a -> do
      case actRequiredClues of
        Just (RequiredClues requiredClues Nothing) -> do
          investigatorIds <- getInvestigatorIds
          pushAll =<< advanceActSideA investigatorIds requiredClues a
        Just (RequiredClues requiredClues (Just locationMatcher)) -> do
          mLocationId <- getId @(Maybe LocationId) locationMatcher
          case mLocationId of
            Just lid -> do
              investigatorIds <- getSetList @InvestigatorId lid
              pushAll =<< advanceActSideA investigatorIds requiredClues a
            Nothing ->
              throwIO $ InvalidState
                "Should not have advanced if locaiton does not exists"
        Nothing -> do
          investigatorIds <- getInvestigatorIds
          pushAll =<< advanceActSideA investigatorIds (Static 0) a
      pure $ a & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AttachTreachery tid (ActTarget aid) | aid == actId ->
      pure $ a & treacheriesL %~ insertSet tid
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      a <$ when (null investigatorIds) (push AllInvestigatorsResigned)
    _ -> pure a
