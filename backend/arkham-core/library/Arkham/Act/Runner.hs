{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Act.Runner (module Arkham.Act.Runner, module X) where

import Arkham.Prelude

import Arkham.Act.Attrs as X
import Arkham.Act.Sequence as X
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Cost as X
import Arkham.Decks
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Message
import Arkham.Name
import Arkham.Query
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait (Trait)
import Arkham.Window

type ActRunner env
  = ( HasQueue env
    , HasCampaignStoryCard env ()
    , Query AgendaMatcher env
    , Query AssetMatcher env
    , Query EnemyMatcher env
    , Query InvestigatorMatcher env
    , Query LocationMatcher env
    , Query ExtendedCardMatcher env
    , HasCount ClueCount env AssetId
    , HasCount ClueCount env InvestigatorId
    , HasCount DamageCount env EnemyId
    , HasCount PlayerCount env ()
    , HasCount SpendableClueCount env InvestigatorId
    , HasCount SpendableClueCount env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasId CardCode env AssetId
    , HasId CardCode env EnemyId
    , HasId LeadInvestigatorId env ()
    , HasList Token env ()
    , HasList ResignedCardCode env ()
    , HasList UnderneathCard env ActDeck
    , HasList DiscardedEncounterCard env ()
    , HasRecord env ()
    , HasSet CompletedScenarioId env ()
    , HasSet EnemyId env LocationId
    , HasSet InvestigatorId env ()
    , HasSet InvestigatorId env (HashSet LocationId)
    , HasSet InvestigatorId env LocationId
    , HasSet InvestigatorId env LocationMatcher
    , HasSet InScenarioInvestigatorId env ()
    , HasSet LocationId env ()
    , HasSet LocationId env [Trait]
    , HasSet ScenarioLogKey env ()
    , HasSet Trait env AssetId
    , HasSet UnrevealedLocationId env ()
    , HasSet VictoryDisplayCardCode env ()
    , HasStep AgendaStep env ()
    , HasList LocationName env ()
    )

type ActAttrsRunner env
  = ( HasSet InScenarioInvestigatorId env ()
    , HasSet InvestigatorId env ()
    , HasCount PlayerCount env ()
    , HasId LeadInvestigatorId env ()
    , HasId (Maybe LocationId) env LocationMatcher
    , HasSet InvestigatorId env LocationId
    )

advanceActSideA
  :: (MonadReader env m, HasId LeadInvestigatorId env ())
  => ActAttrs
  -> AdvancementMethod
  -> m [Message]
advanceActSideA attrs advanceMode = do
  leadInvestigatorId <- getLeadInvestigatorId
  pure
    [ CheckWindow
      [leadInvestigatorId]
      [Window Timing.When (ActAdvance $ toId attrs)]
    , chooseOne leadInvestigatorId [AdvanceAct (toId attrs) (toSource attrs) advanceMode]
    ]

instance ActAttrsRunner env => RunMessage ActAttrs where
  runMessage msg a@ActAttrs {..} = case msg of
    AdvanceAct aid _ advanceMode | aid == actId && onSide A a -> do
      pushAll =<< advanceActSideA a advanceMode
      pure $ a & (sequenceL .~ Act (unActStep $ actStep actSequence) B)
    AttachTreachery tid (ActTarget aid) | aid == actId ->
      pure $ a & treacheriesL %~ insertSet tid
    Discard (TreacheryTarget tid) -> pure $ a & treacheriesL %~ deleteSet tid
    InvestigatorResigned _ -> do
      investigatorIds <- getSet @InScenarioInvestigatorId ()
      whenMsg <- checkWindows
        [Window Timing.When AllUndefeatedInvestigatorsResigned]
      afterMsg <- checkWindows
        [Window Timing.When AllUndefeatedInvestigatorsResigned]
      a <$ when
        (null investigatorIds)
        (pushAll [whenMsg, afterMsg, AllInvestigatorsResigned])
    UseCardAbility iid source _ 999 _ | isSource a source ->
      -- This is assumed to be advancement via spending clues
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues)
    PlaceClues (ActTarget aid) n | aid == actId -> do
      let totalClues = n + fromMaybe 0 actClues
      pure $ a { actClues = Just totalClues }
    _ -> pure a
