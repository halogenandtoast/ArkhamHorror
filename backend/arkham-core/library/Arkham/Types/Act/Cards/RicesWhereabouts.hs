module Arkham.Types.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Exception
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (Discarded)
import Arkham.Types.ScenarioId
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window
import Data.List.Extra (firstJust)

newtype RicesWhereabouts = RicesWhereabouts ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ricesWhereabouts :: ActCard RicesWhereabouts
ricesWhereabouts = act (2, A) RicesWhereabouts Cards.ricesWhereabouts Nothing

instance HasAbilities RicesWhereabouts where
  getAbilities (RicesWhereabouts x) =
    [ mkAbility x 1 $ ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]
    , mkAbility x 2 $ ForcedAbility $ Discarded
      Timing.When
      You
      (cardIs Assets.jazzMulligan)
    , mkAbility x 3 $ Objective $ ForcedAbility $ TookControlOfAsset
      Timing.When
      You
      (assetIs Assets.jazzMulligan)
    ]

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      playerCount <- getPlayerCount
      let discardCount = if playerCount == 1 then 10 else 5
      a <$ push (DiscardTopOfEncounterDeck iid discardCount Nothing)
    UseCardAbility iid source windows' 2 _ | isSource attrs source -> do
      let
        mCard = flip firstJust windows' $ \case
          Window _ (Window.Discarded _ card)
            | toCardDef card == Assets.jazzMulligan -> Just card
          _ -> Nothing
      case mCard of
        Just (EncounterCard ec) ->
          a
            <$ pushAll
                 [ RemoveFromEncounterDiscard ec
                 , InvestigatorDrewEncounterCard iid ec
                 ]
        _ -> throwIO $ InvalidState "did not find the correct card"
    UseCardAbility _ source _ 3 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      alchemyLabsInPlay <- isJust
        <$> selectOne (LocationWithTitle "Alchemy Labs")
      agendaStep <- unAgendaStep <$> getStep ()
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()
      theExperiment <- getSetAsideCard Enemies.theExperiment
      alchemicalConcoction <- getSetAsideCard Assets.alchemicalConcoction

      a <$ pushAll
        ([ PlaceLocationMatching (CardWithTitle "Alchemy Labs")
         | not alchemyLabsInPlay
         ]
        <> [ CreateEnemyAtLocationMatching
               theExperiment
               (LocationWithTitle "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               alchemicalConcoction
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
        <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
        )
    _ -> RicesWhereabouts <$> runMessage msg attrs
