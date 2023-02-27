module Arkham.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Agenda.Types ( Field (..) )
import Arkham.Agenda.Sequence qualified as AS
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Campaign
import Arkham.Matcher
import Arkham.Message hiding ( Discarded )
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Data.List.Extra ( firstJust )

newtype RicesWhereabouts = RicesWhereabouts ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ricesWhereabouts :: ActCard RicesWhereabouts
ricesWhereabouts = act (2, A) RicesWhereabouts Cards.ricesWhereabouts Nothing

instance HasAbilities RicesWhereabouts where
  getAbilities (RicesWhereabouts x) =
    [ mkAbility x 1 $ ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]
    , mkAbility x 2 $ ForcedAbility $ Discarded
      Timing.When
      You
      AnySource
      (cardIs Assets.jazzMulligan)
    , mkAbility x 3 $ Objective $ ForcedAbility $ TookControlOfAsset
      Timing.When
      You
      (assetIs Assets.jazzMulligan)
    ]

instance RunMessage RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      playerCount <- getPlayerCount
      let discardCount = if playerCount == 1 then 10 else 5
      a <$ push (DiscardTopOfEncounterDeck iid discardCount (toAbilitySource attrs 1) Nothing)
    UseCardAbility iid source 2 windows' _ | isSource attrs source -> do
      let
        mCard = flip firstJust windows' $ \case
          Window _ (Window.Discarded _ _ card)
            | toCardDef card == toCardDef Assets.jazzMulligan -> Just card
          _ -> Nothing
      case mCard of
        Just (EncounterCard ec) ->
          a
            <$ pushAll
                 [ RemoveFromEncounterDiscard ec
                 , InvestigatorDrewEncounterCard iid ec
                 ]
        _ -> throwIO $ InvalidState "did not find the correct card"
    UseCardAbility _ source 3 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      agendaId <- selectJust AnyAgenda
      step <- fieldMap AgendaSequence (AS.unAgendaStep . AS.agendaStep) agendaId
      alchemyLabsInPlay <- isJust
        <$> selectOne (LocationWithTitle "Alchemy Labs")
      completedTheHouseAlwaysWins <- elem "02062" <$> getCompletedScenarios
      theExperiment <- getSetAsideCard Enemies.theExperiment
      alchemicalConcoction <- getSetAsideCard Assets.alchemicalConcoction

      a <$ pushAll
        ([ PlaceLocationMatching (CardWithTitle "Alchemy Labs")
         | not alchemyLabsInPlay
         ]
        <> [ CreateEnemyAtLocationMatching
               theExperiment
               (LocationWithTitle "Alchemy Labs")
           | step <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               alchemicalConcoction
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
        <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
        )
    _ -> RicesWhereabouts <$> runMessage msg attrs
