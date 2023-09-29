module Arkham.Act.Cards.TheReallyBadOnesV1 (
  TheReallyBadOnesV1 (..),
  theReallyBadOnesV1,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait

newtype TheReallyBadOnesV1 = TheReallyBadOnesV1 ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReallyBadOnesV1 :: ActCard TheReallyBadOnesV1
theReallyBadOnesV1 =
  act (2, A) TheReallyBadOnesV1 Cards.theReallyBadOnesV1 Nothing

instance HasModifiersFor TheReallyBadOnesV1 where
  getModifiersFor (LocationTarget lid) (TheReallyBadOnesV1 attrs) = do
    targets <- select UnrevealedLocation
    pure
      [ toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)
      | lid `member` targets
      ]
  getModifiersFor _ _ = pure []

instance RunMessage TheReallyBadOnesV1 where
  runMessage msg a@(TheReallyBadOnesV1 attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigators <-
        selectList
          $ InvestigatorAt
          $ locationIs
            Locations.patientConfinementDanielsCell
      danielChesterfield <-
        PlayerCard
          <$> genPlayerCard Assets.danielChesterfield
      enemiesUnderAct <-
        filter ((== EnemyType) . toCardType)
          <$> scenarioField ScenarioCardsUnderActDeck
      pushAll
        ( chooseOne
            leadInvestigatorId
            [ targetLabel
              iid
              [TakeControlOfSetAsideAsset iid danielChesterfield]
            | iid <- investigators
            ]
            : [ ShuffleCardsIntoDeck Deck.EncounterDeck enemiesUnderAct
              , ShuffleEncounterDiscardBackIn
              , AdvanceActDeck (actDeckId attrs) (toSource attrs)
              ]
        )
      pure a
    _ -> TheReallyBadOnesV1 <$> runMessage msg attrs
