module Arkham.Act.Cards.TheReallyBadOnesV1
  ( TheReallyBadOnesV1(..)
  , theReallyBadOnesV1
  ) where

import Arkham.Prelude

import Arkham.Act.Attrs
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.PlayerCard (genPlayerCard)
import Arkham.Classes
import Arkham.Decks
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target
import Arkham.Trait

newtype TheReallyBadOnesV1 = TheReallyBadOnesV1 ActAttrs
  deriving anyclass (IsAct, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theReallyBadOnesV1 :: ActCard TheReallyBadOnesV1
theReallyBadOnesV1 =
  act (2, A) TheReallyBadOnesV1 Cards.theReallyBadOnesV1 Nothing

instance Query LocationMatcher env => HasModifiersFor env TheReallyBadOnesV1 where
  getModifiersFor _ (LocationTarget lid) (TheReallyBadOnesV1 attrs) = do
    targets <- select UnrevealedLocation
    pure
      [ toModifier attrs (TraitRestrictedModifier ArkhamAsylum Blank)
      | lid `member` targets
      ]
  getModifiersFor _ _ _ = pure []

instance ActRunner env => RunMessage TheReallyBadOnesV1 where
  runMessage msg a@(TheReallyBadOnesV1 attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigators <- selectList $ InvestigatorAt $ locationIs
        Locations.patientConfinementDanielsCell
      danielChesterfield <- PlayerCard
        <$> genPlayerCard Assets.danielChesterfield
      enemiesUnderAct <-
        filter ((== EnemyType) . toCardType)
        . mapMaybe (preview _EncounterCard . unUnderneathCard)
        <$> getList ActDeck
      pushAll
        (chooseOne
            leadInvestigatorId
            [ targetLabel
                iid
                [TakeControlOfSetAsideAsset iid danielChesterfield]
            | iid <- investigators
            ]
        : [ ShuffleIntoEncounterDeck enemiesUnderAct
          , ShuffleEncounterDiscardBackIn
          , AdvanceActDeck (actDeckId attrs) (toSource attrs)
          ]
        )
      pure a
    _ -> TheReallyBadOnesV1 <$> runMessage msg attrs
