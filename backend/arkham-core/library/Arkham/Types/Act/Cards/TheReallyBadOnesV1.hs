module Arkham.Types.Act.Cards.TheReallyBadOnesV1
  ( TheReallyBadOnesV1(..)
  , theReallyBadOnesV1
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard (genPlayerCard)
import Arkham.Types.Classes
import Arkham.Types.Decks
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

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

instance ActRunner env => RunMessage env TheReallyBadOnesV1 where
  runMessage msg a@(TheReallyBadOnesV1 attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
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
          ]
        )
      pure a
    _ -> TheReallyBadOnesV1 <$> runMessage msg attrs
