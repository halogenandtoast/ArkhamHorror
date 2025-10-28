module Arkham.Treachery.Cards.CallToOrder (callToOrder) where

import Arkham.Card
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CallToOrder = CallToOrder TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callToOrder :: TreacheryCard CallToOrder
callToOrder = treachery CallToOrder Cards.callToOrder

instance RunMessage CallToOrder where
  runMessage msg t@(CallToOrder attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discards <- scenarioField ScenarioDiscard
      let cultists = take 2 $ filter (`cardMatch` CardWithTrait Cultist) discards
      if null cultists
        then gainSurge attrs
        else do
          locations <- select $ LocationWithMostClues EmptyLocation
          chooseOrRunOneM iid do
            targets locations \location -> do
              for_ cultists (`createEnemyAt_` location)
      pure t
    _ -> CallToOrder <$> liftRunMessage msg attrs
