module Arkham.Event.Events.Hallow3 (hallow3, Hallow3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Taboo

newtype Hallow3 = Hallow3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallow3 :: EventCard Hallow3
hallow3 = event Hallow3 Cards.hallow3

instance RunMessage Hallow3 where
  runMessage msg e@(Hallow3 attrs) = runQueueT $ case msg of
    CardEnteredPlay _ card | attrs.cardId == card.id -> do
      attrs' <- liftRunMessage msg attrs
      pure
        $ Hallow3
        $ attrs'
        & if tabooed TabooList19 attrs' then afterPlayL .~ RemoveThisFromGame else id
    PlayThisEvent iid eid | eid == toId attrs -> do
      ts <-
        foldAllM
          [ selectTargets $ AssetWithDoom (atLeast 1)
          , selectTargets $ InvestigatorWithDoom (atLeast 1)
          , selectTargets $ EnemyWithDoom (atLeast 1)
          , selectTargets $ EventWithDoom (atLeast 1)
          , selectTargets $ LocationWithDoom (atLeast 1)
          , selectTargets $ TreacheryWithDoom (atLeast 1)
          , selectTargets $ AgendaWithDoom (atLeast 1)
          ]
      chooseOrRunOne iid [targetLabel target [RemoveDoom (toSource attrs) target 1] | target <- ts]

      pure e
    _ -> Hallow3 <$> liftRunMessage msg attrs
