module Arkham.Event.Cards.Hallow3 (hallow3, Hallow3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Taboo

newtype Hallow3 = Hallow3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallow3 :: EventCard Hallow3
hallow3 =
  eventWith
    Hallow3
    Cards.hallow3
    (\a -> if tabooed TabooList19 a then a {eventAfterPlay = RemoveThisFromGame} else a)

instance RunMessage Hallow3 where
  runMessage msg e@(Hallow3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      targets <-
        foldAllM
          [ selectTargets $ AssetWithDoom (atLeast 1)
          , selectTargets $ InvestigatorWithDoom (atLeast 1)
          , selectTargets $ EnemyWithDoom (atLeast 1)
          , selectTargets $ EventWithDoom (atLeast 1)
          , selectTargets $ LocationWithDoom (atLeast 1)
          , selectTargets $ TreacheryWithDoom (atLeast 1)
          , selectTargets $ AgendaWithDoom (atLeast 1)
          ]
      chooseOrRunOne iid [targetLabel target [RemoveDoom (toSource attrs) target 1] | target <- targets]

      pure e
    _ -> Hallow3 <$> liftRunMessage msg attrs
