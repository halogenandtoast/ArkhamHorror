module Arkham.Event.Cards.MoonlightRitual2 (moonlightRitual2, MoonlightRitual2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Elite))

newtype MoonlightRitual2 = MoonlightRitual2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual2 :: EventCard MoonlightRitual2
moonlightRitual2 = event MoonlightRitual2 Cards.moonlightRitual2

instance RunMessage MoonlightRitual2 where
  runMessage msg e@(MoonlightRitual2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      hasDoom <-
        select
          $ TargetWithDoom
          <> TargetAtLocation (locationWithInvestigator iid)
          <> not_ (TargetWithTrait Elite)
      when (notNull hasDoom) $ chooseOneToHandle iid attrs hasDoom
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) target -> do
      push $ RemoveAllDoom (toSource attrs) target
      pure e
    _ -> MoonlightRitual2 <$> liftRunMessage msg attrs
