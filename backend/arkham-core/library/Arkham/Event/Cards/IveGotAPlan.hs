module Arkham.Event.Cards.IveGotAPlan (iveGotAPlan, IveGotAPlan (..)) where

import Arkham.Aspect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Modifier

newtype IveGotAPlan = IveGotAPlan EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveGotAPlan :: EventCard IveGotAPlan
iveGotAPlan = event IveGotAPlan Cards.iveGotAPlan

instance RunMessage IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      sid <- getRandom
      skillTestModifier sid attrs iid
        $ ForEach
          (MaxCalculation (Fixed 3) (InvestigatorFieldCalculation iid InvestigatorClues))
          [DamageDealt 1]
      chooseFight <-
        leftOr <$> aspect iid attrs (#intellect `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pushAll chooseFight
      pure e
    _ -> IveGotAPlan <$> liftRunMessage msg attrs
