module Arkham.Location.Cards.OuterWall_285 (outerWall_285) where

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaSide (A))
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OuterWall_285 = OuterWall_285 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerWall_285 :: LocationCard OuterWall_285
outerWall_285 = location OuterWall_285 Cards.outerWall_285 2 (PerPlayer 2)

instance HasAbilities OuterWall_285 where
  getAbilities (OuterWall_285 a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ InvestigatorAt (be a))
      $ forced
      $ PlacedCounterOnAgenda #after (AgendaWithSide A) AnySource #doom (AtLeast $ Static 1)

instance RunMessage OuterWall_285 where
  runMessage msg l@(OuterWall_285 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> OuterWall_285 <$> liftRunMessage msg attrs
