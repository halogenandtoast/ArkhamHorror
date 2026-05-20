module Arkham.Act.Cards.AgainstTheHouse (againstTheHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype AgainstTheHouse = AgainstTheHouse ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstTheHouse :: ActCard AgainstTheHouse
againstTheHouse = act (2, A) AgainstTheHouse Cards.againstTheHouse Nothing

instance HasModifiersFor AgainstTheHouse where
  getModifiersFor (AgainstTheHouse a) =
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities AgainstTheHouse where
  getAbilities = actAbilities \a ->
    [restricted a 1 (Negate $ exists Anywhere) $ Objective $ forced AnyWindow]

instance RunMessage AgainstTheHouse where
  runMessage msg a@(AgainstTheHouse attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> AgainstTheHouse <$> liftRunMessage msg attrs
