module Arkham.Location.Cards.DeconstructionRoom (deconstructionRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype DeconstructionRoom = DeconstructionRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deconstructionRoom :: LocationCard DeconstructionRoom
deconstructionRoom = location DeconstructionRoom Cards.deconstructionRoom 3 (PerPlayer 1)

instance HasAbilities DeconstructionRoom where
  getAbilities (DeconstructionRoom a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage DeconstructionRoom where
  runMessage msg l@(DeconstructionRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat
        $ SumCalculation [Fixed 4, LocationFieldCalculation attrs.id LocationClues]
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember DissectedAnOrgan
      pure l
    _ -> DeconstructionRoom <$> liftRunMessage msg attrs
