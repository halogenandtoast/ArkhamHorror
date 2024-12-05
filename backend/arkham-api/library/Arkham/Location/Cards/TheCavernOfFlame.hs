module Arkham.Location.Cards.TheCavernOfFlame (theCavernOfFlame, TheCavernOfFlame (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheCavernOfFlame = TheCavernOfFlame LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCavernOfFlame :: LocationCard TheCavernOfFlame
theCavernOfFlame = location TheCavernOfFlame Cards.theCavernOfFlame 9 (Static 0)

instance HasModifiersFor TheCavernOfFlame where
  getModifiersFor (TheCavernOfFlame a) = whenUnrevealed a do
    modifySelfWhenM a (selectAny $ locationIs Cards.seventySteps <> LocationWithAnyClues) [Blocked]

instance HasAbilities TheCavernOfFlame where
  getAbilities (TheCavernOfFlame a) =
    extendRevealed1 a $ restricted a 1 (exists $ investigatorAt a) $ forced $ PhaseEnds #when #mythos

instance RunMessage TheCavernOfFlame where
  runMessage msg l@(TheCavernOfFlame attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ investigatorAt (toId attrs)
      lead <- getLead
      chooseOrRunOneAtATimeM lead $ targets investigators \iid -> assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> TheCavernOfFlame <$> liftRunMessage msg attrs
