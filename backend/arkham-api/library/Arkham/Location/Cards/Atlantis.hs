module Arkham.Location.Cards.Atlantis (atlantis) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Atlantis = Atlantis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atlantis :: LocationCard Atlantis
atlantis = location Atlantis Cards.atlantis 3 (Static 2)

instance HasAbilities Atlantis where
  getAbilities (Atlantis a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealChaosToken #after Anyone #autofail

instance RunMessage Atlantis where
  runMessage msg l@(Atlantis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      n <- field LocationDoom attrs.id
      when (n >= 3) $ removeLocation attrs.id
      pure l
    _ -> Atlantis <$> liftRunMessage msg attrs
