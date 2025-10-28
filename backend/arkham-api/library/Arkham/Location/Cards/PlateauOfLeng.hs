module Arkham.Location.Cards.PlateauOfLeng (plateauOfLeng) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype PlateauOfLeng = PlateauOfLeng LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plateauOfLeng :: LocationCard PlateauOfLeng
plateauOfLeng = location PlateauOfLeng Cards.plateauOfLeng 3 (Static 1)

instance HasAbilities PlateauOfLeng where
  getAbilities (PlateauOfLeng a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringAnySkillTest)
      $ forced
      $ RevealChaosToken #at You #elderthing

instance RunMessage PlateauOfLeng where
  runMessage msg l@(PlateauOfLeng attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> PlateauOfLeng <$> liftRunMessage msg attrs
