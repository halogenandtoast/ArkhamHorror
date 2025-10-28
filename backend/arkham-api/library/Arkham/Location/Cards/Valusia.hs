module Arkham.Location.Cards.Valusia (valusia) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DiscoverClues)

newtype Valusia = Valusia LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valusia :: LocationCard Valusia
valusia = location Valusia Cards.valusia 4 (Static 2)

instance HasAbilities Valusia where
  getAbilities (Valusia a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> ChaosTokenCountIs (IncludeSealed #cultist) (atLeast 3)) actionAbility

instance RunMessage Valusia where
  runMessage msg (Valusia attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      whenM (canHaveDamageHealed source iid) $ healDamage iid source 2
      whenM (canHaveHorrorHealed source iid) $ healHorror iid source 2
      pure $ Valusia $ attrs & shroudL ?~ Static 0
    _ -> Valusia <$> liftRunMessage msg attrs
