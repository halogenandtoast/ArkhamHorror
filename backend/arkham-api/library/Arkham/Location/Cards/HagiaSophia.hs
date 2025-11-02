module Arkham.Location.Cards.HagiaSophia (hagiaSophia) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Token

newtype HagiaSophia = HagiaSophia LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hagiaSophia :: LocationCard HagiaSophia
hagiaSophia = symbolLabel $ locationWith HagiaSophia Cards.hagiaSophia 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor HagiaSophia where
  getModifiersFor (HagiaSophia a) = do
    let study = a.token Study
    when (study > 0) $ modifySelf a [ShroudModifier (study * 3)]

instance HasAbilities HagiaSophia where
  getAbilities (HagiaSophia a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage HagiaSophia where
  runMessage msg l@(HagiaSophia attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      placeTokens (attrs.ability 1) attrs #clue n
      placeTokens (attrs.ability 1) attrs Study 1
      pure l
    _ -> HagiaSophia <$> liftRunMessage msg attrs
