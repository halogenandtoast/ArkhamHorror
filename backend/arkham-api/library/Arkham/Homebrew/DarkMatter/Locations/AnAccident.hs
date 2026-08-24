module Arkham.Homebrew.DarkMatter.Locations.AnAccident (anAccident) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Matcher qualified as Matcher

newtype AnAccident = AnAccident LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anAccident :: LocationCard AnAccident
anAccident = symbolLabel $ location AnAccident Cards.anAccident 3 (PerPlayer 1)

{- | "Forced - When you reveal a [cultist] token during a skill test at this
location: Take 1 damage."
-}
instance HasAbilities AnAccident where
  getAbilities (AnAccident a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ Matcher.RevealChaosToken #when You (ChaosTokenFaceIs Cultist)

instance RunMessage AnAccident where
  runMessage msg l@(AnAccident attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> AnAccident <$> liftRunMessage msg attrs
