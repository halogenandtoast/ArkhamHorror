module Arkham.Homebrew.DarkMatter.Locations.BiologyLab (biologyLab) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BiologyLab = BiologyLab LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

biologyLab :: LocationCard BiologyLab
biologyLab = location BiologyLab Cards.biologyLab 2 (PerPlayer 1)

{- | "[action]: Heal 1 damage or 1 horror from each investigator at your location.
(Group limit once per game.)"
-}
instance HasAbilities BiologyLab where
  getAbilities (BiologyLab a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage BiologyLab where
  runMessage msg l@(BiologyLab attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      chooseOneM iid $ campaignI18n do
        labeled' "biologyLab.healDamage" $ for_ here \iid' -> healDamage iid' (attrs.ability 1) 1
        labeled' "biologyLab.healHorror" $ for_ here \iid' -> healHorror iid' (attrs.ability 1) 1
      pure l
    _ -> BiologyLab <$> liftRunMessage msg attrs
