module Arkham.Homebrew.DarkMatter.Locations.AMutiny (aMutiny) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AMutiny = AMutiny LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aMutiny :: LocationCard AMutiny
aMutiny = location AMutiny Cards.aMutiny 3 (PerPlayer 1)

{- | "[action] Deal 1 damage to Brain Cylinder 367: Each investigator at this
location adds 1 tally mark next to their 'Memories'. (Group limit once per
game.)"
-}
instance HasAbilities AMutiny where
  getAbilities (AMutiny a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> exists (assetIs Assets.brainCylinder367))
        actionAbility

instance RunMessage AMutiny where
  runMessage msg l@(AMutiny attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cylinders <- select $ assetIs Assets.brainCylinder367
      for_ cylinders \aid -> dealAssetDamage aid (attrs.ability 1) 1
      here <- select $ investigatorAt attrs.id
      for_ here (`addMemories` 1)
      pure l
    _ -> AMutiny <$> liftRunMessage msg attrs
