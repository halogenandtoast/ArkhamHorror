module Arkham.Location.Cards.ParlorTheMidwinterGala (parlorTheMidwinterGala) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Guest))

newtype ParlorTheMidwinterGala = ParlorTheMidwinterGala LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Parlor' from The Midwinter Gala (#71014).
parlorTheMidwinterGala :: LocationCard ParlorTheMidwinterGala
parlorTheMidwinterGala = location ParlorTheMidwinterGala Cards.parlorTheMidwinterGala 6 (PerPlayer 3)

instance HasModifiersFor ParlorTheMidwinterGala where
  getModifiersFor (ParlorTheMidwinterGala a) = do
    guests <- selectCount $ assetAt a <> AssetWithTrait Guest
    modifySelf a [ShroudModifier (-(min guests 5))]

instance HasAbilities ParlorTheMidwinterGala where
  getAbilities (ParlorTheMidwinterGala a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ FastAbility Free

instance RunMessage ParlorTheMidwinterGala where
  runMessage msg l@(ParlorTheMidwinterGala attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- TODO: Implement Jewel exhaust ability
      pure l
    _ -> ParlorTheMidwinterGala <$> runMessage msg attrs
