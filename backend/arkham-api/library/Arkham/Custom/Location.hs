{- | The runner behind a debug-authored custom location. See "Arkham.Custom.Enemy".

Shroud and clue value are not 'CardDef' fields, so they come from the def's meta
(@shroud@/@revealClues@), which is where the debug editor puts them.
-}
module Arkham.Custom.Location (CustomLocation (..), customLocation) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Card.CustomCard (customMeta)
import Arkham.Custom.Ability (customAbilities, customModifiers, runCustomAbility, runCustomHandlers)
import Arkham.GameValue
import Arkham.Location.Import.Lifted

newtype CustomLocation = CustomLocation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customLocation :: CardDef -> LocationCard CustomLocation
customLocation def =
  location CustomLocation def (customMeta "shroud" 0 def) (customMeta "revealClues" (Static 0) def)

instance HasModifiersFor CustomLocation where
  getModifiersFor (CustomLocation a) = customModifiers a

instance HasAbilities CustomLocation where
  getAbilities (CustomLocation a) = extend a (customAbilities a)

instance RunMessage CustomLocation where
  runMessage msg x@(CustomLocation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) idx -> do
      runCustomAbility attrs iid idx
      pure x
    _ -> do
      runCustomHandlers attrs msg
      CustomLocation <$> liftRunMessage msg attrs
