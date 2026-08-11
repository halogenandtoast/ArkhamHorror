module Arkham.Homebrew.DarkMatter.Locations.Library (library) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern School)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Library = Library LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Shroud is X, the number of adjacent locations.
library :: LocationCard Library
library = location Library Cards.library 0 (PerPlayer 1)

instance HasModifiersFor Library where
  getModifiersFor (Library a) = do
    adjacent <- selectCount $ connectedFrom (be a)
    modifySelf a [ShroudModifier adjacent]

{- | "[action] If there are no clues on Library: Switch Library with any other
[[School]] location and place 1[per_investigator] clues on it, from the token
bank."
-}
instance HasAbilities Library where
  getAbilities (Library a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues) actionAbility

instance RunMessage Library where
  runMessage msg l@(Library attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <- select $ LocationWithTrait School <> not_ (be attrs)
      chooseOrRunOneM iid $ targets others \other -> do
        push $ ScenarioSpecific "switchLocations" (toJSON (attrs.id, other))
        n <- perPlayer 1
        placeClues (attrs.ability 1) attrs n
      pure l
    _ -> Library <$> liftRunMessage msg attrs
