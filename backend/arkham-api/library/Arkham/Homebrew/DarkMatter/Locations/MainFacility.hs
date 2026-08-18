module Arkham.Homebrew.DarkMatter.Locations.MainFacility (mainFacility) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MainFacility = MainFacility LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainFacility :: LocationCard MainFacility
mainFacility = symbolLabel $ location MainFacility Cards.mainFacility 0 (PerPlayer 2)

instance HasModifiersFor MainFacility where
  getModifiersFor (MainFacility a) = do
    connecting <- selectCount $ connectedFrom (be a)
    modifySelf a [ShroudModifier connecting]

instance HasAbilities MainFacility where
  getAbilities (MainFacility a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> exists (RevealedLocation <> LocationWithoutClues)) doubleActionAbility

instance RunMessage MainFacility where
  runMessage msg l@(MainFacility attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      candidates <- select $ RevealedLocation <> LocationWithoutClues
      chooseOrRunOneM iid $ targets candidates $ placeTokensOn (attrs.ability 1) #resource 1
      pure l
    _ -> MainFacility <$> liftRunMessage msg attrs
