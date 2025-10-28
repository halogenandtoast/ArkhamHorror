module Arkham.Location.Cards.WineCellar (wineCellar) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WineCellar = WineCellar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wineCellar :: LocationCard WineCellar
wineCellar = location WineCellar Cards.wineCellar 5 (PerPlayer 1)

instance HasModifiersFor WineCellar where
  getModifiersFor (WineCellar a) = do
    modifySelect
      a
      (location_ "Victorian Halls")
      [ConnectedToWhen "Victorian Halls" (LocationWithId a.id)]

instance HasAbilities WineCellar where
  getAbilities (WineCellar a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage WineCellar where
  runMessage msg l@(WineCellar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck (#monster <> #enemy)
      pure l
    RequestedEncounterCard (isAbilitySource attrs 1 -> True) (Just iid) (Just ec) -> do
      setCardAside $ toCard ec
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> WineCellar <$> liftRunMessage msg attrs
