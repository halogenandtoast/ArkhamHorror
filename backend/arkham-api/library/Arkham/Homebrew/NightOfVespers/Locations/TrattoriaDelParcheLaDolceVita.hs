module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaDelParcheLaDolceVita (trattoriaDelParcheLaDolceVita) where

import Arkham.Ability
import Arkham.Card (genCard)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.History (HistoryField (HistoryPlayedCards), getHistoryField)
import Arkham.Helpers.Location (swapLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype TrattoriaDelParcheLaDolceVita = TrattoriaDelParcheLaDolceVita LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaDelParcheLaDolceVita :: LocationCard TrattoriaDelParcheLaDolceVita
trattoriaDelParcheLaDolceVita =
  symbolLabel
    $ location TrattoriaDelParcheLaDolceVita Cards.trattoriaDelParcheLaDolceVita 2 (Static 0)

instance HasModifiersFor TrattoriaDelParcheLaDolceVita where
  getModifiersFor (TrattoriaDelParcheLaDolceVita a) =
    modifySelectMaybe a (investigatorAt a) \iid -> do
      played <- lift $ getHistoryField #turn iid HistoryPlayedCards
      guard (null played)
      pure [ReduceCostOf AnyCard 1]

instance HasAbilities TrattoriaDelParcheLaDolceVita where
  getAbilities (TrattoriaDelParcheLaDolceVita a) =
    extend1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage TrattoriaDelParcheLaDolceVita where
  runMessage msg l@(TrattoriaDelParcheLaDolceVita attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] #any (DrawFound iid 1)
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Cards.trattoriaDelParcheLaBruttaVita
      -- This scenario's flip supplies the new side's starting clues explicitly;
      -- swapping a location preserves tokens and does not reveal it.
      placeClues attrs attrs =<< perPlayer 1
      pure l
    _ -> TrattoriaDelParcheLaDolceVita <$> liftRunMessage msg attrs
