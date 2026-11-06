module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaDelParcheLaBruttaVita (trattoriaDelParcheLaBruttaVita) where

import Arkham.Ability
import Arkham.Card (genCard)
import Arkham.Helpers.Location (swapLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TrattoriaDelParcheLaBruttaVita = TrattoriaDelParcheLaBruttaVita LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaDelParcheLaBruttaVita :: LocationCard TrattoriaDelParcheLaBruttaVita
trattoriaDelParcheLaBruttaVita =
  symbolLabel
    $ location TrattoriaDelParcheLaBruttaVita Cards.trattoriaDelParcheLaBruttaVita 2 (PerPlayer 1)

instance HasModifiersFor TrattoriaDelParcheLaBruttaVita where
  getModifiersFor (TrattoriaDelParcheLaBruttaVita a) =
    modifySelf
      a
      [AdditionalCostToLeave $ SkillTestCost (IndexedSource 1 (LocationSource a.id)) #agility (Fixed 2)]

instance HasAbilities TrattoriaDelParcheLaBruttaVita where
  getAbilities (TrattoriaDelParcheLaBruttaVita a) =
    extend1 a $ restricted a 1 (exists $ investigatorAt a) $ forced $ RoundEnds #when

instance RunMessage TrattoriaDelParcheLaBruttaVita where
  runMessage msg l@(TrattoriaDelParcheLaBruttaVita attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid ->
        directDamageAndHorror iid (attrs.ability 1) 1 1
      pure l
    FailedThisSkillTest iid (isIndexedSource 1 attrs -> True) -> do
      chooseOneM iid $ withI18n do
        labeled "cancelMove" $ cancelMovement attrs iid
        labeled "takeOneDamage" $ assignDamage iid attrs 1
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Cards.trattoriaDelParcheLaDolceVita
      pure l
    _ -> TrattoriaDelParcheLaBruttaVita <$> liftRunMessage msg attrs
