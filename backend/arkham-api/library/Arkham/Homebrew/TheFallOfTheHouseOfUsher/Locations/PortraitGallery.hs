module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.PortraitGallery (portraitGallery) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype PortraitGallery = PortraitGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

portraitGallery :: LocationCard PortraitGallery
portraitGallery = location PortraitGallery Cards.portraitGallery 3 (PerPlayer 2)

instance HasAbilities PortraitGallery where
  getAbilities (PortraitGallery a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "portraitGallery.peek"
          $ playerLimit PerTurn
          $ restricted a 1 Here
          $ actionAbilityWithCost (HandDiscardCost 1 #any)
      , scenarioI18n $ collapseForced "portraitGallery.collapse" a 2
      ]

instance RunMessage PortraitGallery where
  runMessage msg l@(PortraitGallery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) EncounterDeckTarget [peekTopOfDeck 1] #any (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseOneM iid $ withI18n do
        labeled "discardCard" $ for_ cards (discardCard iid (attrs.ability 1))
        labeled "leaveCardOnTop" nothing
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      flipToRuined attrs
      pure l
    _ -> PortraitGallery <$> liftRunMessage msg attrs
