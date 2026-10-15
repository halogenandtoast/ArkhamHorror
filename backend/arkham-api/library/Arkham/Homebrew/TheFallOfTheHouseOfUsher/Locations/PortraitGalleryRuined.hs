module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.PortraitGalleryRuined (portraitGalleryRuined) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype PortraitGalleryRuined = PortraitGalleryRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

portraitGalleryRuined :: LocationCard PortraitGalleryRuined
portraitGalleryRuined = location PortraitGalleryRuined Cards.portraitGalleryRuined 3 (Static 0)

instance HasAbilities PortraitGalleryRuined where
  getAbilities (PortraitGalleryRuined a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "portraitGalleryRuined.theMadTrist"
      $ skillTestAbility
      $ restricted a 1 (Here <> Remembered unearthedAnArchaicBook <> noMadTristHasEnteredPlay) actionAbility

instance RunMessage PortraitGalleryRuined where
  runMessage msg l@(PortraitGalleryRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.theMadTristDragonsCunning
      pure l
    _ -> PortraitGalleryRuined <$> liftRunMessage msg attrs
