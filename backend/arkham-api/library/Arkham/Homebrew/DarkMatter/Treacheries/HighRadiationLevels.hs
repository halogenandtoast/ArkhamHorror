module Arkham.Homebrew.DarkMatter.Treacheries.HighRadiationLevels (highRadiationLevels) where

import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillTest.Base
import Arkham.Treachery.Import.Lifted

newtype HighRadiationLevels = HighRadiationLevels TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRadiationLevels :: TreacheryCard HighRadiationLevels
highRadiationLevels = treachery HighRadiationLevels Cards.highRadiationLevels

instance RunMessage HighRadiationLevels where
  runMessage msg t@(HighRadiationLevels attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasTablets <- selectAny $ assetIs Assets.radiationTablets <> assetControlledBy iid
      sid <- getRandom
      when hasTablets $ skillTestModifier sid attrs sid SkillTestAutomaticallySucceeds
      chooseBeginSkillTestEdit sid iid attrs iid [#agility, #combat] (Fixed 3) setIsRevelation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directDamage iid attrs 1
      allies <- select $ #ally <> assetControlledBy iid
      for_ allies \ally -> dealAssetDamage ally attrs 1
      pure t
    _ -> HighRadiationLevels <$> liftRunMessage msg attrs
