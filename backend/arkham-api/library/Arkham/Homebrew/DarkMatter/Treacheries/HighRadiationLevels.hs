module Arkham.Homebrew.DarkMatter.Treacheries.HighRadiationLevels (highRadiationLevels) where

import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype HighRadiationLevels = HighRadiationLevels TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRadiationLevels :: TreacheryCard HighRadiationLevels
highRadiationLevels = treachery HighRadiationLevels Cards.highRadiationLevels

{- | "Revelation - Test [agility] or [combat] (3). If you fail, deal 1 direct
damage to your investigator and to each [[Ally]] asset you control. If you
control Radiation Tablets, you automatically succeed at this test."
-}
instance RunMessage HighRadiationLevels where
  runMessage msg t@(HighRadiationLevels attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasTablets <- selectAny $ assetIs Assets.radiationTablets <> assetControlledBy iid
      unless hasTablets do
        sid <- getRandom
        chooseOneM iid $ for_ [#agility, #combat] \skill ->
          skillLabeled skill $ revelationSkillTest sid iid attrs skill (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directDamage iid attrs 1
      allies <- select $ #ally <> assetControlledBy iid
      for_ allies \ally -> dealAssetDamage ally attrs 1
      pure t
    _ -> HighRadiationLevels <$> liftRunMessage msg attrs
