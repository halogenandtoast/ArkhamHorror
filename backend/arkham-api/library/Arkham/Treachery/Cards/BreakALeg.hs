module Arkham.Treachery.Cards.BreakALeg (breakALeg) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BreakALeg = BreakALeg TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakALeg :: TreacheryCard BreakALeg
breakALeg = treachery BreakALeg Cards.breakALeg

instance RunMessage BreakALeg where
  runMessage msg t@(BreakALeg attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      hasAsset <- selectAny $ assetControlledBy iid <> #item <> AssetNonStory <> DiscardableAsset
      chooseOneM iid do
        withI18n $ countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        scenarioI18n $ labeledValidate' hasAsset "breakALeg.option" $ do_ msg
      pure t
    Do (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      assets <- select $ assetControlledBy iid <> #item <> AssetNonStory <> DiscardableAsset
      chooseOrRunOneM iid $ targets assets $ toDiscardBy iid attrs
      pure t
    _ -> BreakALeg <$> liftRunMessage msg attrs
