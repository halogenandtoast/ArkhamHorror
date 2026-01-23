module Arkham.Treachery.Cards.Calcification (calcification) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Cost
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (PerformAction)

newtype Calcification = Calcification TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

calcification :: TreacheryCard Calcification
calcification = treachery Calcification Cards.calcification

instance HasAbilities Calcification where
  getAbilities (Calcification a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ PerformAction #when You (FirstActionMatchOfRound #move)
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage Calcification where
  runMessage msg t@(Calcification attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      whenNone (treacheryInThreatAreaOf iid <> treacheryIs Cards.calcification) do
        placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseOneM iid $ campaignI18n do
        labeled' "calcification.pay" do
          payEffectCost iid attrs $ DirectDamageCost (attrs.ability 2) (InvestigatorWithId iid) 1
          skillTestModifier sid (attrs.ability 1) sid SkillTestAutomaticallySucceeds
        unscoped skip_
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Calcification <$> liftRunMessage msg attrs
