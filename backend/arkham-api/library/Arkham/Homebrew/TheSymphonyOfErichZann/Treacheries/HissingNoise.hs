module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.HissingNoise (hissingNoise) where

import Arkham.Ability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype HissingNoise = HissingNoise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hissingNoise :: TreacheryCard HissingNoise
hissingNoise = treachery HissingNoise Cards.hissingNoise

instance HasAbilities HissingNoise where
  getAbilities (HissingNoise a) =
    [ -- "After a Music treachery is put into play: Either lose 2 resources or take 1 damage."
      restricted a 1 (InThreatAreaOf You) $ forced $ TreacheryEntersPlay #after (TreacheryWithTrait Music)
    , -- "[action]: Test willpower (2). If you succeed: Discard Hissing Noise."
      restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage HissingNoise where
  runMessage msg t@(HissingNoise attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n $ scope "hissingNoise" do
        labeled "loseResources" $ loseResources iid (attrs.ability 1) 2
        labeled "takeDamage" $ assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 2)
      pure t
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> HissingNoise <$> liftRunMessage msg attrs
