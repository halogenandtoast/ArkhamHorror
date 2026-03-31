module Arkham.Asset.Assets.RitualDagger3 (ritualDagger3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher
import Arkham.Trait (Trait (Spell))

newtype RitualDagger3 = RitualDagger3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualDagger3 :: AssetCard RitualDagger3
ritualDagger3 = asset RitualDagger3 Cards.ritualDagger3

instance HasAbilities RitualDagger3 where
  getAbilities (RitualDagger3 a) =
    [ controlled_ a 1 fightAction_
    , controlled_ a 2
        $ triggered
          (PlayEventDiscarding #after You (EventWithTrait Spell))
          (DirectDamageCost (toSource a) You 1 <> exhaust a)
    ]

instance RunMessage RitualDagger3 where
  runMessage msg a@(RitualDagger3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      addSkillValue sid attrs iid #willpower
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getPlayedEvent -> eid) _ -> do
      -- eventModifier (attrs.ability 1) eid (SetAfterPlay ShuffleThisBackIntoDeck)
      shuffleIntoDeck iid eid
      pure a
    _ -> RitualDagger3 <$> liftRunMessage msg attrs
