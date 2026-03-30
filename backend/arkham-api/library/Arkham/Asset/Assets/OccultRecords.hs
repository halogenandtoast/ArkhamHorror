module Arkham.Asset.Assets.OccultRecords (occultRecords) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype OccultRecords = OccultRecords AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultRecords :: AssetCard OccultRecords
occultRecords = asset OccultRecords Cards.occultRecords

instance HasAbilities OccultRecords where
  getAbilities (OccultRecords a) =
    [ controlled
        a
        1
        (DuringTurn You <> exists (HealableInvestigator (a.ability 1) #horror $ affectsColocatedMatch You))
        $ freeTrigger (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage OccultRecords where
  runMessage msg a@(OccultRecords attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror (affectsColocated iid)
      sid <- getRandom
      chooseTargetM iid investigators \iid' -> do
        healHorror iid' (attrs.ability 1) 2
        beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      randomDiscard iid (attrs.ability 1)
      pure a
    _ -> OccultRecords <$> liftRunMessage msg attrs
