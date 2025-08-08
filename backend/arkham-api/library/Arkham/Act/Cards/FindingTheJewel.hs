module Arkham.Act.Cards.FindingTheJewel (findingTheJewel) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.SlotType
import Arkham.Trait
import Data.List.Extra (nubOrd)

newtype FindingTheJewel = FindingTheJewel ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingTheJewel :: ActCard FindingTheJewel
findingTheJewel = act (2, A) FindingTheJewel Cards.findingTheJewel Nothing

instance HasModifiersFor FindingTheJewel where
  getModifiersFor (FindingTheJewel a) = modifySelect a (AssetWithTrait Guest) [DoNotTakeUpSlot AllySlot]

instance HasAbilities FindingTheJewel where
  getAbilities = actAbilities1 \a ->
    skillTestAbility
      $ restricted
        a
        1
        (exists $ AssetWithTrait Guest <> at_ YourLocation <> not_ (AssetControlledBy You))
        parleyAction_

instance RunMessage FindingTheJewel where
  runMessage msg a@(FindingTheJewel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <-
        select $ AssetWithTrait Guest <> at_ (locationWithInvestigator iid) <> not_ (assetControlledBy iid)
      chooseTargetM iid assets (handleTarget iid (attrs.ability 1))
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      card <- fetchCard aid
      let
        skillKinds = nubOrd $ flip mapMaybe card.icons \case
          SkillIcon kind -> Just kind
          _ -> Nothing
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) aid skillKinds (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= traverse_ \case
        AssetTarget aid -> takeControlOfAsset iid aid
        _ -> error "Wrong target"
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FindingTheJewel <$> liftRunMessage msg attrs
