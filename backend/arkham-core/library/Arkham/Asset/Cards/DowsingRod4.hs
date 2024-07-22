module Arkham.Asset.Cards.DowsingRod4 (dowsingRod4, DowsingRod4 (..)) where

import Arkham.Ability.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigate
import Arkham.Matcher hiding (DiscoveringLastClue)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Movement
import Arkham.Window (Window (..), WindowType (..))

newtype DowsingRod4 = DowsingRod4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dowsingRod4 :: AssetCard DowsingRod4
dowsingRod4 = asset DowsingRod4 Cards.dowsingRod4

instance HasAbilities DowsingRod4 where
  getAbilities (DowsingRod4 a) = abilities a $ withActionAbility 1 do
    mustControl
    addAction #investigate
    mustExist $ InvestigatableLocation <> oneOf (YourLocation : [AccessibleFrom YourLocation | a.ready])

instance RunMessage DowsingRod4 where
  runMessage msg a@(DowsingRod4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canInvestigate <- selectAny $ locationWithInvestigator iid <> InvestigatableLocation
      let isForced = attrs.ready && not canInvestigate
      accessibleLocations <- getAccessibleLocations iid attrs
      chooseOneM iid do
        when (attrs.ready && notNull accessibleLocations) $ forcedWhen isForced do
          labeled
            "Exhaust Dowsing Rod and place 1 doom on it to move to a connecting location and get +2 skill value for this investigation."
            $ doStep 2 msg
        labeled
          "If this investigation discovers the last clue at a location, ready Dowsing Rod and remove all doom from it."
          $ doStep 3 msg
      doStep 1 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      investigate <- mkInvestigate sid iid (attrs.ability 1)

      chooseOneM iid do
        labeled "Use your {willpower}" $ push $ withSkillType #willpower investigate
        labeled "get +1 {intellect}" do
          skillTestModifier sid (attrs.ability 1) iid $ SkillModifier #intellect 1
          push investigate
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ Exhaust (toTarget attrs)
      placeDoom (attrs.ability 1) attrs 1
      accessibleLocationIds <- getAccessibleLocations iid attrs
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid $ AnySkillValue 2
      chooseOne iid $ targetLabels accessibleLocationIds (only . Move . move attrs iid)
      pure a
    DoStep 3 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ overAttrs (setMetaKey "option2" True) a
    RunWindow iid windows | attrs `controlledBy` iid && getMetaKey "option2" attrs -> do
      let discoveredLastClue = flip any (map windowType windows) \case
            DiscoveringLastClue iid' _ -> iid == iid'
            _ -> False
      pushWhen (discoveredLastClue && attrs.exhausted) $ Ready (toTarget attrs)
      if discoveredLastClue && attrs.doom > 0
        then do
          push $ RemoveAllDoom (attrs.ability 1) (toTarget attrs)
          pure $ overAttrs (unsetMetaKey "option2") a
        else pure a
    SkillTestEnds _ _ (isSource attrs -> True) -> do
      pure $ overAttrs (unsetMetaKey "option2") a
    _ -> DowsingRod4 <$> liftRunMessage msg attrs
