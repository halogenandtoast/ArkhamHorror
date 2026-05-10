module Arkham.Asset.Assets.DowsingRod (dowsingRod) where

import Arkham.Ability.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.I18n
import Arkham.Investigate
import Arkham.Matcher hiding (DiscoveringLastClue)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Window (Window (..), WindowType (..))

newtype DowsingRod = DowsingRod AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dowsingRod :: AssetCard DowsingRod
dowsingRod = asset DowsingRod Cards.dowsingRod

instance HasAbilities DowsingRod where
  getAbilities (DowsingRod a) = abilities a $ withActionAbility 1 do
    mustControl
    addAction #investigate
    mustExist
      $ InvestigatableLocation
      <> oneOf (YourLocation : [AccessibleFrom ForMovement YourLocation | a.ready])

instance RunMessage DowsingRod where
  runMessage msg a@(DowsingRod attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canInvestigate <- selectAny $ locationWithInvestigator iid <> InvestigatableLocation
      let isForced = attrs.ready && not canInvestigate
      accessibleLocations <- getAccessibleLocations iid attrs
      chooseOneM iid do
        when (attrs.ready && notNull accessibleLocations) $ forcedWhen isForced do
          (cardI18n $ labeled' "dowsingRod.exhaustDowsingRodAndPlace1DoomOnItToMoveToAConnectingLocatio")
            $ doStep 2 msg
        (cardI18n $ labeled' "dowsingRod.ifThisInvestigationDiscoversTheLastClueAtALocationRemove1Doo")
          $ doStep 3 msg
      doStep 1 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      investigate' <- mkInvestigate sid iid (attrs.ability 1)

      chooseOneM iid do
        (withI18n $ skillVar #willpower $ labeled' "useSkill") $ push $ withSkillType #willpower investigate'
        (withI18n $ countVar 1 $ skillVar #intellect $ labeled' "getPlus") do
          skillTestModifier sid (attrs.ability 1) iid $ SkillModifier #intellect 1
          push investigate'
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      exhaustThis attrs
      placeDoom (attrs.ability 1) attrs 1
      accessibleLocationIds <- getAccessibleLocations iid attrs
      chooseTargetM iid accessibleLocationIds (moveTo (attrs.ability 1) iid)
      pure a
    DoStep 3 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ overAttrs (setMetaKey "option2" True) a
    Do (CheckWindows windows) | getMetaKey "option2" attrs -> do
      case attrs.controller of
        Nothing -> pure a
        Just iid -> do
          let discoveredLastClue = flip any (map windowType windows) \case
                DiscoveringLastClue iid' _ -> iid == iid'
                _ -> False
          if discoveredLastClue && attrs.doom > 0
            then do
              removeDoom (attrs.ability 1) attrs 1
              pure $ overAttrs (unsetMetaKey "option2") a
            else pure a
    SkillTestEnds _ _ (isSource attrs -> True) -> do
      pure $ overAttrs (unsetMetaKey "option2") a
    _ -> DowsingRod <$> liftRunMessage msg attrs
