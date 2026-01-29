module Arkham.Asset.Assets.LuckyCharm (luckyCharm) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Ref
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype LuckyCharm = LuckyCharm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyCharm :: AssetCard LuckyCharm
luckyCharm = assetWith LuckyCharm Cards.luckyCharm (sanityL ?~ 2)

instance HasAbilities LuckyCharm where
  getAbilities (LuckyCharm a) =
    [ controlled
        a
        1
        ( DifferentTargetsExist
            (TargetControlledBy You <> TargetMatchesAny [TargetWithSanity, TargetWithHealth])
            (TargetAtLocation YourLocation <> TargetMatchesAny [TargetWithHorror, TargetWithDamage])
        )
        $ freeTrigger (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage LuckyCharm where
  runMessage msg a@(LuckyCharm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      horrorOk <- selectAny $ TargetControlledBy (InvestigatorWithId iid) <> TargetWithSanity
      damageOk <- selectAny $ TargetControlledBy (InvestigatorWithId iid) <> TargetWithHealth
      let
        matcher
          | horrorOk && damageOk = TargetMatchesAny [TargetWithHorror, TargetWithDamage]
          | horrorOk = TargetWithHorror
          | otherwise = TargetWithDamage
      selectOneToHandle iid (attrs.ability 1) $ TargetAtLocation (locationWithInvestigator iid) <> matcher
      pure a
    HandleTargetChoice iid (isSource attrs -> True) from -> do
      damage <- matches from TargetWithDamage
      horror <- matches from TargetWithHorror

      horrorTargets <-
        if horror
          then
            select
              $ TargetControlledBy (InvestigatorWithId iid)
              <> TargetWithSanity
              <> NotTarget (TargetIs from)
          else pure []
      damageTargets <-
        if damage
          then
            select
              $ TargetControlledBy (InvestigatorWithId iid)
              <> TargetWithHealth
              <> NotTarget (TargetIs from)
          else pure []

      chooseTargetM iid (nub $ horrorTargets <> damageTargets) \destination -> do
        chooseOrRunOneM iid do
          when (destination `elem` horrorTargets) do
            labeled "Move 1 horror"
              $ moveTokens (attrs.ability 1) (targetToSource from) destination Horror 1
          when (destination `elem` damageTargets)
            $ labeled "Move 1 damage"
            $ moveTokens (attrs.ability 1) (targetToSource from) destination Damage 1
      pure a
    _ -> LuckyCharm <$> liftRunMessage msg attrs
