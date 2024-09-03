module Arkham.Asset.Cards.WickedAthame (wickedAthame, WickedAthame (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Use (toStartingUses)
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token

newtype WickedAthame = WickedAthame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wickedAthame :: AssetCard WickedAthame
wickedAthame = asset WickedAthame Cards.wickedAthame

instance HasAbilities WickedAthame where
  getAbilities (WickedAthame x) =
    [ restrictedAbility x 1 ControlsThis $ fightAction (AddCurseTokensCost 1 3)
    ]

instance RunMessage WickedAthame where
  runMessage msg a@(WickedAthame attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (addedCurseTokenPayment -> x) -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat (x * 2))
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        selectOneToHandle iid (attrs.ability 1)
          $ assetControlledBy iid
          <> oneOf [AssetWithUses Charge, AssetWithUses Offering]
          <> AssetNotAtUsesX
        pure ()
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      void $ runMaybeT do
        (uType, _) <-
          MaybeT $ fmap (listToMaybe . mapToList) . toStartingUses =<< field AssetStartingUses aid
        lift $ placeTokens (attrs.ability 1) (toTarget aid) uType 1
      pure a
    _ -> WickedAthame <$> liftRunMessage msg attrs
