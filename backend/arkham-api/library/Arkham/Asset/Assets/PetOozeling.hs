module Arkham.Asset.Assets.PetOozeling (petOozeling) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.RequestedChaosTokenStrategy
import Arkham.Enemy.Types (Field (EnemyRemainingHealth))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype PetOozeling = PetOozeling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

petOozeling :: AssetCard PetOozeling
petOozeling = assetWith PetOozeling Cards.petOozeling (healthL ?~ 3)

instance HasAbilities PetOozeling where
  getAbilities (PetOozeling a) =
    [ restricted a 1 (ControlsThis <> exists (EnemyAt YourLocation <> NonEliteEnemy)) $ FastAbility Free ]

instance RunMessage PetOozeling where
  runMessage msg a@(PetOozeling attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> NonEliteEnemy
      enemies' <- filterM (fieldMap EnemyRemainingHealth (maybe False (<= 2))) enemies
      chooseTargetM iid enemies' \eid -> do
        defeatEnemy eid iid (attrs.ability 1)
        healAllDamage (attrs.ability 1) attrs
        placeTokens (attrs.ability 1) attrs #resource 1
        push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal $ attrs.token #resource + 1) SetAside
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      when (any ((== AutoFail) . chaosTokenFace) tokens) do
        sufferPhysicalTrauma iid 1
        investigatorDefeated (attrs.ability 1) iid
      push $ ResetChaosTokens (attrs.ability 1)
      pure a
    _ -> PetOozeling <$> liftRunMessage msg attrs
