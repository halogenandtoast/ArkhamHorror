module Arkham.Asset.Assets.PetOozeling (petOozeling) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PetOozeling = PetOozeling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

petOozeling :: AssetCard PetOozeling
petOozeling = assetWith PetOozeling Cards.petOozeling (healthL ?~ 3)

instance HasAbilities PetOozeling where
  getAbilities (PetOozeling a) =
    [ controlled
        a
        1
        (exists $ EnemyAt YourLocation <> NonEliteEnemy <> EnemyWithRemainingHealth (atMost 2))
        $ FastAbility Free
    ]

instance RunMessage PetOozeling where
  runMessage msg a@(PetOozeling attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ EnemyAt (locationWithInvestigator iid)
          <> NonEliteEnemy
          <> EnemyWithRemainingHealth (atMost 2)
      chooseTargetM iid enemies \eid -> do
        defeatEnemy eid iid (attrs.ability 1)
        healAllDamage (attrs.ability 1) attrs
        placeTokens (attrs.ability 1) attrs #resource 1
        requestChaosTokens iid (attrs.ability 1) (attrs.token #resource + 1)
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      when (any ((== AutoFail) . chaosTokenFace) tokens) do
        sufferPhysicalTrauma iid 1
        investigatorDefeated (attrs.ability 1) iid
      resetChaosTokens (attrs.ability 1)
      pure a
    _ -> PetOozeling <$> liftRunMessage msg attrs
