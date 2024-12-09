module Arkham.Asset.Assets.Tonys38LongColt (tonys38LongColt, Tonys38LongColt (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Fight
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Token qualified as Token

newtype Tonys38LongColt = Tonys38LongColt AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tonys38LongColt :: AssetCard Tonys38LongColt
tonys38LongColt = asset Tonys38LongColt Cards.tonys38LongColt

instance HasAbilities Tonys38LongColt where
  getAbilities (Tonys38LongColt a) =
    [ controlledAbility a 1 (youExist $ handWith Cards.tonys38LongColt)
        $ freeReaction (Arkham.Matcher.PlayCard #after You $ isThisCard a)
    , fightAbility a 2 (assetUseCost a Ammo 1) ControlsThis
    ]

instance HasModifiersFor Tonys38LongColt where
  getModifiersFor (Tonys38LongColt a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ isAbilitySource a 2 <$> MaybeT getSkillTestSource
      bounties <- lift $ fieldMap EnemyTokens (Token.countTokens Token.Bounty) eid
      guard (bounties > 0)
      pure [SkillModifier #combat bounties]

instance RunMessage Tonys38LongColt where
  runMessage msg a@(Tonys38LongColt attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <- selectOne $ inHandOf iid <> basic (cardIs Cards.tonys38LongColt)
      for_ mcard $ push . putCardIntoPlay iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (DamageDealt 1)
      pushAll [enabled, chooseFight]
      pure a
    EnemyDefeated eid _ (isAbilitySource attrs 2 -> True) _ -> do
      hadBounty <- eid <=~> EnemyWithBounty
      bountyContracts <- selectJust $ assetIs Cards.bountyContracts
      pushWhen hadBounty $ AddUses (attrs.ability 2) bountyContracts Bounty 1
      pure a
    _ -> Tonys38LongColt <$> runMessage msg attrs
