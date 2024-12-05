module Arkham.Asset.Assets.NephthysHuntressOfBast4 (
  nephthysHuntressOfBast4,
  NephthysHuntressOfBast4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NephthysHuntressOfBast4 = NephthysHuntressOfBast4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nephthysHuntressOfBast4 :: AssetCard NephthysHuntressOfBast4
nephthysHuntressOfBast4 = ally NephthysHuntressOfBast4 Cards.nephthysHuntressOfBast4 (2, 2)

instance HasModifiersFor NephthysHuntressOfBast4 where
  getModifiersFor (NephthysHuntressOfBast4 a) = case a.controller of
    Just iid -> modified_ a iid [SkillModifier #willpower 1]
    Nothing -> pure mempty

instance HasAbilities NephthysHuntressOfBast4 where
  getAbilities (NephthysHuntressOfBast4 x) =
    [ restrictedAbility x 1 ControlsThis $ freeReaction (TokensWouldBeRemovedFromChaosBag #when #bless)
    , restrictedAbility x 2 (ControlsThis <> exists (be x <> AssetWithSealedChaosTokens 3 #bless))
        $ FastAbility (exhaust x)
    ]

getRemovedBlessTokens :: [Window] -> [ChaosToken]
getRemovedBlessTokens = foldMap \case
  (windowType -> Window.TokensWouldBeRemovedFromChaosBag tokens) -> filter ((== #bless) . (.face)) tokens
  _ -> []

instance RunMessage NephthysHuntressOfBast4 where
  runMessage msg a@(NephthysHuntressOfBast4 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getRemovedBlessTokens -> tokens) _ -> do
      for_ tokens $ \token ->
        pushAll [SealChaosToken token, SealedChaosToken token $ toTarget attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      blessTokens <-
        take 3 <$> filterM (<=~> IncludeSealed (ChaosTokenFaceIs #bless)) attrs.sealedChaosTokens
      player <- getPlayer iid
      chooseOrRunOne iid
        $ [Label "Release 3 {bless} tokens" $ map UnsealChaosToken blessTokens]
        <> [ Label
            "Return 3 {bless} tokens to the pool to do 2 damage to an enemy at your location"
            [ ReturnChaosTokensToPool blessTokens
            , Msg.chooseOrRunOne
                player
                [targetLabel enemy [EnemyDamage enemy $ nonAttack (attrs.ability 2) 2] | enemy <- enemies]
            ]
           | notNull enemies
           ]
      pure a
    _ -> NephthysHuntressOfBast4 <$> liftRunMessage msg attrs
