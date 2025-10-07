module Arkham.Asset.Assets.NephthysHuntressOfBast4 (nephthysHuntressOfBast4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NephthysHuntressOfBast4 = NephthysHuntressOfBast4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nephthysHuntressOfBast4 :: AssetCard NephthysHuntressOfBast4
nephthysHuntressOfBast4 = ally NephthysHuntressOfBast4 Cards.nephthysHuntressOfBast4 (2, 2)

instance HasModifiersFor NephthysHuntressOfBast4 where
  getModifiersFor (NephthysHuntressOfBast4 a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities NephthysHuntressOfBast4 where
  getAbilities (NephthysHuntressOfBast4 x) =
    [ controlled_ x 1 $ freeReaction (TokensWouldBeRemovedFromChaosBag #when #bless)
    , controlled x 2 (exists (be x <> AssetWithSealedChaosTokens 3 #bless)) $ FastAbility (exhaust x)
    ]

getRemovedBlessTokens :: [Window] -> [ChaosToken]
getRemovedBlessTokens = foldMap \case
  (windowType -> Window.TokensWouldBeRemovedFromChaosBag tokens) -> filter ((== #bless) . (.face)) tokens
  _ -> []

instance RunMessage NephthysHuntressOfBast4 where
  runMessage msg a@(NephthysHuntressOfBast4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getRemovedBlessTokens -> tokens) _ -> do
      for_ tokens $ sealChaosToken iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      concealed <- getConcealedIds iid
      blessTokens <-
        take 3 <$> filterM (<=~> IncludeSealed (ChaosTokenFaceIs #bless)) attrs.sealedChaosTokens
      chooseOrRunOneM iid do
        labeled "Release 3 {bless} tokens" $ for_ blessTokens unsealChaosToken
        when (notNull enemies || notNull concealed) do
          labeled "Return 3 {bless} tokens to the pool to do 2 damage to an enemy at your location" do
            push $ ReturnChaosTokensToPool blessTokens
            chooseDamageEnemy iid (attrs.ability 2) (locationWithInvestigator iid) AnyEnemy 2
      pure a
    _ -> NephthysHuntressOfBast4 <$> liftRunMessage msg attrs
