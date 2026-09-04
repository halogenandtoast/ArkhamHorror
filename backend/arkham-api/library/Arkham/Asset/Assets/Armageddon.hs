module Arkham.Asset.Assets.Armageddon (armageddon, armageddonEffect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Effect.Import
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.I18n
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Armageddon = Armageddon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon :: AssetCard Armageddon
armageddon = asset Armageddon Cards.armageddon

instance HasAbilities Armageddon where
  getAbilities (Armageddon a) =
    [ controlled_ a 1
        $ ActionAbility #fight #willpower (ActionCost 1 <> assetUseCost a Charge 1)
    ]

instance RunMessage Armageddon where
  runMessage msg a@(Armageddon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifier sid source iid (DamageDealt 1)
      createSkillTestCardEffect sid Cards.armageddon Nothing source iid
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Armageddon <$> liftRunMessage msg attrs

newtype ArmageddonEffect = ArmageddonEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddonEffect :: EffectArgs -> ArmageddonEffect
armageddonEffect = cardEffect ArmageddonEffect Cards.armageddon

instance RunMessage ArmageddonEffect where
  runMessage msg (ArmageddonEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      fired <- runMaybeT do
        guard $ not attrs.finished
        guard $ token.face == #curse
        sid <- hoistMaybe attrs.skillTest
        current <- MaybeT getSkillTestId
        guard $ sid == current
        lift do
          let
            handleIt assetId = do
              enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource attrs.source

              concealed <- getConcealedIds (ForExpose $ toSource iid) iid
              stillInPlay <- selectAny $ AssetWithId assetId

              if stillInPlay || notNull enemies || notNull concealed
                then do
                  chooseOrRunOneM iid $ cardI18n $ scope "armageddon" do
                    when stillInPlay do
                      labeled "placeCharge" do
                        push $ AddUses attrs.source assetId Charge 1
                    when (notNull enemies || notNull concealed) do
                      labeled "dealDamageToEnemy" do
                        chooseDamageEnemy iid attrs.source (locationWithInvestigator iid) AnyEnemy 1
                  pure True
                else pure False
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            UseAbilitySource _ (AssetSource assetId) 1 -> handleIt assetId
            UseAbilitySource _ (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            UseAbilitySource _ (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure $ ArmageddonEffect $ if fired == Just True then finishedEffect attrs else attrs
    RepeatSkillTest _ stId
      | Just stId == attrs.skillTest ->
          ArmageddonEffect <$> liftRunMessage msg (unfinishedEffect attrs)
    _ -> ArmageddonEffect <$> liftRunMessage msg attrs
