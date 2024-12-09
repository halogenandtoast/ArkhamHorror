module Arkham.Asset.Assets.ShroudOfShadows4 (
  shroudOfShadows4,
  shroudOfShadows4Effect,
  ShroudOfShadows4 (..),
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Game.Helpers (getConnectedMoveLocations)
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Matcher hiding (EnemyEvaded, RevealChaosToken)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype ShroudOfShadows4 = ShroudOfShadows4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadows4 :: AssetCard ShroudOfShadows4
shroudOfShadows4 = asset ShroudOfShadows4 Cards.shroudOfShadows4

instance HasAbilities ShroudOfShadows4 where
  getAbilities (ShroudOfShadows4 a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#evade] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage ShroudOfShadows4 where
  runMessage msg a@(ShroudOfShadows4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #willpower 2)
      createCardEffect Cards.shroudOfShadows4 (effectMetaTarget sid) source iid
      aspect
        iid
        source
        (#willpower `InsteadOf` #agility)
        (setTarget attrs <$> mkChooseEvade sid iid source)
      pure a
    Successful (Action.Evade, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      nonElite <- elem eid <$> select NonEliteEnemy
      pushAll $ EnemyEvaded iid eid : [WillMoveEnemy eid msg | nonElite]
      pure a
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <- select $ ConnectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemyId
      afterEvade $ chooseTargetM iid choices $ push . EnemyMove enemyId
      pure a
    _ -> ShroudOfShadows4 <$> liftRunMessage msg attrs

newtype ShroudOfShadows4Effect = ShroudOfShadows4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadows4Effect :: EffectArgs -> ShroudOfShadows4Effect
shroudOfShadows4Effect = cardEffect ShroudOfShadows4Effect Cards.shroudOfShadows4

instance RunMessage ShroudOfShadows4Effect where
  runMessage msg e@(ShroudOfShadows4Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      void $ runMaybeT do
        iid <- hoistMaybe attrs.target.investigator
        SkillTestTarget sid <- hoistMaybe attrs.metaTarget
        current <- MaybeT getSkillTestId
        guard $ sid == current
        lift do
          let
            handleIt assetId = do
              when (token.face == #curse) do
                locations <- getConnectedMoveLocations iid attrs.source
                stillInPlay <- selectAny $ AssetWithId assetId
                when (stillInPlay || notNull locations) do
                  chooseOrRunOneM iid do
                    when stillInPlay do
                      labeled "Place 1 Charge on Shroud of Shadows4" do
                        push $ AddUses attrs.source assetId Charge 1
                      labeled "Move to a connecting location" do
                        chooseTargetM iid locations $ moveTo attrs.source iid
          case attrs.source of
            AbilitySource (AssetSource assetId) 1 -> handleIt assetId
            AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
            AbilitySource (IndexedSource _ (AssetSource assetId)) 1 -> handleIt assetId
            _ -> error "wrong source"
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> disableReturn e
    _ -> ShroudOfShadows4Effect <$> liftRunMessage msg attrs
