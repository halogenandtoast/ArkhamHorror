module Arkham.Asset.Cards.ShroudOfShadows4 (shroudOfShadows4, shroudOfShadows4Effect, ShroudOfShadows4 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Evade
import Arkham.Matcher hiding (EnemyEvaded, RevealChaosToken)
import Arkham.Movement
import Arkham.Prelude

newtype ShroudOfShadows4 = ShroudOfShadows4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadows4 :: AssetCard ShroudOfShadows4
shroudOfShadows4 = asset ShroudOfShadows4 Cards.shroudOfShadows4

instance HasAbilities ShroudOfShadows4 where
  getAbilities (ShroudOfShadows4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#evade] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage ShroudOfShadows4 where
  runMessage msg a@(ShroudOfShadows4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      chooseEvade <-
        aspect iid source (#willpower `InsteadOf` #agility) (setTarget attrs <$> mkChooseEvade iid source)
      pushAll
        $ [ skillTestModifier source iid (SkillModifier #willpower 2)
          , createCardEffect Cards.shroudOfShadows4 Nothing source iid
          ]
        <> leftOr chooseEvade
      pure a
    Successful (Action.Evade, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      nonElite <- elem eid <$> select NonEliteEnemy
      pushAll $ EnemyEvaded iid eid : [WillMoveEnemy eid msg | nonElite]
      pure a
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _) | isTarget attrs target -> do
      choices <- select $ ConnectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemyId
      player <- getPlayer iid
      let
        enemyMoveChoices =
          chooseOne player [targetLabel choice [EnemyMove enemyId choice] | choice <- choices]
      insertAfterMatching [enemyMoveChoices] \case
        AfterEvadeEnemy {} -> True
        _ -> False
      pure a
    _ -> ShroudOfShadows4 <$> runMessage msg attrs

newtype ShroudOfShadows4Effect = ShroudOfShadows4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadows4Effect :: EffectArgs -> ShroudOfShadows4Effect
shroudOfShadows4Effect = cardEffect ShroudOfShadows4Effect Cards.shroudOfShadows4

instance RunMessage ShroudOfShadows4Effect where
  runMessage msg e@(ShroudOfShadows4Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      let
        handleIt assetId = do
          when (token.face == #curse) do
            targets <- getConnectedMoveLocations iid (toSource attrs)
            stillInPlay <- selectAny $ AssetWithId assetId
            player <- getPlayer iid
            pushAll
              $ [ chooseOrRunOne
                  player
                  $ [Label "Place 1 Charge on Shroud of Shadows4" [AddUses attrs.source assetId Charge 1] | stillInPlay]
                  <> [ Label
                        "Move to a connecting location"
                        [ chooseOne
                            player
                            [targetLabel target [Move $ move attrs.source iid target] | target <- targets]
                        ]
                     ]
                | stillInPlay || notNull targets
                ]
      case attrs.source of
        AbilitySource (AssetSource assetId) 1 -> handleIt assetId
        AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> do
      push (disable attrs)
      pure e
    _ -> ShroudOfShadows4Effect <$> runMessage msg attrs
