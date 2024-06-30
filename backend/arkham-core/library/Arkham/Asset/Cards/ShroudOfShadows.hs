module Arkham.Asset.Cards.ShroudOfShadows (shroudOfShadows, shroudOfShadowsEffect, ShroudOfShadows (..)) where

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

newtype ShroudOfShadows = ShroudOfShadows AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadows :: AssetCard ShroudOfShadows
shroudOfShadows = asset ShroudOfShadows Cards.shroudOfShadows

instance HasAbilities ShroudOfShadows where
  getAbilities (ShroudOfShadows a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#evade] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage ShroudOfShadows where
  runMessage msg a@(ShroudOfShadows attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      chooseEvade <-
        aspect iid source (#willpower `InsteadOf` #agility) (setTarget attrs <$> mkChooseEvade iid source)
      pushAll
        $ createCardEffect Cards.shroudOfShadows Nothing source iid
        : leftOr chooseEvade
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
    _ -> ShroudOfShadows <$> runMessage msg attrs

newtype ShroudOfShadowsEffect = ShroudOfShadowsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudOfShadowsEffect :: EffectArgs -> ShroudOfShadowsEffect
shroudOfShadowsEffect = cardEffect ShroudOfShadowsEffect Cards.shroudOfShadows

instance RunMessage ShroudOfShadowsEffect where
  runMessage msg e@(ShroudOfShadowsEffect attrs) = case msg of
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
                  $ [Label "Place 1 Charge on Shroud of Shadows" [AddUses assetId Charge 1] | stillInPlay]
                  <> [ Label
                        "Move to a connecting location"
                        [ chooseOne
                            player
                            [targetLabel target [Move $ move attrs.source iid target] | target <- targets]
                        ]
                     ]
                | stillInPlay || notNull targets
                ]
              <> [ disable attrs
                 ]
      case attrs.source of
        AbilitySource (AssetSource assetId) 1 -> handleIt assetId
        AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 -> handleIt assetId
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> do
      push (disable attrs)
      pure e
    _ -> ShroudOfShadowsEffect <$> runMessage msg attrs
