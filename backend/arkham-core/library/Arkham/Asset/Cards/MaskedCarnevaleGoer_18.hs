module Arkham.Asset.Cards.MaskedCarnevaleGoer_18 (
  maskedCarnevaleGoer_18,
  MaskedCarnevaleGoer_18 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_18 :: AssetCard MaskedCarnevaleGoer_18
maskedCarnevaleGoer_18 =
  asset MaskedCarnevaleGoer_18 Cards.maskedCarnevaleGoer_18

instance HasAbilities MaskedCarnevaleGoer_18 where
  getAbilities (MaskedCarnevaleGoer_18 x) =
    [ restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]

locationOf :: AssetAttrs -> LocationId
locationOf AssetAttrs {assetPlacement} = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ Flip iid (toAbilitySource attrs 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget attrs -> True) -> do
      let
        lid = locationOf attrs
        elisabettaMagro = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      investigators <- selectList $ investigatorAt $ locationOf attrs
      lead <- getLead
      (enemyId, createElisabettaMagro) <-
        createEnemyAt
          elisabettaMagro
          lid
          Nothing
      pushAll $
        [ createElisabettaMagro
        , Flipped (toSource attrs) elisabettaMagro
        ]
          <> [ chooseOrRunOneAtATime
              lead
              [ targetLabel
                investigator
                [EnemyAttack $ enemyAttack enemyId investigator]
              | investigator <- investigators
              ]
             | isAbilitySource attrs 1 source
             , notNull investigators
             ]
      pure a
    LookAtRevealed iid source target | isTarget a target -> do
      let elisabettaMagro = lookupCard Enemies.elisabettaMagro (toCardId attrs)
      lead <- getLead
      a
        <$ pushAll
          [ FocusCards [elisabettaMagro]
          , chooseOne lead [Label "Continue" [UnfocusCards]]
          , Flip iid source target
          ]
    _ -> MaskedCarnevaleGoer_18 <$> runMessage msg attrs
