module Arkham.Asset.Cards.MaskedCarnevaleGoer_17
  ( maskedCarnevaleGoer_17
  , MaskedCarnevaleGoer_17(..)
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

newtype MaskedCarnevaleGoer_17 = MaskedCarnevaleGoer_17 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_17 :: AssetCard MaskedCarnevaleGoer_17
maskedCarnevaleGoer_17 =
  asset MaskedCarnevaleGoer_17 Cards.maskedCarnevaleGoer_17

instance HasAbilities MaskedCarnevaleGoer_17 where
  getAbilities (MaskedCarnevaleGoer_17 x) =
    [ restrictedAbility
        x
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]

locationOf :: AssetAttrs -> LocationId
locationOf AssetAttrs { assetPlacement } = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_17 where
  runMessage msg a@(MaskedCarnevaleGoer_17 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ Flip iid (toSource iid) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let
        lid = locationOf attrs
        donLagorio = lookupCard Enemies.donLagorio (toCardId attrs)
      investigators <- selectList $ investigatorAt $ locationOf attrs
      lead <- getLead
      (enemyId, createDonLagorio) <- createEnemyAt donLagorio lid Nothing
      pushAll
        [ createDonLagorio
        , Flipped
          (toSource attrs)
          donLagorio
        , chooseOrRunOneAtATime
          lead
          [ targetLabel
              investigator
              [EnemyAttack $ enemyAttack enemyId investigator]
          | investigator <- investigators
          ]
        ]
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let donLagorio = lookupCard Enemies.donLagorio (toCardId attrs)
      lead <- getLead
      pushAll
        [ FocusCards [donLagorio]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_17 <$> runMessage msg attrs
