module Arkham.Asset.Cards.MaskedCarnevaleGoer_20
  ( maskedCarnevaleGoer_20
  , MaskedCarnevaleGoer_20(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 =
  asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

instance HasAbilities MaskedCarnevaleGoer_20 where
  getAbilities (MaskedCarnevaleGoer_20 x) =
    [ restrictedAbility x 1 OnSameLocation
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ClueCost 1
    ]

locationOf :: AssetAttrs -> LocationId
locationOf AssetAttrs { assetPlacement } = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ Flip iid (toSource iid) (toTarget attrs)
        , FindEnemy (EnemyWithCardId $ toCardId attrs) (toTarget attrs)
        ]
      pure a
    FindEnemy matcher (isTarget attrs -> True) -> do
      mEnemy <- selectOne matcher
      for_ mEnemy $ \enemy -> do
        investigators <- selectList $ investigatorAt $ locationOf attrs
        lead <- getLead
        push $ chooseOneAtATime
          lead
          [ targetLabel
              investigator
              [EnemyAttack $ enemyAttack enemy investigator]
          | investigator <- investigators
          ]
      pure a
    Flip _ _ (isTarget a -> True) -> do
      let
        lid = locationOf attrs
        savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      createSavioCorvi <- createEnemyAt_ savioCorvi lid Nothing
      pushAll [createSavioCorvi, Flipped (toSource attrs) savioCorvi]
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      lead <- getLead
      pushAll
        [ FocusCards [savioCorvi]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_20 <$> runMessage msg attrs
