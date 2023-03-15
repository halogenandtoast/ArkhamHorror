module Arkham.Asset.Cards.MaskedCarnevaleGoer_19
  ( maskedCarnevaleGoer_19
  , MaskedCarnevaleGoer_19(..)
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

newtype MaskedCarnevaleGoer_19 = MaskedCarnevaleGoer_19 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_19 :: AssetCard MaskedCarnevaleGoer_19
maskedCarnevaleGoer_19 =
  asset MaskedCarnevaleGoer_19 Cards.maskedCarnevaleGoer_19

instance HasAbilities MaskedCarnevaleGoer_19 where
  getAbilities (MaskedCarnevaleGoer_19 x) =
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

instance RunMessage MaskedCarnevaleGoer_19 where
  runMessage msg a@(MaskedCarnevaleGoer_19 attrs) = case msg of
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
        salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      createSalvatoreNeri <- createEnemyAt_ salvatoreNeri lid Nothing
      pushAll [createSalvatoreNeri, Flipped (toSource attrs) salvatoreNeri]
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      lead <- getLead
      pushAll
        [ FocusCards [salvatoreNeri]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_19 <$> runMessage msg attrs
