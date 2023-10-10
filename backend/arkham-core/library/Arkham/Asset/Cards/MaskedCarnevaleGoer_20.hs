module Arkham.Asset.Cards.MaskedCarnevaleGoer_20 (
  maskedCarnevaleGoer_20,
  MaskedCarnevaleGoer_20 (..),
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
        <> ClueCost (Static 1)
    ]

locationOf :: AssetAttrs -> LocationId
locationOf AssetAttrs {assetPlacement} = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ Flip iid (toAbilitySource attrs 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget a -> True) -> do
      let
        lid = locationOf attrs
        savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      investigators <- selectList $ investigatorAt $ locationOf attrs
      lead <- getLeadPlayer
      (enemyId, createSavioCorvi) <- createEnemyAt savioCorvi lid Nothing
      pushAll
        $ [ createSavioCorvi
          , Flipped (toSource attrs) savioCorvi
          ]
        <> [ chooseOrRunOneAtATime
            lead
            [ targetLabel
              investigator
              [EnemyAttack $ enemyAttack enemyId attrs investigator]
            | investigator <- investigators
            ]
           | isAbilitySource attrs 1 source
           , notNull investigators
           ]
      pure a
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let savioCorvi = lookupCard Enemies.savioCorvi (toCardId attrs)
      lead <- getLeadPlayer
      pushAll
        [ FocusCards [savioCorvi]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_20 <$> runMessage msg attrs
