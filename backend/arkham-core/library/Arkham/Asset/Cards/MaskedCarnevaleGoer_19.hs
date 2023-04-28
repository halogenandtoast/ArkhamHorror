module Arkham.Asset.Cards.MaskedCarnevaleGoer_19 (
  maskedCarnevaleGoer_19,
  MaskedCarnevaleGoer_19 (..),
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
locationOf AssetAttrs {assetPlacement} = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_19 where
  runMessage msg a@(MaskedCarnevaleGoer_19 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ Flip iid (toAbilitySource attrs 1) (toTarget attrs)
      pure a
    Flip _ source (isTarget a -> True) -> do
      let
        lid = locationOf attrs
        salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      investigators <- selectList $ investigatorAt $ locationOf attrs
      lead <- getLead
      (enemyId, createSalvatoreNeri) <- createEnemyAt salvatoreNeri lid Nothing
      pushAll $
        [ createSalvatoreNeri
        , Flipped (toSource attrs) salvatoreNeri
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
    LookAtRevealed _ _ (isTarget a -> True) -> do
      let salvatoreNeri = lookupCard Enemies.salvatoreNeri (toCardId attrs)
      lead <- getLead
      pushAll
        [ FocusCards [salvatoreNeri]
        , chooseOne lead [Label "Continue" [UnfocusCards]]
        ]
      pure a
    _ -> MaskedCarnevaleGoer_19 <$> runMessage msg attrs
