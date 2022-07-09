module Arkham.Asset.Cards.MaskedCarnevaleGoer_17
  ( maskedCarnevaleGoer_17
  , MaskedCarnevaleGoer_17(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement
import Arkham.Source

newtype MaskedCarnevaleGoer_17 = MaskedCarnevaleGoer_17 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

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
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        lid = locationOf attrs
        enemyId = EnemyId $ toCardId attrs
      investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
      a <$ pushAll
        (Flip iid (InvestigatorSource iid) (toTarget attrs)
        : [ EnemyAttack iid' enemyId DamageAny RegularAttack | iid' <- investigatorIds ]
        )
    Flip _ _ target | isTarget attrs target -> do
      let
        lid = locationOf attrs
        donLagorio = EncounterCard
          $ lookupEncounterCard Enemies.donLagorio (toCardId attrs)
      a <$ pushAll
        [ CreateEnemyAt donLagorio lid Nothing
        , Flipped (toSource attrs) donLagorio
        ]
    LookAtRevealed _ _ target | isTarget a target -> do
      let
        donLagorio = EncounterCard
          $ lookupEncounterCard Enemies.donLagorio (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [donLagorio]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_17 <$> runMessage msg attrs
