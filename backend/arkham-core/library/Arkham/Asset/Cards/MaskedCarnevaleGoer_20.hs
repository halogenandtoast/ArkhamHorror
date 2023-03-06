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
import Arkham.Card.EncounterCard
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement
import Arkham.Source

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 =
  asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

instance HasAbilities MaskedCarnevaleGoer_20 where
  getAbilities (MaskedCarnevaleGoer_20 x) =
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

instance RunMessage MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        lid = locationOf attrs
        enemyId = EnemyId $ toCardId attrs
      investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
      pushAll
        $ Flip iid (InvestigatorSource iid) (toTarget attrs)
        : map (EnemyAttack . enemyAttack enemyId) investigatorIds
      pure a
    Flip _ _ target | isTarget attrs target -> do
      let
        lid = locationOf attrs
        savioCorvi = EncounterCard
          $ lookupEncounterCard Enemies.savioCorvi (toCardId attrs)
      a <$ pushAll
        [ CreateEnemyAt savioCorvi lid Nothing
        , Flipped (toSource attrs) savioCorvi
        ]
    LookAtRevealed _ _ target | isTarget a target -> do
      let
        savioCorvi = EncounterCard
          $ lookupEncounterCard Enemies.savioCorvi (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [savioCorvi]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_20 <$> runMessage msg attrs
