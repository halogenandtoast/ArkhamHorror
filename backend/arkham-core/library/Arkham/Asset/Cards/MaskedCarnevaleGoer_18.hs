module Arkham.Asset.Cards.MaskedCarnevaleGoer_18
  ( maskedCarnevaleGoer_18
  , MaskedCarnevaleGoer_18(..)
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

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

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
locationOf AssetAttrs { assetPlacement } = case assetPlacement of
  AtLocation lid -> lid
  _ -> error "impossible"

instance RunMessage MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
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
        elisabettaMagro = EncounterCard
          $ lookupEncounterCard Enemies.elisabettaMagro (toCardId attrs)
      a <$ pushAll
        [ CreateEnemyAt elisabettaMagro lid Nothing
        , Flipped (toSource attrs) elisabettaMagro
        ]
    LookAtRevealed iid source target | isTarget a target -> do
      let
        elisabettaMagro = EncounterCard
          $ lookupEncounterCard Enemies.elisabettaMagro (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [elisabettaMagro]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        , Flip iid source target
        ]
    _ -> MaskedCarnevaleGoer_18 <$> runMessage msg attrs
