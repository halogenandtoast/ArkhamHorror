module Arkham.Asset.Cards.MaskedCarnevaleGoer_19
  ( maskedCarnevaleGoer_19
  , MaskedCarnevaleGoer_19(..)
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

newtype MaskedCarnevaleGoer_19 = MaskedCarnevaleGoer_19 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

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
        salvatoreNeri = EncounterCard
          $ lookupEncounterCard Enemies.salvatoreNeri (toCardId attrs)
      a <$ pushAll
        [ CreateEnemyAt salvatoreNeri lid Nothing
        , Flipped (toSource attrs) salvatoreNeri
        ]
    LookAtRevealed _ _ target | isTarget a target -> do
      let
        salvatoreNeri = EncounterCard
          $ lookupEncounterCard Enemies.salvatoreNeri (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [salvatoreNeri]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_19 <$> runMessage msg attrs
