module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_17
  ( maskedCarnevaleGoer_17
  , MaskedCarnevaleGoer_17(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.Source

newtype MaskedCarnevaleGoer_17 = MaskedCarnevaleGoer_17 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

maskedCarnevaleGoer_17 :: AssetCard MaskedCarnevaleGoer_17
maskedCarnevaleGoer_17 =
  asset MaskedCarnevaleGoer_17 Cards.maskedCarnevaleGoer_17

instance HasActions MaskedCarnevaleGoer_17 where
  getActions (MaskedCarnevaleGoer_17 attrs) =
    [ restrictedAbility
        attrs
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]

instance HasModifiersFor env MaskedCarnevaleGoer_17

locationOf :: AssetAttrs -> LocationId
locationOf AssetAttrs { assetLocation } = case assetLocation of
  Just lid -> lid
  Nothing -> error "impossible"

instance
  ( HasSet InvestigatorId env LocationId
  , HasQueue env
  , HasModifiersFor env ()
  , HasId LeadInvestigatorId env ()
  )
  => RunMessage env MaskedCarnevaleGoer_17 where
  runMessage msg a@(MaskedCarnevaleGoer_17 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      let
        lid = locationOf attrs
        enemyId = EnemyId $ toCardId attrs
      investigatorIds <- getSetList lid
      a <$ pushAll
        (Flip (InvestigatorSource iid) (toTarget attrs)
        : [ EnemyAttack iid' enemyId DamageAny | iid' <- investigatorIds ]
        )
    Flip _ target | isTarget attrs target -> do
      let
        lid = locationOf attrs
        donLagorio = EncounterCard
          $ lookupEncounterCard Enemies.donLagorio (toCardId attrs)
      a
        <$ pushAll
             [ RemoveFromGame (toTarget attrs)
             , CreateEnemyAt donLagorio lid Nothing
             ]
    LookAtRevealed _ target | isTarget a target -> do
      let
        donLagorio = EncounterCard
          $ lookupEncounterCard Enemies.donLagorio (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [donLagorio]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_17 <$> runMessage msg attrs
