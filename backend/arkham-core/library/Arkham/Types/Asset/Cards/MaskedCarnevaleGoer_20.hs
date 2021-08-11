module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_20
  ( maskedCarnevaleGoer_20
  , MaskedCarnevaleGoer_20(..)
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

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 =
  asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

instance HasActions MaskedCarnevaleGoer_20 where
  getActions (MaskedCarnevaleGoer_20 attrs) =
    [ restrictedAbility
        attrs
        1
        OnSameLocation
        (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1])
    ]

instance HasModifiersFor env MaskedCarnevaleGoer_20

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
  => RunMessage env MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = case msg of
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
        savioCorvi = EncounterCard
          $ lookupEncounterCard Enemies.savioCorvi (toCardId attrs)
      a
        <$ pushAll
             [ RemoveFromGame (toTarget attrs)
             , CreateEnemyAt savioCorvi lid Nothing
             ]
    LookAtRevealed _ target | isTarget a target -> do
      let
        savioCorvi = EncounterCard
          $ lookupEncounterCard Enemies.savioCorvi (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [savioCorvi]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        ]
    _ -> MaskedCarnevaleGoer_20 <$> runMessage msg attrs
