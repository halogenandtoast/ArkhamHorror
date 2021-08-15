module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_18
  ( maskedCarnevaleGoer_18
  , MaskedCarnevaleGoer_18(..)
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
import Arkham.Types.Source
import Arkham.Types.Window

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, TargetEntity)

maskedCarnevaleGoer_18 :: AssetCard MaskedCarnevaleGoer_18
maskedCarnevaleGoer_18 =
  asset MaskedCarnevaleGoer_18 Cards.maskedCarnevaleGoer_18

ability :: AssetAttrs -> Ability
ability attrs =
  (mkAbility attrs 1 (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]))
    { abilityCriteria = OnLocation <$> assetLocation attrs
    }

instance HasAbilities env MaskedCarnevaleGoer_18 where
  getAbilities _ NonFast (MaskedCarnevaleGoer_18 attrs) = pure [ability attrs]
  getAbilities iid window (MaskedCarnevaleGoer_18 attrs) =
    getAbilities iid window attrs

instance HasModifiersFor env MaskedCarnevaleGoer_18

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
  => RunMessage env MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = case msg of
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
        elisabettaMagro = EncounterCard
          $ lookupEncounterCard Enemies.elisabettaMagro (toCardId attrs)
      a <$ pushAll
        [ RemoveFromGame (toTarget attrs)
        , CreateEnemyAt elisabettaMagro lid Nothing
        ]
    LookAtRevealed iid target | isTarget a target -> do
      let
        elisabettaMagro = EncounterCard
          $ lookupEncounterCard Enemies.elisabettaMagro (toCardId attrs)
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ FocusCards [elisabettaMagro]
        , chooseOne leadInvestigatorId [Label "Continue" [UnfocusCards]]
        , Flip iid target
        ]
    _ -> MaskedCarnevaleGoer_18 <$> runMessage msg attrs
