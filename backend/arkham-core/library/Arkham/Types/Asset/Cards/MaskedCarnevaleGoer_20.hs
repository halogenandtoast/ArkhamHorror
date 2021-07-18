module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_20
  ( maskedCarnevaleGoer_20
  , MaskedCarnevaleGoer_20(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.EncounterCard
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Window

newtype MaskedCarnevaleGoer_20 = MaskedCarnevaleGoer_20 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedCarnevaleGoer_20 :: AssetCard MaskedCarnevaleGoer_20
maskedCarnevaleGoer_20 =
  asset MaskedCarnevaleGoer_20 Cards.maskedCarnevaleGoer_20

ability :: AssetAttrs -> Ability
ability attrs =
  (mkAbility attrs 1 (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]))
    { abilityRestrictions = OnLocation <$> assetLocation attrs
    }

instance HasActions env MaskedCarnevaleGoer_20 where
  getActions iid NonFast (MaskedCarnevaleGoer_20 attrs) =
    pure [UseAbility iid (ability attrs)]
  getActions iid window (MaskedCarnevaleGoer_20 attrs) =
    getActions iid window attrs

instance HasModifiersFor env MaskedCarnevaleGoer_20

instance
  ( HasSet InvestigatorId env LocationId
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env MaskedCarnevaleGoer_20 where
  runMessage msg a@(MaskedCarnevaleGoer_20 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      case assetLocation attrs of
        Just lid -> do
          investigatorIds <- getSetList lid
          let
            savioCorviId = EnemyId $ toCardId attrs
            savioCorvi = EncounterCard
              $ lookupEncounterCard Enemies.savioCorvi (toCardId attrs)
          a <$ pushAll
            ([ RemoveFromGame (toTarget attrs)
             , CreateEnemyAt savioCorvi lid Nothing
             ]
            <> [ EnemyAttack iid savioCorviId | iid <- investigatorIds ]
            )
        Nothing -> error "not possible"
    _ -> MaskedCarnevaleGoer_20 <$> runMessage msg attrs
