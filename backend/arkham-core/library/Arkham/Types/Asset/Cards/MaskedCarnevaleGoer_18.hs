module Arkham.Types.Asset.Cards.MaskedCarnevaleGoer_18
  ( maskedCarnevaleGoer_18
  , MaskedCarnevaleGoer_18(..)
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

newtype MaskedCarnevaleGoer_18 = MaskedCarnevaleGoer_18 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskedCarnevaleGoer_18 :: AssetCard MaskedCarnevaleGoer_18
maskedCarnevaleGoer_18 =
  asset MaskedCarnevaleGoer_18 Cards.maskedCarnevaleGoer_18

ability :: AssetAttrs -> Ability
ability attrs =
  (mkAbility attrs 1 (ActionAbility Nothing $ Costs [ActionCost 1, ClueCost 1]))
    { abilityRestrictions = OnLocation <$> assetLocation attrs
    }

instance HasActions env MaskedCarnevaleGoer_18 where
  getActions iid NonFast (MaskedCarnevaleGoer_18 attrs) =
    pure [UseAbility iid (ability attrs)]
  getActions iid window (MaskedCarnevaleGoer_18 attrs) =
    getActions iid window attrs

instance HasModifiersFor env MaskedCarnevaleGoer_18

instance
  ( HasSet InvestigatorId env LocationId
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env MaskedCarnevaleGoer_18 where
  runMessage msg a@(MaskedCarnevaleGoer_18 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      case assetLocation attrs of
        Just lid -> do
          investigatorIds <- getSetList lid
          let
            elisabettaMagroId = EnemyId $ toCardId attrs
            elisabettaMagro = EncounterCard
              $ lookupEncounterCard Enemies.elisabettaMagro (toCardId attrs)
          a <$ pushAll
            ([ RemoveFromGame (toTarget attrs)
             , CreateEnemyAt elisabettaMagro lid Nothing
             ]
            <> [ EnemyAttack iid elisabettaMagroId | iid <- investigatorIds ]
            )
        Nothing -> error "not possible"
    _ -> MaskedCarnevaleGoer_18 <$> runMessage msg attrs
