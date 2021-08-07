module Arkham.Types.Asset.Cards.SmokingPipe
  ( smokingPipe
  , SmokingPipe(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe =
  assetWith SmokingPipe Cards.smokingPipe (startingUsesL ?~ Uses Supply 3)

fastAbility :: InvestigatorId -> AssetAttrs -> Ability
fastAbility iid attrs = restrictedAbility
  (toSource attrs)
  1
  (InvestigatorExists $ You <> InvestigatorWithHorror)
  (FastAbility
    (Costs
      [ UseCost (toId attrs) Supply 1
      , ExhaustCost (toTarget attrs)
      , DamageCost (toSource attrs) (InvestigatorTarget iid) 1
      ]
    )
  )

instance HasActions env SmokingPipe where
  getActions iid FastPlayerWindow (SmokingPipe a) | ownedBy a iid =
    pure [fastAbility iid a]
  getActions _ _ _ = pure []

instance HasModifiersFor env SmokingPipe

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealHorror (InvestigatorTarget iid) 1)
    _ -> SmokingPipe <$> runMessage msg attrs
