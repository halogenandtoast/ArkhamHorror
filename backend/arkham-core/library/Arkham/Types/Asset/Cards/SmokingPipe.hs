module Arkham.Types.Asset.Cards.SmokingPipe
  ( smokingPipe
  , SmokingPipe(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype SmokingPipe = SmokingPipe AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smokingPipe :: AssetCard SmokingPipe
smokingPipe = asset SmokingPipe Cards.smokingPipe

fastAbility :: InvestigatorId -> AssetAttrs -> Ability
fastAbility iid attrs = mkAbility
  (toSource attrs)
  1
  (FastAbility
    (Costs
      [ UseCost (toId attrs) Ammo 1
      , ExhaustCost (toTarget attrs)
      , DamageCost (toSource attrs) (InvestigatorTarget iid) 1
      ]
    )
  )

instance HasActions env SmokingPipe where
  getActions iid _ (SmokingPipe a) | ownedBy a iid =
    pure [UseAbility iid (fastAbility iid a)]
  getActions _ _ _ = pure []

instance HasModifiersFor env SmokingPipe where
  getModifiersFor = noModifiersFor

instance (HasSet InvestigatorId env (), HasQueue env, HasModifiersFor env ()) => RunMessage env SmokingPipe where
  runMessage msg a@(SmokingPipe attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      SmokingPipe <$> runMessage msg (attrs & usesL .~ Uses Supply 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealHorror (InvestigatorTarget iid) 1)
    _ -> SmokingPipe <$> runMessage msg attrs
