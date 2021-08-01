module Arkham.Types.Asset.Cards.Painkillers
  ( painkillers
  , Painkillers(..)
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
import Arkham.Types.PlayRestriction
import Arkham.Types.Target

newtype Painkillers = Painkillers AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painkillers :: AssetCard Painkillers
painkillers =
  assetWith Painkillers Cards.painkillers (startingUsesL ?~ Uses Supply 3)

fastAbility :: InvestigatorId -> AssetAttrs -> Ability
fastAbility iid attrs = restrictedAbility
  (toSource attrs)
  1
  (InvestigatorExists $ You <> InvestigatorWithDamage)
  (FastAbility
    (Costs
      [ UseCost (toId attrs) Ammo 1
      , ExhaustCost (toTarget attrs)
      , HorrorCost (toSource attrs) (InvestigatorTarget iid) 1
      ]
    )
  )

instance HasActions env Painkillers where
  getActions iid _ (Painkillers a) | ownedBy a iid =
    pure [UseAbility iid (fastAbility iid a)]
  getActions _ _ _ = pure []

instance HasModifiersFor env Painkillers

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Painkillers where
  runMessage msg a@(Painkillers attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealDamage (InvestigatorTarget iid) 1)
    _ -> Painkillers <$> runMessage msg attrs
