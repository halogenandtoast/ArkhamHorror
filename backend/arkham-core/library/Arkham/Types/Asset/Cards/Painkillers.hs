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
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype Painkillers = Painkillers AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painkillers :: AssetCard Painkillers
painkillers = asset Painkillers Cards.painkillers

instance HasActions Painkillers where
  getActions (Painkillers a) =
    [ restrictedAbility
        a
        1
        (OwnsThis <> InvestigatorExists (You <> InvestigatorWithAnyDamage))
        (FastAbility
          (Costs
            [ UseCost (toId a) Supply 1
            , ExhaustThis
            , HorrorCost (toSource a) YouTarget 1
            ]
          )
        )
    ]


instance HasModifiersFor env Painkillers

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Painkillers where
  runMessage msg a@(Painkillers attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (HealDamage (InvestigatorTarget iid) 1)
    _ -> Painkillers <$> runMessage msg attrs
