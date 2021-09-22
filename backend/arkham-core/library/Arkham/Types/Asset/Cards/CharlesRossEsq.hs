module Arkham.Types.Asset.Cards.CharlesRossEsq
  ( charlesRossEsq
  , CharlesRossEsq(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message

newtype CharlesRossEsq = CharlesRossEsq AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: AssetCard CharlesRossEsq
charlesRossEsq = ally CharlesRossEsq Cards.charlesRossEsq (1, 2)

instance HasAbilities CharlesRossEsq where
  getAbilities (CharlesRossEsq attrs) =
    [ restrictedAbility attrs 1 OwnsThis $ FastAbility $ ExhaustCost $ toTarget
        attrs
    ]

instance AssetRunner env => RunMessage env CharlesRossEsq where
  runMessage msg a@(CharlesRossEsq attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      -- TODO: we may want to track the investigator instead of the asset
      a <$ push
        (CreateEffect (toCardCode attrs) Nothing source (toTarget attrs))
    _ -> CharlesRossEsq <$> runMessage msg attrs
