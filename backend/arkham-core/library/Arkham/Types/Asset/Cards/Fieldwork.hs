module Arkham.Types.Asset.Cards.Fieldwork
  ( fieldwork
  , Fieldwork(..)
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
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Fieldwork = Fieldwork AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldwork :: AssetCard Fieldwork
fieldwork = asset Fieldwork Cards.fieldwork

instance HasAbilities Fieldwork where
  getAbilities (Fieldwork attrs) =
    [ restrictedAbility
        attrs
        1
        OwnsThis
        (ReactionAbility
            (Enters Timing.After You
            $ LocationWithClues (GreaterThan $ Static 0)
            )
        $ ExhaustCost
        $ toTarget attrs
        )
    ]

instance AssetRunner env => RunMessage env Fieldwork where
  runMessage msg a@(Fieldwork attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a
        <$ push
             (CreateEffect (toCardCode attrs) Nothing source
             $ InvestigatorTarget iid
             )
    _ -> Fieldwork <$> runMessage msg attrs
