module Arkham.Types.Asset.Cards.ScrollOfProphecies
  ( ScrollOfProphecies(..)
  , scrollOfProphecies
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (UseType(Secret))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype ScrollOfProphecies = ScrollOfProphecies AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfProphecies :: AssetCard ScrollOfProphecies
scrollOfProphecies = hand ScrollOfProphecies Cards.scrollOfProphecies

instance HasModifiersFor env ScrollOfProphecies

instance HasActions ScrollOfProphecies where
  getActions (ScrollOfProphecies x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId x) Secret 1]
        )
    ]

instance AssetRunner env => RunMessage env ScrollOfProphecies where
  runMessage msg (ScrollOfProphecies attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [DrawCards iid' 3 False, ChooseAndDiscardCard iid']
          | iid' <- investigatorIds
          ]
        )
      pure
        $ ScrollOfProphecies
        $ attrs
        & exhaustedL
        .~ True
        & usesL
        %~ Resource.use
    _ -> ScrollOfProphecies <$> runMessage msg attrs
