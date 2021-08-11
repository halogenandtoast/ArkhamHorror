module Arkham.Types.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = hand OldBookOfLore Cards.oldBookOfLore

instance HasModifiersFor env OldBookOfLore

instance HasActions OldBookOfLore where
  getActions (OldBookOfLore a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustCost $ toTarget a]
    ]

instance AssetRunner env => RunMessage env OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      a <$ push
        (chooseOne
          iid
          [ SearchTopOfDeck
              iid'
              source
              (InvestigatorTarget iid')
              3
              []
              (ShuffleBackIn $ DrawFound iid')
          | iid' <- investigatorIds
          ]
        )
    _ -> OldBookOfLore <$> runMessage msg attrs
