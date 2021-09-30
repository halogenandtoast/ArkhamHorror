module Arkham.Types.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = hand OldBookOfLore Cards.oldBookOfLore

instance HasAbilities OldBookOfLore where
  getAbilities (OldBookOfLore a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustCost $ toTarget a]
    ]

instance AssetRunner env => RunMessage env OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId iid
      investigatorIds <- selectList $ InvestigatorAt $ LocationWithId locationId
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [ Search
                  iid'
                  source
                  (InvestigatorTarget iid')
                  [fromTopOfDeck 3]
                  AnyCard
                  (DrawFound iid' 1)
              ]
          | iid' <- investigatorIds
          ]
        )
    _ -> OldBookOfLore <$> runMessage msg attrs
