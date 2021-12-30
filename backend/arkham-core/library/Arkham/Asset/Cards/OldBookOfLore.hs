module Arkham.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Target

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = asset OldBookOfLore Cards.oldBookOfLore

instance HasAbilities OldBookOfLore where
  getAbilities (OldBookOfLore a) =
    [ restrictedAbility
          a
          1
          (OwnsThis <> InvestigatorExists
            (InvestigatorAt YourLocation
            <> InvestigatorWithoutModifier CannotManipulateDeck
            )
          )
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ExhaustCost $ toTarget a]
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
