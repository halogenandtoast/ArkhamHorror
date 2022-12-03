module Arkham.Asset.Cards.ForbiddenTome
  ( forbiddenTome
  , ForbiddenTome(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection

newtype ForbiddenTome = ForbiddenTome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTome :: AssetCard ForbiddenTome
forbiddenTome = asset ForbiddenTome Cards.forbiddenTome

instance HasAbilities ForbiddenTome where
  getAbilities (ForbiddenTome a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> Negate (SelfHasModifier CannotDrawCards))
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage ForbiddenTome where
  runMessage msg a@(ForbiddenTome attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payments -> do
      pushAll
        [ drawCards iid attrs 1
        , UseCardAbilityChoice
          iid
          (toSource attrs)
          1
          NoAbilityMetadata
          windows'
          payments
        ]
      pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 _ _ _ -> do
      n <- fieldMap InvestigatorHand length iid
      noUses <- fieldMap AssetUses ((== 0) . useCount) (toId attrs)
      when (n >= 10 && noUses) $ do
        push $ chooseOne
          iid
          [ Label
            "Discard Forbidden Tome"
            [Discard (toTarget attrs), Record YouHaveTranslatedTheTome]
          , Label "Do not discard Forbidden Tome" []
          ]
      pure a
    _ -> ForbiddenTome <$> runMessage msg attrs
