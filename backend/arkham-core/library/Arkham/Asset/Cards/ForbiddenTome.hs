module Arkham.Asset.Cards.ForbiddenTome (
  forbiddenTome,
  ForbiddenTome (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype ForbiddenTome = ForbiddenTome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenTome :: AssetCard ForbiddenTome
forbiddenTome = asset ForbiddenTome Cards.forbiddenTome

instance HasAbilities ForbiddenTome where
  getAbilities (ForbiddenTome a) =
    [ controlledAbility a 1 CanDrawCards $ actionAbilityWithCost $ exhaust a <> assetUseCost a Secret 1
    ]

instance RunMessage ForbiddenTome where
  runMessage msg a@(ForbiddenTome attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payments -> do
      drawing <- drawCards iid (toAbilitySource attrs 1) 1
      pushAll
        [ drawing
        , UseCardAbilityChoice iid (toSource attrs) 1 NoAbilityMetadata windows' payments
        ]
      pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 _ _ _ -> do
      n <- fieldMap InvestigatorHand length iid
      noUses <- fieldMap AssetUses ((== 0) . findWithDefault 0 Secret) (toId attrs)
      player <- getPlayer iid
      pushWhen (n >= 10 && noUses)
        $ chooseOne player
        $ [ Label "Discard Forbidden Tome"
              $ [toDiscardBy iid (toAbilitySource attrs 1) attrs, Record YouHaveTranslatedTheTome]
          , Label "Do not discard Forbidden Tome" []
          ]
      pure a
    _ -> ForbiddenTome <$> runMessage msg attrs
