module Arkham.Asset.Cards.ArcaneInitiate3
  ( arcaneInitiate3
  , ArcaneInitiate3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ArcaneInitiate3 = ArcaneInitiate3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate3 :: AssetCard ArcaneInitiate3
arcaneInitiate3 = ally ArcaneInitiate3 Cards.arcaneInitiate3 (1, 3)

instance HasAbilities ArcaneInitiate3 where
  getAbilities (ArcaneInitiate3 a) =
    [ restrictedAbility a 1 ControlsThis
      $ ForcedAbility
      $ AssetEntersPlay Timing.When
      $ AssetWithId
      $ toId a
    , restrictedAbility a 2 ControlsThis $ FastAbility $ ExhaustCost $ toTarget
      a
    ]

instance RunMessage ArcaneInitiate3 where
  runMessage msg a@(ArcaneInitiate3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Label "Place 1 doom" [PlaceDoom (toTarget attrs) 1]
        , Label "Place 2 horror" [PlaceHorror (toTarget attrs) 2]
        ]
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ targetLabel
            iid
            [ Search
                  iid
                  source
                  (InvestigatorTarget iid)
                  [fromTopOfDeck 3]
                  (CardWithTrait Spell)
                $ DrawFound iid 1
            ]
        ]
      pure a
    _ -> ArcaneInitiate3 <$> runMessage msg attrs
