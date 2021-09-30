module Arkham.Types.Asset.Cards.ArcaneInitiate
  ( arcaneInitiate
  , ArcaneInitiate(..)
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
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetCard ArcaneInitiate
arcaneInitiate = ally ArcaneInitiate Cards.arcaneInitiate (1, 2)

instance HasAbilities ArcaneInitiate where
  getAbilities (ArcaneInitiate a) =
    [ restrictedAbility a 1 OwnsThis
      $ ForcedAbility
      $ AssetEntersPlay Timing.When
      $ AssetWithId
      $ toId a
    , restrictedAbility a 2 OwnsThis $ FastAbility $ ExhaustCost $ toTarget a
    ]

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (PlaceDoom (toTarget attrs) 1)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Search iid source (InvestigatorTarget iid) [fromTopOfDeck 3] [Spell]
            $ DrawFound iid 1
        ]
      pure a
    _ -> ArcaneInitiate <$> runMessage msg attrs
