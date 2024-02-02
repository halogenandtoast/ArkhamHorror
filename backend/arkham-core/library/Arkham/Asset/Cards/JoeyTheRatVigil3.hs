module Arkham.Asset.Cards.JoeyTheRatVigil3 (
  joeyTheRatVigil3,
  JoeyTheRatVigil3 (..),
) where

import Arkham.Prelude

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn, FastPlayerWindow)
import Arkham.Projection
import Arkham.Trait
import Arkham.Window

newtype JoeyTheRatVigil3 = JoeyTheRatVigil3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

joeyTheRatVigil3 :: AssetCard JoeyTheRatVigil3
joeyTheRatVigil3 = ally JoeyTheRatVigil3 Cards.joeyTheRatVigil3 (3, 2)

-- TODO: This does not account for the 1 resource spent in the cost
instance HasAbilities JoeyTheRatVigil3 where
  getAbilities (JoeyTheRatVigil3 x) =
    [ controlledAbility
        x
        1
        (PlayableCardExists UnpaidCost (InHandOf You <> #item))
        (FastAbility $ ResourceCost 1)
    , restrictedAbility x 2 (ControlsThis <> can.gain.resources You)
        $ FastAbility
        $ DiscardAssetCost #item
    ]

instance RunMessage JoeyTheRatVigil3 where
  runMessage msg a@(JoeyTheRatVigil3 attrs) = case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      handCards <- field InvestigatorHand iid
      let items = filter (member Item . toTraits) handCards
      let windows'' = nub $ windows' <> [mkWhen (DuringTurn iid), mkWhen FastPlayerWindow]
      playableItems <- filterM (getIsPlayable iid source UnpaidCost windows'') items
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel (toCardId item) [PayCardCost iid item windows'']
          | item <- playableItems
          ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ takeResources iid (toAbilitySource attrs 2) 2
      pure a
    _ -> JoeyTheRatVigil3 <$> runMessage msg attrs
