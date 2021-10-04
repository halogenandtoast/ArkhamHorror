module Arkham.Types.Asset.Cards.GrotesqueStatue4
  ( GrotesqueStatue4(..)
  , grotesqueStatue4
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.ChaosBagStepState
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype GrotesqueStatue4 = GrotesqueStatue4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

grotesqueStatue4 :: AssetCard GrotesqueStatue4
grotesqueStatue4 =
  handWith GrotesqueStatue4 Cards.grotesqueStatue4 (discardWhenNoUsesL .~ True)

instance HasAbilities GrotesqueStatue4 where
  getAbilities (GrotesqueStatue4 x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility (WouldRevealChaosToken Timing.When You)
        $ UseCost (toId x) Charge 1
        )
    ]

instance AssetRunner env => RunMessage env GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs) = case msg of
    UseCardAbility iid source [Window Timing.When (Window.WouldRevealChaosToken drawSource _)] 1 _
      | isSource attrs source
      -> a <$ push
        (ReplaceCurrentDraw drawSource iid
        $ Choose 1 [Undecided Draw, Undecided Draw] []
        )
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
