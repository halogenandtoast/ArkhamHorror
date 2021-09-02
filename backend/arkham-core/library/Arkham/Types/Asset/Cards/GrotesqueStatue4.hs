module Arkham.Types.Asset.Cards.GrotesqueStatue4
  ( GrotesqueStatue4(..)
  , grotesqueStatue4
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.ChaosBagStepState
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype GrotesqueStatue4 = GrotesqueStatue4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

grotesqueStatue4 :: AssetCard GrotesqueStatue4
grotesqueStatue4 = hand GrotesqueStatue4 Cards.grotesqueStatue4

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
      -> do
        when (useCount (assetUses attrs) == 1) $ push (Discard (toTarget attrs))
        a <$ push
          (ReplaceCurrentDraw drawSource iid
          $ Choose 1 [Undecided Draw, Undecided Draw] []
          )
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
