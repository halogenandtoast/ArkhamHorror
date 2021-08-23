module Arkham.Types.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window hiding (SuccessfulInvestigation)

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs =
  location AlchemyLabs Cards.alchemyLabs 5 (Static 0) Squiggle [Hourglass]

instance HasModifiersFor env AlchemyLabs where
  getModifiersFor _ target (AlchemyLabs attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env AlchemyLabs where
  getAbilities iid window@(Window Timing.When NonFast) (AlchemyLabs attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseAbilities iid window attrs $ do
      let
        ability = mkAbility attrs 1
          $ ActionAbility (Just Action.Investigate) (ActionCost 1)
      pure [locationAbility ability]
  getAbilities iid window (AlchemyLabs attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (Investigate iid (locationId attrs) source SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source -> do
      maid <- selectOne (assetIs Cards.alchemicalConcoction)
      l <$ case maid of
        Just aid -> push (TakeControlOfAsset iid aid)
        Nothing -> pure ()
    _ -> AlchemyLabs <$> runMessage msg attrs
