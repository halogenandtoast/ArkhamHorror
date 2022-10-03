module Arkham.Location.Cards.AlchemyLabs
  ( alchemyLabs
  , AlchemyLabs(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source

newtype AlchemyLabs = AlchemyLabs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemyLabs :: LocationCard AlchemyLabs
alchemyLabs = location AlchemyLabs Cards.alchemyLabs 5 (Static 0)

instance HasModifiersFor AlchemyLabs where
  getModifiersFor target (AlchemyLabs attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

instance HasAbilities AlchemyLabs where
  getAbilities (AlchemyLabs attrs) = withBaseAbilities
    attrs
    [ withTooltip
        "{action}: _Investigate_. If you are successful, instead of discovering clues, take the Alchemical Concoction from underneath this location if able."
      $ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Investigate) (ActionCost 1)
    | locationRevealed attrs
    ]

instance RunMessage AlchemyLabs where
  runMessage msg l@(AlchemyLabs attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 1)
        Nothing
        SkillIntellect
        False
      )
    Successful (Action.Investigate, _) iid (AbilitySource source 1) _ _
      | isSource attrs source -> do
        maid <- selectOne (assetIs Cards.alchemicalConcoction)
        for_ maid (push . TakeControlOfAsset iid)
        pure l
    _ -> AlchemyLabs <$> runMessage msg attrs
