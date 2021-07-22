module Arkham.Types.Location.Cards.DestroyedPath
  ( destroyedPath
  , DestroyedPath(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (destroyedPath)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Window

newtype DestroyedPath = DestroyedPath LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destroyedPath :: LocationCard DestroyedPath
destroyedPath = locationWith
  DestroyedPath
  Cards.destroyedPath
  3
  (Static 0)
  NoSymbol
  []
  ((revealedSymbolL .~ Squiggle)
  . (revealedConnectedSymbolsL .~ setFromList [Triangle, Equals])
  )

instance HasModifiersFor env DestroyedPath

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

investigateAbility :: LocationAttrs -> Ability
investigateAbility a = mkAbility
  (toSource a)
  2
  (ActionAbility (Just Action.Investigate) (ActionCost 1))

instance ActionRunner env => HasActions env DestroyedPath where
  getActions iid NonFast (DestroyedPath attrs) =
    withBaseActions iid NonFast attrs
      $ pure [locationAbility iid (investigateAbility attrs)]
  getActions iid (AfterRevealLocation You) (DestroyedPath attrs) = do
    actionRemainingCount <- unActionRemainingCount <$> getCount iid
    pure
      [ locationAbility iid (forcedAbility attrs) | actionRemainingCount == 0 ]
  getActions iid window (DestroyedPath attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DestroyedPath where
  runMessage msg l@(DestroyedPath attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      amount <- getPlayerCountValue (PerPlayer 1)
      l <$ push (PlaceDoom (toTarget attrs) amount)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> l <$ push
      (Investigate
        iid
        (toId attrs)
        (AbilitySource source 2)
        SkillIntellect
        False
      )
    SuccessfulInvestigation _ _ (AbilitySource source 2)
      | isSource attrs source -> l <$ push (RemoveDoom (toTarget attrs) 1)
    _ -> DestroyedPath <$> runMessage msg attrs
