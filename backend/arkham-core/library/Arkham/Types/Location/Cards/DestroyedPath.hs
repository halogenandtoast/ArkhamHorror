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
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.SkillType
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing

newtype DestroyedPath = DestroyedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
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

instance HasAbilities env DestroyedPath where
  getAbilities iid window (DestroyedPath attrs) =
    withBaseAbilities iid window attrs $ pure $ if locationRevealed attrs
      then
        [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
        , restrictedAbility attrs 2 Here
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        ]
      else []

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
        Nothing
        SkillIntellect
        False
      )
    SuccessfulInvestigation _ _ (AbilitySource source 2) _
      | isSource attrs source -> l <$ push (RemoveDoom (toTarget attrs) 1)
    _ -> DestroyedPath <$> runMessage msg attrs
