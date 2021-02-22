module Arkham.Types.Location.Cards.DestroyedPath
  ( destroyedPath
  , DestroyedPath(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DestroyedPath = DestroyedPath LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destroyedPath :: DestroyedPath
destroyedPath =
  DestroyedPath
    $ base
    & (revealedSymbolL .~ Squiggle)
    & (revealedConnectedSymbolsL .~ setFromList [Triangle, Equals])
 where
  base = baseAttrs
    "02287"
    (Name "Destroyed Path" Nothing)
    EncounterSet.WhereDoomAwaits
    3
    (Static 0)
    NoSymbol
    []
    [Dunwich, Woods, Altered]

instance HasModifiersFor env DestroyedPath where
  getModifiersFor = noModifiersFor

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 ForcedAbility

investigateAbility :: LocationAttrs -> Ability
investigateAbility a = mkAbility
  (toSource a)
  2
  (ActionAbility (Just Action.Investigate) (ActionCost 1))

instance ActionRunner env => HasActions env DestroyedPath where
  getActions iid NonFast (DestroyedPath attrs) =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction iid (investigateAbility attrs)
      | iid `on` attrs
      ]
  getActions iid (AfterRevealLocation You) (DestroyedPath attrs)
    | iid `on` attrs = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure
        [ ActivateCardAbilityAction iid (forcedAbility attrs)
        | actionRemainingCount == 0
        ]
  getActions iid window (DestroyedPath attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env DestroyedPath where
  runMessage msg l@(DestroyedPath attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      amount <- getPlayerCountValue (PerPlayer 1)
      l <$ unshiftMessage (PlaceDoom (toTarget attrs) amount)
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ unshiftMessage
        (Investigate
          iid
          (toId attrs)
          (AbilitySource source 2)
          SkillIntellect
          False
        )
    SuccessfulInvestigation _ _ (AbilitySource source 2)
      | isSource attrs source -> do
        l <$ unshiftMessage (RemoveDoom (toTarget attrs) 1)
    _ -> DestroyedPath <$> runMessage msg attrs
