module Arkham.Types.Location.Cards.ATearInThePath
  ( aTearInThePath
  , ATearInThePath(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (aTearInThePath)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Query
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype ATearInThePath = ATearInThePath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInThePath :: LocationCard ATearInThePath
aTearInThePath = locationWith
  ATearInThePath
  Cards.aTearInThePath
  3
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ Equals)
  . (revealedConnectedSymbolsL .~ setFromList [Square, Squiggle])
  )

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 LegacyForcedAbility

instance ActionRunner env => HasAbilities env ATearInThePath where
  getAbilities iid (Window Timing.After (RevealLocation who _)) (ATearInThePath attrs)
    | iid == who
    = do
      actionRemainingCount <- unActionRemainingCount <$> getCount iid
      pure [ locationAbility (forcedAbility attrs) | actionRemainingCount == 0 ]
  getAbilities iid window (ATearInThePath attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> ATearInThePath <$> runMessage msg attrs
