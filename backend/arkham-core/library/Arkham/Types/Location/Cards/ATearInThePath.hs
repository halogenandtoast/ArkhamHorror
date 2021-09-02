module Arkham.Types.Location.Cards.ATearInThePath
  ( aTearInThePath
  , ATearInThePath(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (aTearInThePath)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities ATearInThePath where
  getAbilities (ATearInThePath attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithoutActionsRemaining)
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId attrs
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 2 0)
    _ -> ATearInThePath <$> runMessage msg attrs
