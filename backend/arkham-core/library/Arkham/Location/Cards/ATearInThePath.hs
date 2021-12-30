module Arkham.Location.Cards.ATearInThePath
  ( aTearInThePath
  , ATearInThePath(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (aTearInThePath)
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Timing qualified as Timing

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
  . (revealedConnectedMatchersL .~ map LocationWithSymbol [Square, Squiggle])
  )

instance HasAbilities ATearInThePath where
  getAbilities (ATearInThePath attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
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
