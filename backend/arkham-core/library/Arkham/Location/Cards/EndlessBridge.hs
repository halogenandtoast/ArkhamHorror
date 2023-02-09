module Arkham.Location.Cards.EndlessBridge
  ( endlessBridge
  , EndlessBridge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Label ( mkLabel )
import Arkham.Location.Cards qualified as Cards ( endlessBridge )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Timing qualified as Timing
import Control.Monad.Extra ( findM )

newtype EndlessBridge = EndlessBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessBridge :: LocationCard EndlessBridge
endlessBridge = location EndlessBridge Cards.endlessBridge 4 (Static 2)

instance HasAbilities EndlessBridge where
  getAbilities (EndlessBridge attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.After Anyone
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage EndlessBridge where
  runMessage msg l@(EndlessBridge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ LoseResources iid 2
      let
        labels = [ nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2] ]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . EndlessBridge $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ chooseOne
        iid
        [ Label "Place 1 doom on Endless Bridge" [PlaceDoom (toTarget attrs) 1]
        , Label "Discard Endless Bridge" [Discard (toAbilitySource attrs 1) (toTarget attrs)]
        ]
      pure l
    _ -> EndlessBridge <$> runMessage msg attrs
