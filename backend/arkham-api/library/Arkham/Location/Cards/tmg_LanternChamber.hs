module Arkham.Location.Cards.TMGLanternChamber (tmgLanternChamber) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype TMGLanternChamber = TMGLanternChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Lantern Chamber' from The Midwinter Gala (#71008).
tmgLanternChamber :: LocationCard TMGLanternChamber
tmgLanternChamber =
  victory 1
    $ location
        TMGLanternChamber
        Cards.tmgLanternChamber
        5
        (PerPlayer 2)
        NoSymbol
        []
        & revealedBy False

instance HasAbilities TMGLanternChamber where
  getAbilities (TMGLanternChamber attrs) =
    withBaseAbilities attrs
      [ limitedAbility (GroupLimit PerTurn 1)
          $ restrictedAbility attrs 1 Here
          $ FastAbility Free
      , restrictedAbility attrs 2 Here
          $ ActionAbility Nothing
      ]

instance RunMessage TMGLanternChamber where
  runMessage msg l@(TMGLanternChamber attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      -- TODO: Implement guessing ability
      pure l
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      -- TODO: Implement spellbound flipping ability
      pure l
    _ -> TMGLanternChamber <$> runMessage msg attrs
