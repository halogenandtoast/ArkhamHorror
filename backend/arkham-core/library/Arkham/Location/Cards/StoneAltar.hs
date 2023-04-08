module Arkham.Location.Cards.StoneAltar
  ( stoneAltar
  , StoneAltar(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Projection
import Arkham.RequestedTokenStrategy
import Arkham.Token

newtype StoneAltar = StoneAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneAltar :: LocationCard StoneAltar
stoneAltar = location StoneAltar Cards.stoneAltar 3 (PerPlayer 1)

instance HasAbilities StoneAltar where
  getAbilities (StoneAltar attrs) = withBaseAbilities attrs []

instance RunMessage StoneAltar where
  runMessage msg l@(StoneAltar attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ RequestTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure l
    RequestedTokens (isSource attrs -> True) (Just iid) tokens -> do
      push $ ResetTokens (toSource attrs)
      when
          (any
            ((`elem` [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail])
            . tokenFace
            )
            tokens
          )
        $ do
            actionsRemaining <- field InvestigatorRemainingActions iid
            push $ if actionsRemaining > 0
              then LoseActions iid (toSource attrs) 1
              else InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      pure l
    _ -> StoneAltar <$> runMessage msg attrs
