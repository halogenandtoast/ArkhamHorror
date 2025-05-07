module Arkham.Location.Cards.StoneAltar (stoneAltar) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StoneAltar = StoneAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneAltar :: LocationCard StoneAltar
stoneAltar = location StoneAltar Cards.stoneAltar 3 (PerPlayer 1)

instance HasAbilities StoneAltar where
  getAbilities (StoneAltar a) = extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage StoneAltar where
  runMessage msg l@(StoneAltar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map (.face) -> tokens) -> do
      withI18n $ prompt_ iid "continue"
      let triggeringTokens = [ElderSign, Skull, Cultist, Tablet, ElderThing, AutoFail]
      when (any (`elem` triggeringTokens) tokens) do
        actionsRemaining <- iid.remainingActions
        if actionsRemaining > 0
          then loseActions iid (attrs.ability 1) 1
          else assignHorror iid (attrs.ability 1) 1
      resetChaosTokens attrs
      pure l
    _ -> StoneAltar <$> liftRunMessage msg attrs
