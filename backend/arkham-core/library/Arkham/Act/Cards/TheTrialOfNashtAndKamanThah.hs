module Arkham.Act.Cards.TheTrialOfNashtAndKamanThah (
  TheTrialOfNashtAndKamanThah (..),
  theTrialOfNashtAndKamanThah,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TheTrialOfNashtAndKamanThah = TheTrialOfNashtAndKamanThah ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrialOfNashtAndKamanThah :: ActCard TheTrialOfNashtAndKamanThah
theTrialOfNashtAndKamanThah = act (2, A) TheTrialOfNashtAndKamanThah Cards.theTrialOfNashtAndKamanThah Nothing

instance HasAbilities TheTrialOfNashtAndKamanThah where
  getAbilities (TheTrialOfNashtAndKamanThah attrs) | onSide A attrs = do
    [ restrictedAbility
        attrs
        1
        (Negate $ exists $ oneOf [enemyIs Enemies.nasht, enemyIs Enemies.kamanThah])
        $ Objective
        $ ForcedAbility AnyWindow
      ]
  getAbilities _ = []

instance RunMessage TheTrialOfNashtAndKamanThah where
  runMessage msg a@(TheTrialOfNashtAndKamanThah attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ advanceVia #other attrs attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators
      placeSevenHundredSteps <- placeSetAsideLocation_ Locations.sevenHundredSteps
      placeBaseOfTheSteps <- placeSetAsideLocation_ Locations.baseOfTheSteps
      placeTheEnchantedPath <- placeSetAsideLocation_ Locations.theEnchantedPath
      pushAll
        $ map (RemoveAllClues (toSource attrs) . toTarget) investigators
        <> [ placeSevenHundredSteps
           , placeBaseOfTheSteps
           , placeTheEnchantedPath
           , AddChaosToken Skull
           , advanceActDeck attrs
           ]
      pure a
    _ -> TheTrialOfNashtAndKamanThah <$> runMessage msg attrs
