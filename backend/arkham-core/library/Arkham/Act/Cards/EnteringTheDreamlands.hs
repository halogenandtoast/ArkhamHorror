module Arkham.Act.Cards.EnteringTheDreamlands (
  EnteringTheDreamlands (..),
  enteringTheDreamlands,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype EnteringTheDreamlands = EnteringTheDreamlands ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

enteringTheDreamlands :: ActCard EnteringTheDreamlands
enteringTheDreamlands = act (1, A) EnteringTheDreamlands Cards.enteringTheDreamlands Nothing

instance HasAbilities EnteringTheDreamlands where
  getAbilities (EnteringTheDreamlands attrs) | onSide A attrs = do
    [ mkAbility attrs 1
        $ Objective
        $ ForcedAbility
        $ Enters #after Anyone "The Cavern of Flame"
      ]
  getAbilities _ = []

instance RunMessage EnteringTheDreamlands where
  runMessage msg a@(EnteringTheDreamlands attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ advanceVia #other attrs attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      theCavernOfFlame <- getJustLocationByName "The Cavern of Flame"
      investigators <- getInvestigators
      nasht <- getSetAsideCard Enemies.nasht
      createNasht <- createEnemyAt_ nasht theCavernOfFlame Nothing
      kamanThah <- getSetAsideCard Enemies.kamanThah
      createKamanThah <- createEnemyAt_ kamanThah theCavernOfFlame Nothing
      pushAll
        $ map (RemoveAllClues (toSource attrs) . toTarget) investigators
        <> [createNasht, createKamanThah, AddChaosToken Skull, advanceActDeck attrs]
      pure a
    _ -> EnteringTheDreamlands <$> runMessage msg attrs
