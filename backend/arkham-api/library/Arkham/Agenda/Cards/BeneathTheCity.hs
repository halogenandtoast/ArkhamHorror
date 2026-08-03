module Arkham.Agenda.Cards.BeneathTheCity (beneathTheCity) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype BeneathTheCity = BeneathTheCity AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beneathTheCity :: AgendaCard BeneathTheCity
beneathTheCity =
  agendaWith (1, A) BeneathTheCity Cards.beneathTheCity (Static 0)
    $ (doomThresholdL .~ Nothing)
    . (removeDoomMatchersL .~ keepDoomInPlay)

{- | "...immediately advance (do not remove doom from cards in play)." The doom the
Sigil-Carved Alcoves load onto the Artifacts is exactly what weakens Cthulhu once
/Cthulhu Awakened/ takes over, so this advance must leave every card's doom alone.
-}
keepDoomInPlay :: RemoveDoomMatchers
keepDoomInPlay =
  RemoveDoomMatchers
    { removeDoomLocations = Nowhere
    , removeDoomInvestigators = NoOne
    , removeDoomEnemies = not_ AnyEnemy
    , removeDoomAssets = not_ AnyAsset
    , removeDoomActs = NotAct AnyAct
    , removeDoomAgendas = NotAgenda AnyAgenda
    , removeDoomTreacheries = not_ AnyTreachery
    , removeDoomEvents = not_ AnyEvent
    , removeDoomSkills = NotSkill AnySkill
    }

instance HasAbilities BeneathTheCity where
  getAbilities (BeneathTheCity a)
    | onSide A a =
        -- "[Forced] When doom would be placed on a card other than a story asset."
        -- Story assets are excluded so the Artifacts keep the doom put on them.
        [ forcedAbility a 1
            $ WouldPlaceDoomCounter #when AnySource (NotTarget $ AssetTargetMatches StoryAsset)
        ]
    | otherwise = []

instance RunMessage BeneathTheCity where
  runMessage msg a@(BeneathTheCity attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      -- A replacement effect: the doom is never placed, Disturbance rises instead.
      cancelWindowBatch ws
      increaseDisturbance
      -- The scenario applies that increase when the message above is processed, so
      -- the threshold is checked in a follow-up step that runs after it.
      doStep 1 msg
      pure a
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _ _) -> do
      -- "If the current Disturbance is 6 or more (8 or more instead if there are
      -- exactly 1 or 2 investigators in the game), immediately advance."
      disturbance <- getDisturbance
      playerCount <- getPlayerCount
      when (disturbance >= if playerCount <= 2 then 8 else 6) $ advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Cthulhu is the back of this card, so generate the enemy side directly
      -- rather than fetching (a fetch resolves to the in-play agenda side). He
      -- spawns at Dreamer's Rest via his own instruction.
      card <- genCard Enemies.cthulhuDeadAndDreaming
      createEnemy_ card ()
      advanceAgendaDeck attrs
      pure a
    _ -> BeneathTheCity <$> liftRunMessage msg attrs
