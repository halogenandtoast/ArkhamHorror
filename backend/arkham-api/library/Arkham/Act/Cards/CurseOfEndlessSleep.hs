module Arkham.Act.Cards.CurseOfEndlessSleep (curseOfEndlessSleep, curseOfEndlessSleepEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Campaigns.TheForgottenAge.Helpers (setExplorationDeck)
import Arkham.Card
import Arkham.Classes.HasQueue (pushEnd)
import Arkham.Effect.Import
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Trait (Trait (Brotherhood, Cairo, Expedition))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype CurseOfEndlessSleep = CurseOfEndlessSleep ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfEndlessSleep :: ActCard CurseOfEndlessSleep
curseOfEndlessSleep = act (1, A) CurseOfEndlessSleep Cards.curseOfEndlessSleep Nothing

instance HasAbilities CurseOfEndlessSleep where
  getAbilities (CurseOfEndlessSleep a) =
    [ mkAbility a 1 $ forced $ AddedToVictory #after Nothing (CardWithTrait Brotherhood)
    , restricted
        a
        2
        (InVictoryDisplay (mapOneOf cardIs evidenceCards) (EqualTo $ Static 6))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage CurseOfEndlessSleep where
  runMessage msg a@(CurseOfEndlessSleep attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ [c | (windowType -> Window.AddedToVictory _ c) <- ws] \card -> do
        let evidence = flipCard card
        lift $ pushEnd $ ReplaceCard card.id evidence
        let code = toCardCode evidence
        readNow <- maybe (pure True) remembered (evidenceTask code)
        if readNow
          then readEvidence code
          else createCardEffect Cards.curseOfEndlessSleep Nothing attrs (CardCodeTarget code)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      expedition <- getSetAsideCardsMatching $ CardWithTrait Expedition
      treacheries <-
        traverse
          fetchCard
          [ Treacheries.terrorUnderThePyramids
          , Treacheries.swarmOfLocusts
          , Treacheries.abyssalReach
          , Treacheries.eclipse
          , Treacheries.sandstorm
          ]
      setExplorationDeck =<< shuffle (expedition <> treacheries)
      shuffleEncounterDiscardBackIn
      eachInvestigator (removeAllClues attrs)
      selectEach (LocationWithTrait Cairo) (removeAllClues attrs)
      advanceActDeck attrs
      pure a
    _ -> CurseOfEndlessSleep <$> liftRunMessage msg attrs

newtype CurseOfEndlessSleepEffect = CurseOfEndlessSleepEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curseOfEndlessSleepEffect :: EffectArgs -> CurseOfEndlessSleepEffect
curseOfEndlessSleepEffect = cardEffect CurseOfEndlessSleepEffect Cards.curseOfEndlessSleep

instance HasAbilities CurseOfEndlessSleepEffect where
  getAbilities (CurseOfEndlessSleepEffect attrs) = case attrs.target of
    CardCodeTarget code -> case evidenceTask code of
      Just task -> [mkAbility attrs 1 $ SilentForcedAbility $ RememberedLogKey #after task]
      Nothing -> []
    _ -> []

instance RunMessage CurseOfEndlessSleepEffect where
  runMessage msg e@(CurseOfEndlessSleepEffect attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      case attrs.target of
        CardCodeTarget code -> readEvidence code
        _ -> pure ()
      disable attrs
      pure e
    _ -> CurseOfEndlessSleepEffect <$> liftRunMessage msg attrs
