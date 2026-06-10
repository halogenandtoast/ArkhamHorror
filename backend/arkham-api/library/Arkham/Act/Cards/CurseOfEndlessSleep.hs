module Arkham.Act.Cards.CurseOfEndlessSleep (curseOfEndlessSleep) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Campaigns.TheForgottenAge.Helpers (setExplorationDeck)
import Arkham.Card
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Message.Lifted.Story
import Arkham.Placement
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
    [mkAbility a 1 $ forced $ AddedToVictory #after Nothing (CardWithTrait Brotherhood)]

instance RunMessage CurseOfEndlessSleep where
  runMessage msg a@(CurseOfEndlessSleep attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      lead <- getLead
      for_ [c | (windowType -> Window.AddedToVictory _ c) <- ws] \card ->
        for_ (evidenceFor card.cardCode) \evidence -> do
          evidenceCard <- genCard evidence
          resolveStoryWithPlacement lead evidenceCard Global
      evidenceCount <-
        selectCount $ VictoryDisplayCardMatch $ basic $ mapOneOf cardIs brotherhoodEnemies
      when (evidenceCount >= 6) $ advancedWithOther attrs
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
      eachInvestigator \iid -> removeAllClues attrs iid
      selectEach (LocationWithTrait Cairo) \loc -> removeAllClues attrs loc
      advanceActDeck attrs
      pure a
    _ -> CurseOfEndlessSleep <$> liftRunMessage msg attrs
