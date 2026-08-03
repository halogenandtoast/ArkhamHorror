module Arkham.Act.Cards.BanishHim (banishHim) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Act.Sequence
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCardsMatching)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Trait (Trait (Ally, Cthulhu))

newtype BanishHim = BanishHim ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

banishHim :: ActCard BanishHim
banishHim = act (1, A) BanishHim Cards.banishHim Nothing

instance HasAbilities BanishHim where
  getAbilities = actAbilities \a ->
    [ playerLimit PerTest
        $ restricted a 1 attackAtYourLocation
        $ FastAbility (ClueCost $ Static 1)
    , restricted a 2 (InVictoryDisplay (CardWithTrait Cthulhu) (atLeast 3))
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage BanishHim where
  runMessage msg a@(BanishHim attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      returnCthulhuFacetsToBoard
      increaseCthulhuRage 1
      rage <- (+ 1) <$> getCthulhuRage

      if rage <= 3
        then push $ RevertAct attrs.id
        else do
          playerCount <- getPlayerCount
          setCthulhuRage (if playerCount <= 2 then 4 else 5)
          locations <- select $ RevealedLocation <> not_ LocationWithVictory
          for_ locations $ placeCluesUpToClueValue (attrs.ability 2)
          eachInvestigator (`forInvestigator` msg)
          agenda <- getSetAsideCard Agendas.theFinalSeal
          push $ SetCurrentAgendaDeck 1 [agenda]
          toDiscard attrs attrs
          placeDoomOnAgenda =<< getPlayerCount
      pure a
    DoStep 2 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      for_ cthulhuFacets \(front, _enraged) ->
        selectEach (enemyIs front) (flipOver lead)

      pure a
    ForInvestigator iid (AdvanceAct aid _ _) | aid == attrs.id -> do
      allies <- getSetAsideCardsMatching (#asset <> CardWithTrait Ally)
      unless (null allies) do
        chooseOneM iid $ scenarioI18n do
          questionLabeled' "chooseAlly"
          labeled' "noAlly" nothing
          for_ allies \ally -> cardLabeled ally $ createAssetAt_ ally (InPlayArea iid)
      pure a
    RevertAct aid | aid == attrs.id && onSide B attrs -> do
      pure $ BanishHim $ attrs & sequenceL .~ Sequence 1 A
    _ -> BanishHim <$> liftRunMessage msg attrs
