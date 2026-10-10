module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.SuddenSymptoms (suddenSymptoms) where

import Arkham.Ability
import Arkham.ChaosBagStepState
import Arkham.Helpers.Window (getDrawSource)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype SuddenSymptoms = SuddenSymptoms TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suddenSymptoms :: TreacheryCard SuddenSymptoms
suddenSymptoms = treachery SuddenSymptoms Cards.suddenSymptoms

instance HasAbilities SuddenSymptoms where
  getAbilities (SuddenSymptoms a) =
    -- "When you would reveal a chaos token during a skill test you are
    -- performing: ..." and "At the end of the round, discard Sudden Symptoms."
    -- No printed limit, so each reveal in a test is replaced, not just the first.
    [ restricted a 1 (InThreatAreaOf You <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ forced (WouldRevealChaosToken #when You)
    , restricted a 2 (InThreatAreaOf You) $ forced $ RoundEnds #when
    ]

instance RunMessage SuddenSymptoms where
  runMessage msg t@(SuddenSymptoms attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatAreaOnlyOne attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (attrs.ability 1) 1 ResolveChoice [Undecided (DrawUntil IsSymbol)] [] Nothing
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> SuddenSymptoms <$> liftRunMessage msg attrs
