module Arkham.Homebrew.DarkMatter.Agendas.SaturnAgendas (
  moonsOfSaturn,
  signsFromAldebaran,
  flightOfTheByakhees,
  againstTheSun,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom)
import Arkham.Homebrew.DarkMatter.Traits (pattern Brain)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.Phase

{- | The four "Strange Moons" agendas.

Agenda 1 ("Moons of Saturn"): "Do not draw cards from the encounter deck during
the mythos phase."

Agendas 2-4: "Forced - When a [[Brain]] story asset is defeated: Remove it from
the game and add 1 tally mark under 'Impending Doom' in your Campaign Log."
-}
newtype SaturnAgenda = SaturnAgenda AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkSaturn :: Int -> CardDef -> GameValue -> AgendaCard SaturnAgenda
mkSaturn n def = agenda (n, A) SaturnAgenda def

moonsOfSaturn :: AgendaCard SaturnAgenda
moonsOfSaturn = mkSaturn 1 Cards.moonsOfSaturn (Static 3)

signsFromAldebaran :: AgendaCard SaturnAgenda
signsFromAldebaran = mkSaturn 2 Cards.signsFromAldebaran (Static 8)

flightOfTheByakhees :: AgendaCard SaturnAgenda
flightOfTheByakhees = mkSaturn 3 Cards.flightOfTheByakhees (Static 5)

againstTheSun :: AgendaCard SaturnAgenda
againstTheSun = mkSaturn 4 Cards.againstTheSun (Static 3)

isMoonsOfSaturn :: AgendaAttrs -> Bool
isMoonsOfSaturn a = toCardCode (toCardDef a) == toCardCode Cards.moonsOfSaturn

instance HasModifiersFor SaturnAgenda where
  getModifiersFor (SaturnAgenda a) =
    when (isMoonsOfSaturn a)
      $ modified_ a (PhaseTarget #mythos) [SkipMythosPhaseStep EachInvestigatorDrawsEncounterCardStep]

instance HasAbilities SaturnAgenda where
  getAbilities (SaturnAgenda a)
    | isMoonsOfSaturn a = []
    | otherwise = [mkAbility a 1 $ forced $ Matcher.AssetDefeated #when ByAny (AssetWithTrait Brain)]

instance RunMessage SaturnAgenda where
  runMessage msg a@(SaturnAgenda attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      brains <- select $ AssetWithTrait Brain
      for_ brains \aid -> do
        push $ RemoveFromGame (AssetTarget aid)
        addImpendingDoom 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SaturnAgenda <$> liftRunMessage msg attrs
