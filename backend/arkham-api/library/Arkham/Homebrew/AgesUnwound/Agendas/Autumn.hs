module Arkham.Homebrew.AgesUnwound.Agendas.Autumn (autumn) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (putTaskIntoPlayWithRevelation)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Spawn

newtype Autumn = Autumn AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

autumn :: AgendaCard Autumn
autumn = agenda (1, A) Autumn Cards.autumn (Static 5)

{- | "Each copy of The Myriad Gentleman loses hunter, and gains aloof and
\"__Spawn__ - Any empty location.\""

Matched by printed title rather than by def, which is what "each copy" means and
keeps holding if another printing of the Gentleman ever shares the table.
'OverwrittenSpawn' is the seam for a scenario rule replacing an enemy's printed
spawn; a 'ForceSpawn' from a drawing effect still takes precedence over it,
which is correct -- On the Hunt names its own location.
-}
instance HasModifiersFor Autumn where
  getModifiersFor (Autumn a) =
    modifySelect
      a
      (EnemyWithTitle "The Myriad Gentleman")
      [ RemoveKeyword Keyword.Hunter
      , AddKeyword Keyword.Aloof
      , OverwrittenSpawn (SpawnAt EmptyLocation)
      ]

instance RunMessage Autumn where
  runMessage msg a@(Autumn attrs) = runQueueT $ case msg of
    {- "__Opening Moves__ - Put the set-aside Keeper of Knowledge and Strange
    Portal treacheries into play next to the act deck, resolving their
    revelation effects. /(Note: Each copy of The Myriad Gentleman is no longer
    aloof.)/"

    The parenthetical is the front's modifier simply going away with the card:
    nothing has to undo it. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      putTaskIntoPlayWithRevelation lead Treacheries.keeperOfKnowledge
      putTaskIntoPlayWithRevelation lead Treacheries.strangePortal
      advanceAgendaDeck attrs
      pure a
    _ -> Autumn <$> liftRunMessage msg attrs
