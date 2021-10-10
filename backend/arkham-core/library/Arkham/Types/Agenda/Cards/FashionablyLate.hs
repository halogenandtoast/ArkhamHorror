module Arkham.Types.Agenda.Cards.FashionablyLate
  ( FashionablyLate
  , fashionablyLate
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Trait

newtype FashionablyLate = FashionablyLate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fashionablyLate :: AgendaCard FashionablyLate
fashionablyLate =
  agenda (1, A) FashionablyLate Cards.fashionablyLate (Static 3)

instance AgendaRunner env => RunMessage env FashionablyLate where
  runMessage msg a@(FashionablyLate attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      dianneDevine <- getSetAsideCard Cards.dianneDevine

      a <$ pushAll
        [ CreateEnemyAtLocationMatching dianneDevine
        $ LocationWithAsset
        $ AssetWithFewestClues
        $ AssetWithTrait Bystander
        , ShuffleEncounterDiscardBackIn
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
    _ -> FashionablyLate <$> runMessage msg attrs
