module Arkham.Agenda.Cards.FashionablyLate (
  FashionablyLate (..),
  fashionablyLate,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait

newtype FashionablyLate = FashionablyLate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fashionablyLate :: AgendaCard FashionablyLate
fashionablyLate =
  agenda (1, A) FashionablyLate Cards.fashionablyLate (Static 3)

instance RunMessage FashionablyLate where
  runMessage msg a@(FashionablyLate attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      dianneDevine <- getSetAsideCard Cards.dianneDevine
      createDianneDevine <-
        createEnemyAtLocationMatching_ dianneDevine
          $ LocationWithAsset
          $ AssetWithFewestClues
          $ AssetWithTrait Bystander

      pushAll
        [ createDianneDevine
        , ShuffleEncounterDiscardBackIn
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> FashionablyLate <$> runMessage msg attrs
