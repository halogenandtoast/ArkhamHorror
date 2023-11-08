module Arkham.Agenda.Cards.EyesInTheDark (
  EyesInTheDark (..),
  eyesInTheDark,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher hiding (InvestigatorDefeated)

newtype EyesInTheDark = EyesInTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesInTheDark :: AgendaCard EyesInTheDark
eyesInTheDark =
  agenda (2, A) EyesInTheDark Cards.eyesInTheDark (StaticWithPerPlayer 12 1)

instance HasAbilities EyesInTheDark where
  getAbilities (EyesInTheDark a) =
    [ restrictedAbility
        a
        1
        (LocationExists $ YourLocation <> LocationWithoutClues)
        $ ActionAbility []
        $ ActionCost 1
    ]

instance RunMessage EyesInTheDark where
  runMessage msg a@(EyesInTheDark attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          (toSource attrs)
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll
        $ concatMap
          ( \iid ->
              [SufferTrauma iid 1 0, InvestigatorDefeated (toSource attrs) iid]
          )
          iids
      pure a
    _ -> EyesInTheDark <$> runMessage msg attrs
