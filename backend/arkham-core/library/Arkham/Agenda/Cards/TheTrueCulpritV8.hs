module Arkham.Agenda.Cards.TheTrueCulpritV8 (
  TheTrueCulpritV8 (..),
  theTrueCulpritV8,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait (Trait (Staff))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheTrueCulpritV8 = TheTrueCulpritV8 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theTrueCulpritV8 :: AgendaCard TheTrueCulpritV8
theTrueCulpritV8 = agenda (3, A) TheTrueCulpritV8 Cards.theTrueCulpritV8 (Static 6)

instance HasAbilities TheTrueCulpritV8 where
  getAbilities (TheTrueCulpritV8 attrs) =
    guard (onSide A attrs)
      *> ( [ restrictedAbility
            (proxy (assetIs asset) attrs)
            1
            ControlsThis
            (actionAbilityWithCost $ AssetClueCost (toTitle asset) (assetIs asset) $ Static 1)
           | asset <-
              [ Cards.alienDevice
              , Cards.managersKey
              , Cards.sinisterSolution
              , Cards.timeWornLocket
              , Cards.tomeOfRituals
              ]
           ]
            <> [ mkAbility attrs 2 $ ReactionAbility (EnemyDefeated #after Anyone ByAny $ EnemyWithTrait Staff) Free
               , restrictedAbility
                  attrs
                  3
                  ( exists
                      $ treacheryIs Treacheries.harvestedBrain
                      <> TreacheryWithHorror (AtLeast $ StaticWithPerPlayer 2 1)
                  )
                  $ Objective
                  $ ForcedAbility AnyWindow
               ]
         )

instance RunMessage TheTrueCulpritV8 where
  runMessage msg a@(TheTrueCulpritV8 attrs) =
    case msg of
      UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
        push $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Staff
        pure a
      UseThisAbility _ (isSource attrs -> True) 2 -> do
        harvestedBrain <- selectJust $ treacheryIs Treacheries.harvestedBrain
        push $ PlaceHorror (toAbilitySource attrs 2) (toTarget harvestedBrain) 1
        pure a
      UseThisAbility _ (isSource attrs -> True) 3 -> do
        push $ AdvanceAgendaBy (toId attrs) AgendaAdvancedWithOther
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithDoom | aid == toId attrs && onSide B attrs -> do
        push R2
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithOther | aid == toId attrs && onSide B attrs -> do
        push R1
        pure a
      _ -> TheTrueCulpritV8 <$> runMessage msg attrs
