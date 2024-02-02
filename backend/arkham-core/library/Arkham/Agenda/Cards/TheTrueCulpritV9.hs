module Arkham.Agenda.Cards.TheTrueCulpritV9 (
  TheTrueCulpritV9 (..),
  theTrueCulpritV9,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards

newtype TheTrueCulpritV9 = TheTrueCulpritV9 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theTrueCulpritV9 :: AgendaCard TheTrueCulpritV9
theTrueCulpritV9 = agenda (3, A) TheTrueCulpritV9 Cards.theTrueCulpritV9 (Static 6)

instance HasAbilities TheTrueCulpritV9 where
  getAbilities (TheTrueCulpritV9 attrs) =
    guard (onSide A attrs)
      *> [ restrictedAbility
            (proxy (locationIs Cards.room212) attrs)
            1
            (exists (assetIs Cards.tomeOfRituals <> AssetAt (locationIs Cards.room212)))
            actionAbility
         , restrictedAbility
            attrs
            2
            ( exists
                $ treacheryIs Cards.harvestedBrain
                <> TreacheryWithHorror (AtLeast $ PerPlayer 3)
            )
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage TheTrueCulpritV9 where
  runMessage msg a@(TheTrueCulpritV9 attrs) =
    case msg of
      UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
        tomeOfRituals <- selectJust $ assetIs Cards.tomeOfRituals
        n <- perPlayer 1
        clues <- fieldMap AssetClues (min n) tomeOfRituals
        player <- getPlayer iid
        pushWhen (clues > 0)
          $ chooseAmounts
            player
            "Choose amount of clues to move"
            (MaxAmountTarget n)
            [("Clues", (0, min n clues))]
            (toTarget attrs)

        pure a
      ResolveAmounts _ (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) -> do
        tomeOfRituals <- selectJust $ assetIs Cards.tomeOfRituals
        harvestedBrain <- selectJust $ treacheryIs Cards.harvestedBrain
        push $ MovedClues (toSource tomeOfRituals) (toTarget harvestedBrain) n
        pure a
      UseThisAbility _ (isSource attrs -> True) 2 -> do
        push $ AdvanceAgendaBy (toId attrs) AgendaAdvancedWithOther
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithDoom | aid == toId attrs && onSide B attrs -> do
        push R2
        pure a
      AdvanceAgendaBy aid AgendaAdvancedWithOther | aid == toId attrs && onSide B attrs -> do
        push R1
        pure a
      _ -> TheTrueCulpritV9 <$> runMessage msg attrs
