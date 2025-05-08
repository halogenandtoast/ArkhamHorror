module Arkham.Agenda.Cards.TheTrueCulpritV9 (theTrueCulpritV9) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards

newtype TheTrueCulpritV9 = TheTrueCulpritV9 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV9 :: AgendaCard TheTrueCulpritV9
theTrueCulpritV9 = agenda (3, A) TheTrueCulpritV9 Cards.theTrueCulpritV9 (Static 6)

instance HasAbilities TheTrueCulpritV9 where
  getAbilities (TheTrueCulpritV9 attrs) =
    guard (onSide A attrs)
      *> [ skillTestAbility
             $ withTooltip
               "{action} If Tome of Rituals is at this location: Test {willpower} (5) or {intellect} (5). If you succeed, move up to 1 {perPlayer} clues from Tome of Rituals to Harvested Brain."
             $ restricted
               (proxied (locationIs Cards.room212) attrs)
               1
               (exists (assetIs Cards.tomeOfRituals <> AssetAt (locationIs Cards.room212)))
               actionAbility
         , restricted
             attrs
             2
             (exists $ treacheryIs Cards.harvestedBrain <> TreacheryWithClues (AtLeast $ PerPlayer 3))
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage TheTrueCulpritV9 where
  runMessage msg a@(TheTrueCulpritV9 attrs) = runQueueT $ case msg of
    UseThisAbility iid source@(ProxySource _ (isSource attrs -> True)) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind -> do
          skillLabeled kind $ beginSkillTest sid iid source iid kind (Fixed 5)
      pure a
    PassedThisSkillTest iid (isProxySource attrs -> True) -> do
      tomeOfRituals <- selectJust $ assetIs Cards.tomeOfRituals
      n <- perPlayer 1
      clues <- fieldMap AssetClues (min n) tomeOfRituals
      when (clues > 0) do
        chooseAmounts
          iid
          "Choose amount of clues to move"
          (MaxAmountTarget n)
          [("Clues", (0, min n clues))]
          (toTarget attrs)
      pure a
    ResolveAmounts _ (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) -> do
      tomeOfRituals <- selectJust $ assetIs Cards.tomeOfRituals
      harvestedBrain <- selectJust $ treacheryIs Cards.harvestedBrain
      moveTokens (attrs.ability 1) tomeOfRituals harvestedBrain #clue n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ AdvanceAgendaBy attrs.id #other
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) AgendaAdvancedWithDoom -> do
      push R2
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) AgendaAdvancedWithOther -> do
      push R1
      pure a
    _ -> TheTrueCulpritV9 <$> liftRunMessage msg attrs
