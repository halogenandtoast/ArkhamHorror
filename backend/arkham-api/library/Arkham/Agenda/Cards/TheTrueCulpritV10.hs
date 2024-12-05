module Arkham.Agenda.Cards.TheTrueCulpritV10 (TheTrueCulpritV10 (..), theTrueCulpritV10) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist, Guest, Innocent, Lead))

newtype TheTrueCulpritV10 = TheTrueCulpritV10 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV10 :: AgendaCard TheTrueCulpritV10
theTrueCulpritV10 = agenda (3, A) TheTrueCulpritV10 Cards.theTrueCulpritV10 (Static 12)

instance HasModifiersFor TheTrueCulpritV10 where
  getModifiersFor (TheTrueCulpritV10 attrs) = do
    modifySelect attrs (EnemyWithTrait Guest) [LoseVictory, RemoveTrait Innocent, AddTrait Cultist]

instance HasAbilities TheTrueCulpritV10 where
  getAbilities (TheTrueCulpritV10 attrs) =
    guard (onSide A attrs)
      *> [ skillTestAbility $ restricted (proxied (locationIs Cards.basement) attrs) 1 Here actionAbility
         , restricted attrs 2 (exists $ AgendaWithDoom (EqualTo $ Static 0) <> AgendaWithId attrs.id)
            $ Objective
            $ forced AnyWindow
         ]

instance RunMessage TheTrueCulpritV10 where
  runMessage msg a@(TheTrueCulpritV10 attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      leadAssets <- select $ AssetWithTrait Lead <> assetControlledBy iid
      sid <- getRandom
      chooseOneM iid do
        labeled
          "remove 1 clue from a Lead asset in the Basement to reduce the difficulty of this test by 2"
          do
            chooseOrRunOneM iid do
              targets leadAssets \asset -> do
                push $ RemoveClues (AbilitySource p 1) (toTarget asset) 1
                skillTestModifier sid (AbilitySource p 1) sid (Difficulty (-2))
        labeled "Skip" nothing
      chooseOneM iid do
        for_ [#willpower, #agility] \skill -> do
          skillLabeled skill $ beginSkillTest sid iid (AbilitySource p 1) iid skill (Fixed 4)
      pure a
    PassedThisSkillTest _ p@(isProxyAbilitySource attrs 1 -> True) -> do
      removeDoom p attrs 1
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
    _ -> TheTrueCulpritV10 <$> liftRunMessage msg attrs
