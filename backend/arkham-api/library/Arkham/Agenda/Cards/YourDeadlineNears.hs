module Arkham.Agenda.Cards.YourDeadlineNears (yourDeadlineNears) where

import Arkham.Ability
import Arkham.Helpers.Window.Enemy
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ByTheBook.Helpers
import Arkham.Trait (Trait (Cultist))

newtype YourDeadlineNears = YourDeadlineNears AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourDeadlineNears :: AgendaCard YourDeadlineNears
yourDeadlineNears = agenda (2, A) YourDeadlineNears Cards.yourDeadlineNears (Static 6)

instance HasAbilities YourDeadlineNears where
  getAbilities (YourDeadlineNears a) =
    [ mkAbility a 1 $ forced $ EnemyWouldBeDefeated #when (NonWeaknessEnemy <> EnemyWithTrait Cultist)
    , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny rolandBanks
    , mkAbility a 3 $ ActionAbility #resign Nothing (ActionCost 1)
    ]

instance RunMessage YourDeadlineNears where
  runMessage msg a@(YourDeadlineNears attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      healCultistInsteadOfDefeat (attrs.ability 1) eid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      resign iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n $ scope "timeHasRunOut" $ flavor $ h "title" >> p "body"
      cultists <- select victoryDisplayCultists
      let (greys, rest) = partition ((== Enemies.mrGrey) . toCardDef) cultists
      case greys of
        (grey : _) -> obtainCard grey
        [] -> when (notNull rest) do
          lead <- getLead
          focusCards rest do
            chooseOneM lead do
              for_ rest \card -> cardLabeled card do
                unfocusCards
                obtainCard card
      push R1
      pure a
    _ -> YourDeadlineNears <$> liftRunMessage msg attrs
