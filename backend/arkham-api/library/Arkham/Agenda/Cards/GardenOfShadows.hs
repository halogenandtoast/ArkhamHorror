module Arkham.Agenda.Cards.GardenOfShadows (gardenOfShadows) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.I18n
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey

newtype GardenOfShadows = GardenOfShadows AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gardenOfShadows :: AgendaCard GardenOfShadows
gardenOfShadows = agenda (3, A) GardenOfShadows Cards.gardenOfShadows (Static 5)

instance HasAbilities GardenOfShadows where
  getAbilities (GardenOfShadows a) =
    [ restricted a 1 (HasScenarioCount StrengthOfTheAbyss $ atMost 1)
        $ forced
        $ oneOf
          [ ScenarioCountDecremented #after StrengthOfTheAbyss
          , AgendaEntersPlay #after (AgendaWithId a.id)
          ]
    ]

instance RunMessage GardenOfShadows where
  runMessage msg a@(GardenOfShadows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getStrengthOfTheAbyss
      addStrengthOfTheAbyss (2 - n)
      eachInvestigator \iid -> do
        withI18n $ chooseOneM iid do
          countVar 1 $ labeled' "drawCards" $ drawCards iid (attrs.ability 1) 1
          countVar 1 $ labeled' "healHorror" $ healHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      addStrengthOfTheAbyss 1
      selectEach (not_ ResignedInvestigator <> UneliminatedInvestigator) \iid -> do
        investigatorTakenByTheAbyss iid
        push $ InvestigatorDefeated (toSource attrs) iid
      pure a
    _ -> GardenOfShadows <$> liftRunMessage msg attrs
