module Arkham.Agenda.Cards.TheFeastOfHemlockVale.HemlockHouse.LivingWalls (livingWalls) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.TheFeastOfHemlockVale.HemlockHouse qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Scenarios.HemlockHouse.Helpers (flipLocationOver, scenarioI18n)
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (..))

newtype LivingWalls = LivingWalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingWalls :: AgendaCard LivingWalls
livingWalls = agenda (3, A) LivingWalls Cards.livingWalls (Static 8)

instance HasAbilities LivingWalls where
  getAbilities (LivingWalls a) =
    [ mkAbility a 1 $ forced $ PhaseEnds #when #mythos
    , restricted a 2 (exists YourLocation)
        $ freeTrigger
        $ OrCost
          [ PlaceClueOnLocationCost (PerPlayer 1)
          , scenarioI18n
              $ LabeledCost (ikey' "cost.removeSeal")
              $ SpendTokenCost Resource (LocationTargetMatches YourLocation)
          ]
    ]

instance RunMessage LivingWalls where
  runMessage msg a@(LivingWalls attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      thePredatoryHouse <- selectJust $ storyIs Stories.thePredatoryHouse
      sendMessage' thePredatoryHouse $ requestChaosTokens lead (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withLocationOf iid \lid -> do
        readyThis lid
        flipLocationOver lid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        defeat attrs iid
      pure a
    _ -> LivingWalls <$> liftRunMessage msg attrs
